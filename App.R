
library(shiny)

# path <- 'D:/R projects/Kinetic-MUNANA-App/'
# setwd(path)

source('./MUNANA2.R')
source('./report.R')
source('./utilities.R')


######## SUPPLEMENTARY FUNCTIONS ########

init_result_table <- function(samples) {
    len <- length(samples)
    data.frame(name = samples,
               Vmax = numeric(len),
               Vmax_lower = numeric(len),
               Vmax_upper = numeric(len),
               Km = numeric(len),
               Km_lower = numeric(len),
               Km_upper = numeric(len))
}


edit_result_table <- function(df, name,
                              Vmax, Vmax_lower, Vmax_upper,
                              Km, Km_lower, Km_upper) {
    df$Vmax[df$name == name] <- Vmax
    df$Vmax_lower[df$name == name] <- Vmax_lower
    df$Vmax_upper[df$name == name] <- Vmax_upper
    df$Km[df$name == name] <- Km
    df$Km_lower[df$name == name] <- Km_lower
    df$Km_upper[df$name == name] <- Km_upper
    df
}


get_coef_ci <- function(model, par) {
    coefs <- coef(model)
    ci <- confint(model)
    switch(par,
           'Vmax' = signif(c(coefs[1], ci[1,1], ci[1,2]), 3),
           'Km' = signif(c(coefs[2], ci[2,1], ci[2,2]), 3),
           NULL)
}


######## SHINY APPLICATION ########


ui <- fluidPage(title = 'Kinetic MUNANA App',
    
    titlePanel(title = h1('Kinetic MUNANA App', align = 'center')),
    
    navbarPage('Steps:', id = 'main_tabs',
               
               tabPanel('Loading files',
                        
                        sidebarLayout(
                            sidebarPanel(
                                
                                h4('Standard Table'),
                                fluidRow(
                                    column(width = 5,
                                           numericInput('st_table_from', 'From:', value = 1, min = 1, max = 12)),
                                    column(width = 5,
                                           numericInput('st_table_to', 'To:', value = 2, min = 1, max = 12))
                                ),
                                downloadButton('save_st_table', 'Save Template'),
                                br(), br(),
                                fileInput('st_table_file', label = 'Choose file:', accept = c('.csv', '.xlsx')),
                                
                                h4('Sample Table'),
                                fluidRow(
                                    column(width = 5,
                                           numericInput('sample_table_from', 'From:', value = 3, min = 1, max = 12)),
                                    column(width = 5,
                                           numericInput('sample_table_to', 'To:', value = 3, min = 1, max = 12))),
                                downloadButton('save_sample_table', 'Save Template'),
                                br(), br(),
                                fileInput('sample_table_file', label = 'Choose file:', accept = c('.csv', '.xlsx')),
                                
                                h4('RFU Data'),
                                downloadButton('save_RFU_table', 'Save Template'), br(), br(),
                                selectInput('time_format', label = 'Time format',
                                            choices = c('Excel' = 'excel', 'Seconds' = 'sec',
                                                        'Minutes' = 'min', 'Hours' = 'hour'),
                                            selected = 'Excel'),
                                fileInput('RFU_table_file', label = 'Choose file:', accept = c('.csv', '.xlsx')),
                                
                                width = 2),
                            
                            
                            mainPanel(
                                
                                fluidRow(
                                    column(6,
                                           wellPanel(
                                               h3('Standard Table'),
                                               tableOutput('standard_table'))),
                                    column(6,
                                           wellPanel(
                                               h3('Sample Table'),
                                               tableOutput('sample_table')))
                                ),
                                wellPanel(
                                    h3('RFU Data'),
                                    tableOutput('RFU_table')
                                )
                            )
                        )
               ),
               
               tabPanel('Standard',
                        
                        sidebarLayout(
                            sidebarPanel(
                                selectInput('calib_method', 'Calibration Method:',
                                            choices = c('Log-linear' = 'log-linear',
                                                        'Linear' = 'linear')),
                                checkboxInput('bright_check', 'Brightness Correction', value = FALSE),
                                conditionalPanel(condition = 'input.bright_check == true',
                                                 numericInput('bright', 'Brightness Difference',
                                                              value = 250, min = 50, max = 1000)),
                                actionButton('calculate', 'Calculate'),
                                width = 2),
                            
                            mainPanel(
                                wellPanel(h3('Standard plot'),
                                          plotOutput('standard_plot', width =  "650px", height = "500px"), br(),
                                          selectInput('standard_plot_mode', label = 'Mode',
                                                      choices = c('Averaged' = 'averaged', 'Individual' = 'individual'),
                                                      selected = 'Averaged', width = '650px')
                                ),
                                wellPanel(h3('Parameters of calibration curves'),
                                          fluidRow(
                                              column(3,
                                                     plotOutput('plot_intercept', width = "300px", height = "270px"),
                                                     checkboxInput('int_edit_check', 'Change Y axis limits', value = FALSE),
                                                     conditionalPanel(condition = 'input.int_edit_check == true',
                                                                      fluidRow(
                                                                          column(4,
                                                                                 numericInput('int_y_min', 'Min',
                                                                                              value = -7, min = -15, max = 15, step = 0.1)),
                                                                          column(4,
                                                                                 numericInput('int_y_max', 'Max',
                                                                                              value = -5, min = -15, max = 15, step = 0.1)
                                                                          )
                                                                      )
                                                     )
                                              ),
                                              column(3, offset = 1,
                                                     plotOutput('plot_slope', width = "300px", height = "270px"),
                                                     checkboxInput('slope_edit_check', 'Change Y axis limits', value = FALSE),
                                                     conditionalPanel(condition = 'input.slope_edit_check == true',
                                                                      fluidRow(
                                                                          column(4,
                                                                                 numericInput('slope_y_min', 'Min',
                                                                                              value = 0.5, min = 0, max = 2, step = 0.1)),
                                                                          column(4,
                                                                                 numericInput('slope_y_max', 'Max',
                                                                                              value = 1.5, min = 1, max = 2, step = 0.1)
                                                                          )
                                                                      )
                                                     )
                                              ),
                                              column(3, offset = 1,
                                                     plotOutput('plot_Rsq', width = "300px", height = "270px"),
                                                     checkboxInput('rsq_edit_check', 'Change Y axis limits', value = FALSE),
                                                     conditionalPanel(condition = 'input.rsq_edit_check == true',
                                                                      fluidRow(
                                                                          column(4,
                                                                                 numericInput('rsq_y_min', 'Min',
                                                                                              value = 0.99, min = 0.5, max = 0.99, step = 0.01)),
                                                                          column(4,
                                                                                 numericInput('rsq_y_max', 'Max',
                                                                                              value = 1.01, min = 1.0, max = 1.2, step = 0.01)
                                                                          )
                                                                      )
                                                     )
                                              )
                                          )
                                )
                            )
                        )
                        
               ),
               
               tabPanel('Progress curves',
                        
                        sidebarLayout(
                            sidebarPanel(
                                selectInput('prog_curve_mode', label = 'Mode',
                                            choices = c('All in one' = 'all',
                                                        'By sample' = 'by sample',
                                                        'By sample & concentration' = 'by sample & conc'),
                                            selected = 'by sample'),
                                checkboxInput('add_velo', 'Show velocities', value = FALSE),
                                width = 2),
                            
                            mainPanel(
                                wellPanel(
                                    uiOutput('plot_progress_curves', width = "2000px", height = "900px")
                                )
                            )
                        )
               ),
               
               tabPanel('Michaelis-Menten Models',
                        
                        sidebarLayout(
                            sidebarPanel(
                                h4('Non-linear model parameters'),
                                uiOutput('sample_list'),
                                checkboxInput('manual_vmax_km', 'Manual guess of Vmax and Km', value = FALSE),
                                conditionalPanel(condition = 'input.manual_vmax_km == true',
                                                 fluidRow(
                                                   column(width = 5,
                                                          numericInput('Vmax', label = 'Vmax', value = 0.15, min = 0.01, max = 200, step = 0.01)
                                                   ),
                                                   column(width = 5,
                                                          numericInput('Km', label = 'Km', value = 35, min = 1, max = 2000, step = 1)
                                                   )
                                                 )
                                ),
                                width = 2),
                            
                            mainPanel(
                                wellPanel(
                                    h3('Velocity Data'),
                                    fluidRow(column(7,
                                                    tableOutput('velo_table_output')),
                                             column(4,
                                                    downloadButton('save_current_velocity_table', 'Save Current Data'),
                                                    br(), br(),
                                                    downloadButton('save_all_velocity_table', 'Save All Data')
                                                    ))
                                ),
                                wellPanel(
                                    h3('Michaelis-Menten Plot'),
                                    fluidRow(
                                        column(7,
                                               plotOutput('mm_plot', width = "600px", height = "500px"),
                                               checkboxInput('add_mm_line', 'Show model', value = TRUE)
                                        ),
                                        column(4,
                                               htmlOutput('nls_model_text'),
                                               br(),
                                               verbatimTextOutput('Vmax_out'),
                                               verbatimTextOutput('Km_out'),
                                               tags$head(tags$style(HTML('#nls_model_text
                                                                         {font-family: "Courier New"}'))
                                               ),
                                               tags$head(tags$style(HTML('#Vmax_out
                                                                         {font-size: 20px;
                                                                          font-family: "Courier New";
                                                                          color: red}'))
                                               ),
                                               tags$head(tags$style(HTML('#Km_out
                                                                         {font-size: 20px;
                                                                          font-family: "Courier New";
                                                                          color: red}'))
                                               ),
                                               h4('95% confidence intervals are shown in parentheses')
                                        )
                                    )
                                ),
                                wellPanel(
                                    h3('Result Table'),
                                    fluidRow(
                                        column(7,
                                               tableOutput('result_table')),
                                        column(4,
                                               downloadButton('save_mm_table', 'Save Data'))
                                    )
                                )
                            )
                        )
               ),
               
               tabPanel('Plots',
                        
                        sidebarLayout(
                            sidebarPanel(
                                h4('Samples'), br(),
                                uiOutput('sample_check_list'),
                            width = 2),
                            mainPanel(
                                wellPanel(
                                    h3('Michaelis-Menten Plots'), br(),
                                    fluidRow(
                                        column(9,
                                               plotOutput('mm_plots_layout', width = "850px", height = "750px")),
                                        column(2,
                                               tableOutput('result_table_selected'))
                                    )
                                ),
                                wellPanel(
                                    h3('Km vs Vmax Plot'), br(),
                                    plotOutput('km_vmax_plot', width = "850px", height = "750px"),
                                    checkboxInput('show_error_bars', 'Show 95% confidence intervals (error bars)', value = TRUE)
                                )
                            )
                        )
               ),
               
               tabPanel('Statistics',
                        
                        sidebarLayout(
                            sidebarPanel(
                                h4('Reference sample'),
                                uiOutput('ref_sample_select'),
                                h4('Adjust p-values for multiple comparisons'),
                                selectInput('p_adjust', label = 'Method',
                                            choices = c('No' = 'no',
                                                        'Holm' = 'holm',
                                                        'Hochberg' = 'hochberg',
                                                        'Hommel' = 'hommel',
                                                        'Bonferroni' = 'bonferroni',
                                                        'Benjamini & Hochberg' = "BH",
                                                        'Benjamini & Yekutieli' = "BY"),
                                            selected = 'bonferroni'),
                            width = 2),
                            mainPanel(
                                wellPanel(
                                    fluidRow(column(7,
                                                    h3('Vmax'),
                                                    tableOutput('vmax_stat')),
                                             column(4,
                                                    br(), br(),
                                                    downloadButton('save_vmax_data', 'Save Data')
                                             ))
                                ),
                                wellPanel(
                                    fluidRow(column(7,
                                                    h3('Km'),
                                                    tableOutput('km_stat')),
                                            column(4,
                                                   br(), br(),
                                                    downloadButton('save_km_data', 'Save Data')
                                                    ))

                                )
                            )
                        )
                ),
               
               tabPanel('Report',
                        
                        sidebarLayout(
                            sidebarPanel(
                                h4('Report components'), br(),
                                checkboxGroupInput('report_comps',
                                                   label = 'Select items to include:',
                                                   choices = c('Standard plot' = 'std_plot',
                                                               'Parameters of Calibration curves' = 'std_pars',
                                                               'Progress Curves' = 'progress_curves',
                                                               'Velocity Data' = 'velo_data',
                                                               'Michaelis-Menten Plots' = 'mm_plots',
                                                               'Km vs Vmax plot' = 'km_vmax_plot',
                                                               'Statistics' = 'stat'),
                                                   selected = c('std_plot',
                                                                'std_pars',
                                                                'progress_curves',
                                                                'velo_data',
                                                                'mm_plots',
                                                                'km_vmax_plot',
                                                                'stat')),
                                width = 2),
                            mainPanel(
                                h3('Report'),
                                dateInput('date', 'Date', width = '100px', weekstart = 1, format = 'dd.mm.yyyy'),
                                textInput('report_title', 'Title', width = '500px'),
                                downloadButton('save_report', 'Save Report')
                                )
                        )
                        ),
               
               navbarMenu('Utilities',
                          
                          tabPanel('Brightness Difference',
                                   
                                   sidebarLayout(
                                       sidebarPanel(h4('Save Template'),
                                                    downloadButton('save_bright_template', 'Download'), br(), br(),
                                                    h4('Load data'),
                                                    fileInput('br_table_file', label = 'Choose file:', accept = c('.csv', '.xlsx')),
                                                    width = 2),
                                       mainPanel(h3('Brightness Difference Between Substrate and Product'), br(),
                                                 fluidRow(column(7,
                                                                 plotOutput('bright_diff_plot', width = "700px", height = "550px")),
                                                          column(3,
                                                                 verbatimTextOutput('br_diff'),
                                                                 tags$head(tags$style(HTML('#br_diff
                                                                                           {font-size: 20px;
                                                                                            font-family: "Courier New";
                                                                                            color: red}')
                                                                                      )
                                                                           )
                                                                 )
                                                          )
                                                 )
                                   )
                          ),
                          
                          tabPanel('Spectrum Analysis',
                                   
                                   sidebarLayout(
                                       sidebarPanel(h4('Save Template'),
                                                    downloadButton('save_spectrum_template', 'Download'), br(), br(),
                                                    h4('Load data'),
                                                    fileInput('spectrum_table_file', label = 'Choose file:', accept = c('.csv', '.xlsx')),
                                                    checkboxInput('show_points', 'Show individual points', value = FALSE),
                                                    checkboxInput('log_scale', 'Log scale', value = FALSE),
                                                    checkboxInput('add_band', 'Add band', value = TRUE),
                                                    numericInput('bandwidth', 'Band width', value = 9, min = 1, max = 100, step = 1),
                                                    width = 2),
                                       mainPanel(h3('Spectrum Analysis'), br(),
                                                 fluidRow(column(7,
                                                                 plotOutput('spectrum_plot', width = "700px", height = "550px")
                                                                 ),
                                                          column(3,
                                                                 verbatimTextOutput('br_max'),
                                                                 tags$head(tags$style(HTML('#br_max
                                                                                           {font-size: 20px;
                                                                                            font-family: "Courier New";
                                                                                            color: red}')
                                                                                      )
                                                                           )
                                                                 )
                                                          )
                                                 
                                                 )
                                   )
                          )
               ),
               
               tabPanel('About',
                        
                        sidebarLayout(
                            sidebarPanel(tags$img(src='GU logo.jpg'),
                                         h3('Gothenburg,\nSweden\n2021'),
                                         width = 2),
                            mainPanel(br(), br(), br(),
                                      h4("This application was designed by Ilya Smirnov, a postdoc in Davide Angeletti's group."),
                                      h4('Please, report problems or suggestions for improvement by email:'),
                                      h4(a('davide.angeletti@gu.se', href='mailto:davide.angeletti@gu.se'),
                                         ' or ',
                                         a('smirnov.iv.mail@gmail.com', href = 'mailto:smirnov.iv.mail@gmial.com')),
                                      br(), br(),
                                      plotOutput('visits')
                                      )
                                      )
                        )
    )
)


server <- function(input, output, session) {
    
    ###### GLOBAL OBJECTS ######
    
    std_fname <- character()
    smpl_fname <- character()
    rfu_fname <- character()
    result_table_df <- NULL
    nls_models_list <- list()
    
    
    ###### VISIT STAT ######
    
    fname <- 'visit_stat.csv'
    if (file.exists(fname)) {
        visit_table <- read.csv(fname, sep = ';', dec = ',')
    } else {
        visit_table <- data.frame(
            date = character(),
            n = integer()
        )
    }
    today <- as.character(Sys.Date())
    n <- visit_table$n[visit_table$date == today]
    if (length(n) == 0) {
        visit_table <- rbind(
            visit_table,
            data.frame(date = today,
                       n = 1)
        )
    } else {
        visit_table$n[visit_table$date == today] <- n + 1
    }
    write.table(visit_table, fname, sep = ';', dec = ',', row.names = FALSE)
    
    ###### LOADING FILES TAB ######
    
      ### SIDE BAR PANEL ###
    
    output$save_st_table <- downloadHandler(
        filename = function() {
            paste0(Sys.Date(), '_standard_table', ".xlsx")
        },
        content = function(file) {
            save_standard_template(file,
                                   columns = input$st_table_from:input$st_table_to,
                                   open_file = FALSE)
        }
    )
    
    output$save_sample_table <- downloadHandler(
        filename = function() {
            paste0(Sys.Date(), '_sample_table', ".xlsx")
        },
        content = function(file) {
            save_sample_template(file,
                                 columns = input$sample_table_from:input$sample_table_to,
                                 open_file = FALSE)
        }
    )
    
    output$save_RFU_table <- downloadHandler(
        filename = function() {
            paste0(Sys.Date(), '_RFU_table', ".xlsx")
        },
        content = function(file) {
            save_RFU_template(file,
                              open_file = FALSE)
        }
    )
    
    
      ### MAIN PANEL ###
    
    standard_data <- reactive({
        standard_data_file <- input$st_table_file
        req(standard_data_file)
        std_fname <<- standard_data_file$name
        read_standard_table(standard_data_file$datapath)
    })
    
    output$standard_table <- renderTable({
        standard_data()[1:10,]
    })
    
    sample_data <- reactive({
        sample_data_file <- input$sample_table_file
        req(sample_data_file)
        smpl_fname <<- sample_data_file$name
        read_sample_table(sample_data_file$datapath)
    })
    
    output$sample_table <- renderTable({
        sample_data()[1:10,]
    })
    
    
    RFU_data <- reactive({
        RFU_data_file <- input$RFU_table_file
        req(RFU_data_file)
        rfu_fname <<- RFU_data_file$name
        read_RFU_data_table(RFU_data_file$datapath, input$time_format)
    })
    
    output$RFU_table <- renderTable({
        RFU_data()[1:10,]
    })
    
    
    ###### STANDARD TAB ######
    
       ### SIDE BAR PANEL ###
    
    bright_value <- reactive({
        if (input$bright_check == FALSE) return()
        input$bright
    })
    
    assay_obj <- eventReactive(input$calculate, {
        result_table_df <<- data.frame()
        assay(standard_data = standard_data(),
              sample_data = sample_data(),
              RFU_data = RFU_data(),
              calibration_method = input$calib_method,
              bright = bright_value())
    })
    
    velo_table <- eventReactive(input$calculate, {
        model_progress_curves(assay_obj())
    })
    
    
       ### MAIN PANEL ###
    
    std_plot <- reactive({
        show_standard_data(assay_obj(), input$standard_plot_mode)
    })
    
    output$standard_plot <- renderPlot({
        std_plot()
    })
    
    int_plot <- reactive({
        if (!input$int_edit_check) {
            plot <- show_calibration_pars(assay_obj(),
                                          parameter = 'intercept',
                                          y_limits = c(NA, NA))
        } else {
            plot <- show_calibration_pars(assay_obj(),
                                          parameter = 'intercept',
                                          y_limits = c(input$int_y_min, input$int_y_max))
        }
        plot
    })
    
    output$plot_intercept <- renderPlot({
        int_plot()
    })
    
    slope_plot <- reactive({
        if (!input$slope_edit_check) {
            plot <- show_calibration_pars(assay_obj(),
                                          parameter = 'slope',
                                          y_limits = c(NA, NA))
        } else {
            plot <- show_calibration_pars(assay_obj(),
                                          parameter = 'slope',
                                          y_limits = c(input$slope_y_min, input$slope_y_max))
        }
        plot
    })
    
    output$plot_slope <- renderPlot({
        slope_plot()
    })
    
    rsq_plot <- reactive({
        if (!input$rsq_edit_check) {
            plot <- show_calibration_pars(assay_obj(),
                                          parameter = 'Rsq',
                                          y_limits = c(NA, NA))
        } else {
            plot <- show_calibration_pars(assay_obj(),
                                          parameter = 'Rsq',
                                          y_limits = c(input$rsq_y_min, input$rsq_y_max))
        }
        plot
    })
    
    output$plot_Rsq <- renderPlot({
        rsq_plot()
    })
    
    
    ###### PROGRESS CURVES TAB ######
    
      ### MAIN PANEL ###
    
    
    # https://gist.github.com/wch/5436415/
    progress_curves_plots <- reactive({
        progress_curves <- show_progress_curves(assay_obj(),
                                                mode = input$prog_curve_mode,
                                                show_velocities = input$add_velo,
                                                curve_models = velo_table())
        for (nm in names(progress_curves)) {
            local({
                loc_nm <- nm
                output[[loc_nm]] <- renderPlot({
                    progress_curves[[loc_nm]]
                })
                
            })
        }
        progress_curves
    })
    
    output$plot_progress_curves <- renderUI({
        plot_list <- list()
        for (nm in names(progress_curves_plots())) {
            plot_list[[nm]] <- plotOutput(nm, width = "700px", height = "550px")
        }
        do.call(tagList, plot_list)
    })
    
    
    ###### MM MODELS TAB ######
    
      ### SIDE PANEL ###
    
    sample_list <- reactive({
        sl <- unique(velo_table()$name)
        if (length(sl) < 2) {
            updateCheckboxGroupInput(session, 'report_comps',
                                     choices = c('Standard plot' = 'std_plot',
                                                 'Parameters of Calibration curves' = 'std_pars',
                                                 'Progress Curves' = 'progress_curves',
                                                 'Velocity Data' = 'velo_data',
                                                 'Michaelis-Menten Plots' = 'mm_plots'),
                                     selected = c('std_plot',
                                                  'std_pars',
                                                  'progress_curves',
                                                  'velo_data',
                                                  'mm_plots'))
            hideTab('main_tabs', target = 'Statistics')
        } else {
            updateCheckboxGroupInput(session, 'report_comps',
                                     choices = c('Standard plot' = 'std_plot',
                                                 'Parameters of Calibration curves' = 'std_pars',
                                                 'Progress Curves' = 'progress_curves',
                                                 'Velocity Data' = 'velo_data',
                                                 'Michaelis-Menten Plots' = 'mm_plots',
                                                 'Km vs Vmax plot' = 'km_vmax_plot',
                                                 'Statistics' = 'stat'),
                                     selected = c('std_plot',
                                                  'std_pars',
                                                  'progress_curves',
                                                  'velo_data',
                                                  'mm_plots',
                                                  'km_vmax_plot',
                                                  'stat'))
            showTab('main_tabs', target = 'Statistics')
        }
        sl
    })
    
    output$sample_list <- renderUI({
        selectInput('sample_list_select', 'Sample',
                    choices = sample_list(),
                    selected = sample_list()[1])
    })
    
      ### MAIN PANEL ###
    
    current_velo_table <- reactive({
        vt <- velo_table()
        if (dim(vt)[1] > 0) {
            return(subset(velo_table(), name == input$sample_list_select))
        }
    })
    
    output$save_current_velocity_table <- downloadHandler(
        filename = function() {
            paste0(Sys.Date(), '_', input$sample_list_select, "_velocity data.xlsx")
        },
        content = function(file) {
            df <- current_velo_table()
            save_table(df, file, open_file = FALSE)
        }
    )
    
    output$save_all_velocity_table <- downloadHandler(
        filename = function() {
            paste0(Sys.Date(), "_velocity data.xlsx")
        },
        content = function(file) {
            df <- velo_table()
            save_table(df, file, open_file = FALSE)
        }
    )
    
    output$velo_table_output <- renderTable({
        current_velo_table()
    })
    
    
    last_sample_value <- character(0)
    
    current_nls_model <- reactive({
        req(current_velo_table())
        guess <- guess_vmax_km(current_velo_table())
        vmax = guess[1]
        km = guess[2]
        req(vmax, km)
        if (length(last_sample_value) == 0 & !is.na(vmax) & !is.na(km)) {
            updateNumericInput(session, 'Vmax', value = round(vmax, 2))
            updateNumericInput(session, 'Km', value = round(km))
            last_sample_value <<- input$sample_list_select
        }
        if (input$manual_vmax_km) {
            if (input$sample_list_select != last_sample_value) {
                updateNumericInput(session, 'Vmax', value = round(vmax, 2))
                updateNumericInput(session, 'Km', value = round(km))
                last_sample_value <<- input$sample_list_select
            }
            vmax = input$Vmax
            km = input$Km
        }
        nls_model <- get_nls(current_velo_table(), vmax = vmax, km = km)
        if (!is.null(input$sample_list_select)) {
            nls_models_list[[input$sample_list_select]] <<- nls_model
        }
        nls_model
    })
    
    output$mm_plot <- renderPlot({
        req(current_velo_table())
        model <- if (input$add_mm_line) current_nls_model() else NULL
        show_mm_plot(current_velo_table(), model = model)
    })
    
    output$nls_model_text <- renderUI({
        text <- capture.output(current_nls_model())
        HTML(paste('<b>', text, '</b>', collapse = '<br/>'))
    })
    
    Vmax <- reactive({
        if (is.null(current_nls_model())) return(c(0, 0, 0))
        get_coef_ci(current_nls_model(), 'Vmax')
    })
    
    output$Vmax_out <- renderText({
        val <- Vmax()
        if (all(val == 0)) return('')
        paste0('Vmax = ', val[1], ' (', val[2], '-', val[3], ')')
    })
    
    Km <- reactive({
        if (is.null(current_nls_model())) return(c(0, 0, 0))
        get_coef_ci(current_nls_model(), 'Km')
    })
    
    output$Km_out <- renderText({
        val <- Km()
        if (all(val == 0)) return('')
        paste0('Km = ', val[1], ' (', val[2], '-', val[3], ')')
    })
    
    
    result_table <- reactive({
        # auto calculation of result table
        if (dim(result_table_df)[1] == 0) {
            s_list <- sample_list()
            len <- length(s_list)
            result_table_df <<- init_result_table(s_list)
            for (smpl in s_list) {
                v_data <- subset(velo_table(), name == smpl)
                guess <- guess_vmax_km(v_data)
                nls <- get_nls(v_data,
                               vmax = guess[1],
                               km = guess[2])
                nls_models_list[[smpl]] <<- nls
                Vmax <- get_coef_ci(nls, 'Vmax')
                Km <- get_coef_ci(nls, 'Km')
                result_table_df <<- edit_result_table(result_table_df,
                                                      name = smpl,
                                                      Vmax = Vmax[1],
                                                      Vmax_lower = Vmax[2],
                                                      Vmax_upper = Vmax[3],
                                                      Km = Km[1],
                                                      Km_lower = Km[2],
                                                      Km_upper = Km[3])
            }
        # manual correction of results
        } else {
            smpl <- input$sample_list_select
            result_table_df <<- edit_result_table(result_table_df,
                                                  name = smpl, Vmax = Vmax, Km = Km)
            
        }
        result_table_df
    })

    output$result_table <- renderTable({
        result_table()
    }, digits = 3)
    
    output$save_mm_table <- downloadHandler(
        filename = function() {
            paste0(Sys.Date(), "_mm data.xlsx")
        },
        content = function(file) {
            df <- result_table()
            save_table(df, file, open_file = FALSE)
        }
    )
    
    
    ###### PLOTS TAB ######
    
      ### SIDE PANEL ###
    
    output$sample_check_list <- renderUI({
        sl <- sample_list()
        checkboxGroupInput('samples_check', label = 'Add to plot only:',
                           choices = sl, selected = sl)
    })
    
      ### MAIN PANEL ###
    
    mm_plots_layout <- reactive({
        show_mm_plots_layout(velo_table(), nls_models_list, input$samples_check)
    })
    
    output$mm_plots_layout <- renderPlot({
        mm_plots_layout()
    })
    
    sub_result_table <- reactive({
        req(velo_table())
        if (is.null(input$samples_check)) {
            sub_df <- subset(result_table(), Km != 0)
        } else {
            sub_df <- subset(result_table(), name %in% input$samples_check)
        }
        sub_df
    })
    
    output$result_table_selected <- renderTable({
        subset(sub_result_table(), select = c(name, Vmax, Km))
    })
    
    km_vmax_plot <- reactive({
        show_km_vmax(sub_result_table(), input$show_error_bars)
    })
    
    output$km_vmax_plot <- renderPlot({
        km_vmax_plot()
    })
    
    
    ###### STATSTICS TAB ######
    
      ### SIDE PANEL ###
    
    output$ref_sample_select <- renderUI({
        sl <- sample_list()
        selectInput('ref_sample', 'Sample',
                    choices = sl, selected = sl[1])
    })
    
      ### MAIN PANEL ###
    
    get_stat <- reactive({
        vd <- velo_table()
        rt <- result_table()
        if (dim(vd)[1] > 0 & dim(rt)[2] & !is.null(input$ref_sample)) {
            stat <- compare_vmax_km(velo_data = velo_table(),
                                    vmax_km_data = result_table(),
                                    ref_sample = input$ref_sample,
                                    p.adjust_method = input$p_adjust)
        } else return(NULL)
        stat
    })
    
    output$vmax_stat <- renderTable({
        get_stat()[[1]]
    }, digits = 4)
    
    output$save_vmax_data <- downloadHandler(
        filename = function() {
            paste0(Sys.Date(), "_vmax stat.xlsx")
        },
        content = function(file) {
            df <- get_stat()[[1]]
            save_table(df, file, open_file = FALSE)
        }
    )
    
    output$km_stat <- renderTable({
        get_stat()[[2]]
    }, digits = 4)
    
    output$save_km_data <- downloadHandler(
        filename = function() {
            paste0(Sys.Date(), "_km stat.xlsx")
        },
        content = function(file) {
            df <- get_stat()[[2]]
            save_table(df, file, open_file = FALSE)
        }
    )
    
    
    ###### REPORT TAB ######
    
      ### MAIN PANEL ###
    
    output$save_report <- downloadHandler(
        filename = function() {
            paste0(Sys.Date(), '_report.docx')
        },
        content = function(file) {
            save_report(file,
                        date = as.character(input$date),
                        title = input$report_title,
                        std_fname = std_fname,
                        smpl_fname = smpl_fname,
                        rfu_fname = rfu_fname,
                        calib_method = input$calib_method,
                        bright_check = input$bright_check,
                        bright_diff = input$bright,
                        standard_plot = std_plot(),
                        slope_plot = slope_plot(),
                        int_plot = int_plot(),
                        rsq_plot = rsq_plot(),
                        progress_curves = progress_curves_plots(),
                        velo_data_table = velo_table(),
                        mm_plots = mm_plots_layout(),
                        result_table = result_table(),
                        km_vmax_plot = km_vmax_plot(),
                        vmax_stat_table = get_stat()[[1]],
                        km_stat_table = get_stat()[[2]],
                        items_to_include = input$report_comps)
        }
    )
    
    ###### UTILITIES ######
    
    #### BRIGHTNESS DIFFERENCE ####
    
    output$save_bright_template <- downloadHandler(
        filename = function() {
            'brightness_difference.xlsx'
        },
        content = function(file) {
            save_brightdiff_template(file)
        }
    )
    
    bright_diff_obj <- reactive({
        file <- input$br_table_file
        req(file)
        calculate_bright_diff(file$datapath)
    })    

    output$br_diff <- renderText({
        result <- bright_diff_obj()
        paste('Difference:', result[[1]], 'times')
    })
        
    output$bright_diff_plot <- renderPlot({
        result <- bright_diff_obj()
        result[[2]]
    })
    
    
    #### SPECTRUM ANALYSIS ####
    
    output$save_spectrum_template <- downloadHandler(
        filename = function() {
            'spectrum_analysis.xlsx'
        },
        content = function(file) {
            save_brightdiff_template(file)
        }
    )
    
    spectrum_obj <- reactive({
        file <- input$spectrum_table_file
        req(file)
        input$bandwidth
        spectrum_analysis(file$datapath,
                          show_points = input$show_points,
                          log_scale = input$log_scale,
                          add_band = input$add_band,
                          bandwidth = input$bandwidth)
    })   
    
    output$br_max <- renderText({
        result <- spectrum_obj()
        paste('Emission max:', result[[1]], 'nm')
    })
    
    output$spectrum_plot <- renderPlot({
        result <- spectrum_obj()
        result[[2]]
    })
    
    
    ###### ABOUT TAB ######
    
    
    output$visits <- {
        renderPlot({
            n_visits <- sum(visit_table$n)
            plot <- ggplot(visit_table, aes(as.Date(date), n)) +
                geom_line(color = 'darkblue') +
                plot_theme +
                theme(plot.title = element_text(size = 18)) +
                labs(x = 'Date', y = 'Number of visits') +
                ggtitle(paste('Total number of visits:', n_visits), )
            plot
        })
    }

}


shinyApp(ui, server)
