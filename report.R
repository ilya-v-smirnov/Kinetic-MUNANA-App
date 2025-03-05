
# Load required libraries
library(officer)
library(cowplot)

# ------------------------------------------------------------------------------
# Define text and paragraph formatting styles using officer's formatting functions
# ------------------------------------------------------------------------------

# Date text style
date_text_style <- fp_text(font.size = 11, underlined = TRUE, font.family = 'Times')
date_par_style <- fp_par(padding.bottom = 15)

# Title text style
title_text_style <- fp_text(font.size = 13, bold = TRUE, font.family = 'Times')
title_par_style <- fp_par(text.align = 'center', padding.bottom = 10, padding.top = 10)

# Section header style
section_text_style <- fp_text(font.size = 12, bold = TRUE, italic = TRUE, font.family = 'Times')
section_par_style <- fp_par(padding.bottom = 10, padding.top = 10)

# Bold text style
bold_text <- fp_text(font.size = 12, bold = TRUE, font.family = 'Times')

# File name text style
fname_text_style <- fp_text(color = 'dark green', font.family = 'Courier')

# ------------------------------------------------------------------------------
# Define a common theme for report figures using ggplot2 settings
# ------------------------------------------------------------------------------
report_fig_theme <-
    theme(strip.text = element_text(size = 11, face = "plain"),
          legend.title = element_text(size = 9),
          legend.text = element_text(size = 8),
          legend.key.size = unit(0.7, "line"),
          axis.title = element_text(size = 10),
          axis.text = element_text(size = 9, color = "black"))

# ------------------------------------------------------------------------------
# Main Function: save_report
# Generates a Word document report with various plots and tables.
#
# Parameters:
#   path             - File path to save the report.
#   date, title      - Report date and title.
#   std_fname        - File name for standard table.
#   smpl_fname       - File name for sample table. 
#   rfu_fname        - File name for RFU data.
#   calib_method     - Calibration method, e.g. 'log-linear' or 'linear'.
#   bright_check     - Logical flag to include brightness difference details.
#   bright_diff      - Numeric value representing brightness difference.
#   standard_plot    - ggplot object for the standard plot.
#   slope_plot       - ggplot object for slope parameter.
#   int_plot         - ggplot object for intercept parameter.
#   rsq_plot         - ggplot objects for R-squared parameter.
#   progress_curves  - List of ggplot objects for progress curves.
#   velo_data_table  - Data table for velocity data.
#   mm_plots         - ggplot object for Michaelis-Menten plots.
#   result_table     - Data table for Km and Vmax results.
#   km_vmax_plot     - ggplot object for Km vs Vmax plot.
#   vmax_stat_table  - Data table for Vmax statistical parameters.
#   km_stat_table    - Data table for Km statistical parameters.
#   padjust          - String indicating the p-value adjustment method.
#   items_to_include - Optional vector specifying which report sections to include.
# ------------------------------------------------------------------------------
save_report <- function(path,
                        date, title,
                        std_fname, smpl_fname, rfu_fname,
                        calib_method, bright_check, bright_diff,
                        standard_plot,
                        slope_plot, int_plot, rsq_plot,
                        progress_curves,
                        velo_data_table,
                        mm_plots,
                        result_table,
                        km_vmax_plot,
                        vmax_stat_table,
                        km_stat_table,
                        padjust,
                        items_to_include = NULL) {
    
    # ---------------------------
    # Section: Date, Title, and File Names
    # ---------------------------
    # Create formatted text for the date and title using the defined styles
    date_text <- fpar(ftext(date, date_text_style), fp_p = date_par_style)
    title_text <- fpar(ftext(title, title_text_style), fp_p = title_par_style)
    
    # Format file names for Standard, Sample, and RFU data
    standard_fname <- ftext(std_fname, fname_text_style)
    standard_fname_line <- fpar('Standard file:\t', standard_fname)
    
    sample_fname <- ftext(smpl_fname, fname_text_style)
    sample_fname_line <- fpar('Sample file:\t', sample_fname)
    
    RFUdata_fname <- ftext(rfu_fname, fname_text_style)
    RFUdata_fname_line <- fpar('RFU data:\t', RFUdata_fname)
    
    # Initialize an empty Word document and add the date, title, and file name lines
    report <- read_docx() %>%
        body_add_fpar(date_text) %>%
        body_add_fpar(title_text) %>%
        body_add_fpar(standard_fname_line) %>%
        body_add_fpar(sample_fname_line) %>%
        body_add_fpar(RFUdata_fname_line)
    
    # ---------------------------
    # Section: Standard Plot
    # ---------------------------
    # Check if the "std_plot" section should be included based on items_to_include
    if (! is.null(items_to_include) & 'std_plot' %in% items_to_include) {
        
        # Create a section header for the Standard Plot
        std_plot_line <- fpar(ftext('Standard plot', section_text_style), fp_p = section_par_style)
        
        # Format the calibration method text: convert 'log-linear' to 'Log-linear' if necessary
        calib_method <- ifelse(calib_method == 'log-linear', 'Log-linear', 'Linear')
        calib_method_text <- ftext(calib_method, bold_text)
        calib_method_line <- fpar('Calibration method: ', calib_method_text)
        
        # Add the Standard Plot section header, plot (with the theme), and calibration method info
        report <- report %>%
            body_add_fpar(std_plot_line) %>%
            body_add_gg(standard_plot + report_fig_theme, width = 6, height = 4) %>%
            body_add_fpar(calib_method_line)
        
        # If brightness check is enabled, add brightness difference information
        if (bright_check) {
            bright_diff_text <- ftext(as.character(bright_diff), bold_text)
            brifht_diff_line <- fpar('Brightness difference: ', bright_diff_text)
            
            report <- report %>%
                body_add_fpar(brifht_diff_line)
        }
    }
    
    # ---------------------------
    # Section: Parameters of Calibration Curves
    # ---------------------------
    # Check if the "std_pars" section should be included
    if (! is.null(items_to_include) & 'std_pars' %in% items_to_include) {
        
        # Create a section header for calibration curve parameters
        stdpar_plot_line <- fpar(ftext('Parameters of calibration curves', section_text_style), fp_p = section_par_style)
        
        # Combine the slope, intercept, and R-squared plots into one grid layout
        stdpar_plot <- plot_grid(slope_plot + report_fig_theme,
                                 int_plot + report_fig_theme,
                                 rsq_plot + report_fig_theme, nrow = 1)
        
        # Add the header and combined plot to the report
        report <- report %>%
            body_add_fpar(stdpar_plot_line) %>%
            body_add_gg(stdpar_plot + report_fig_theme, width = 6.5, height = 2.4)
    }
    
    # ---------------------------
    # Section: Progress Curves
    # ---------------------------
    # Check if the "progress_curves" section should be included
    if (! is.null(items_to_include) & 'progress_curves' %in% items_to_include) {
        
        # Create a section header for Progress Curves
        progcurves_plot_line <- fpar(ftext('Progress curves', section_text_style), fp_p = section_par_style)
        
        # Add the progress curves header to the report
        report <- body_add_fpar(report, progcurves_plot_line)
        
        # Loop through each progress curve plot and add it to the report with the defined theme
        for (pc in progress_curves) {
            report <- body_add_gg(report, pc + report_fig_theme, width = 5, height = 4)
        }
    }
    
    # ---------------------------
    # Section: Velocity Data Table
    # ---------------------------
    # Check if the "velo_data" section should be included
    if (! is.null(items_to_include) & 'velo_data' %in% items_to_include) {
        
        # Create a section header for Velocity Data
        velocity_data_line <- fpar(ftext('Velocity data', section_text_style), fp_p = section_par_style)
        # Round selected numeric columns (columns 4 to 7) in the velocity data table for better presentation
        velo_data_table <- cbind(velo_data_table[, 1:3], round(velo_data_table[, 4:7], 3))
        
        # Add the header and velocity data table to the report
        report <- report %>%
            body_add_fpar(velocity_data_line)  %>%
            body_add_table(velo_data_table, style = 'table_template')
    }
    
    # ---------------------------
    # Section: Michaelis-Menten (MM) Plots
    # ---------------------------
    # Check if the "mm_plots" section should be included
    if (! is.null(items_to_include) & 'mm_plots' %in% items_to_include) {
        
        # Create a section header for Michaelis-Menten Plots
        mm_plot_line <- fpar(ftext('Michaelis-Menten Plots', section_text_style), fp_p = section_par_style)
        
        # Add the header and MM plot to the report
        report <- report %>%
            body_add_fpar(mm_plot_line) %>%
            body_add_gg(mm_plots + report_fig_theme, width = 6, height = 4.5)
    }
    
    # ---------------------------
    # Section: Km and Vmax Table
    # ---------------------------
    # Create a section header for the Km and Vmax Table
    km_vmax_line <- fpar(ftext('Km and Vmax Table', section_text_style), fp_p = section_par_style)
    
    # Add the header and the result table (containing Km and Vmax data) to the report
    report <- report %>%
        body_add_fpar(km_vmax_line) %>%
        body_add_table(result_table, style = 'table_template')
    
    # ---------------------------
    # Section: Km vs Vmax Plot
    # ---------------------------
    # Check if the "km_vmax_plot" section should be included
    if (! is.null(items_to_include) & 'km_vmax_plot' %in% items_to_include) {
        
        # Add a blank line followed by the Km vs Vmax plot to the report
        report <- report %>%
            body_add('') %>%
            body_add_gg(km_vmax_plot + report_fig_theme, width = 6, height = 4.5)
    }
    
    # ---------------------------
    # Section: Statistical Analysis
    # ---------------------------
    # Check if the "stat" section should be included
    if (! is.null(items_to_include) & 'stat' %in% items_to_include) {
        
        # Create section headers for Vmax and Km statistics
        vmax_line <- fpar(ftext('Statistics: Vmax', section_text_style), fp_p = section_par_style)
        km_line <- fpar(ftext('Statistics: Km', section_text_style), fp_p = section_par_style)
        
        # Convert the padjust code to a full descriptive method name using a switch statement
        padjust <- switch(padjust,
                          'no' = 'No',
                          'holm' = 'Holm',
                          'hochberg' = 'Hochberg',
                          'hommel' = 'Hommel',
                          'bonferroni' = 'Bonferroni',
                          'BH' = 'Benjamini & Hochberg',
                          'BY' = 'Benjamini & Yekutieli')
        
        padjust_method_text <- ftext(padjust, bold_text)
        padjust_method_line <- fpar('P-value adjustment method: ', padjust_method_text)
        
        # Add Vmax and Km statistics tables along with the p-value adjustment method info
        report <- report %>%
            body_add_fpar(vmax_line) %>%
            body_add_table(vmax_stat_table, style = 'table_template') %>%
            body_add_fpar(km_line) %>%
            body_add_table(km_stat_table, style = 'table_template') %>%
            body_add_fpar(padjust_method_line)
    }
    
    # ------------------------------------------------------------------------------
    # Final Step: Save the constructed Word document to the specified path.
    # ------------------------------------------------------------------------------
    print(report, target = path)
    
}
