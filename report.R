
library(officer)
library(cowplot)


# Styles
date_text_style <- fp_text(font.size = 11, underlined = TRUE, font.family = 'Times')
date_par_style <- fp_par(padding.bottom = 15)

title_text_style <- fp_text(font.size = 13, bold = TRUE, font.family = 'Times')
title_par_style <- fp_par(text.align = 'center', padding.bottom = 10, padding.top = 10)

section_text_style <- fp_text(font.size = 12, bold = TRUE, italic = TRUE, font.family = 'Times')
section_par_style <- fp_par(padding.bottom = 10, padding.top = 10)

bold_text <- fp_text(font.size = 12, bold = TRUE, font.family = 'Times')

fname_text_style <- fp_text(color = 'dark green', font.family = 'Courier')

report_fig_theme <-
    theme(strip.text = element_text(size = 11, face = "plain"),
          legend.title = element_text(size = 9),
          legend.text = element_text(size = 8),
          legend.key.size = unit(0.7, "line"),
          axis.title = element_text(size = 10),
          axis.text = element_text(size = 9, color = "black"))



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
    
    # Date, title and files
    date_text <- fpar(ftext(date, date_text_style), fp_p = date_par_style)
    title_text <- fpar(ftext(title, title_text_style), fp_p = title_par_style)
    
    standard_fname <- ftext(std_fname, fname_text_style)
    standard_fname_line <- fpar('Standard file:\t', standard_fname)
    sample_fname <- ftext(smpl_fname, fname_text_style)
    sample_fname_line <- fpar('Sample file:\t', sample_fname)
    RFUdata_fname <- ftext(rfu_fname, fname_text_style)
    RFUdata_fname_line <- fpar('RFU data:\t', RFUdata_fname)
    
    report <- read_docx() %>%
        body_add_fpar(date_text) %>%
        body_add_fpar(title_text) %>%
        body_add_fpar(standard_fname_line) %>%
        body_add_fpar(sample_fname_line) %>%
        body_add_fpar(RFUdata_fname_line)
        
    # Standard plot
    
    if (! is.null(items_to_include) & 'std_plot' %in% items_to_include) {
        
        std_plot_line <- fpar(ftext('Standard plot', section_text_style), fp_p = section_par_style)
        calib_method <- ifelse(calib_method == 'log-linear', 'Log-linear', 'Linear')
        calib_method_text <- ftext(calib_method, bold_text)
        calib_method_line <- fpar('Calibration method: ', calib_method_text)
        
        report <- report %>%
            body_add_fpar(std_plot_line) %>%
            body_add_gg(standard_plot+report_fig_theme, width = 6, height = 4) %>%
            body_add_fpar(calib_method_line)
        
        if (bright_check) {
            bright_diff_text <- ftext(as.character(bright_diff), bold_text)
            brifht_diff_line <- fpar('Brightness difference: ', bright_diff_text)
            
            report <- report %>%
                body_add_fpar(brifht_diff_line)
        }
    }
    
    # Parameters of calibration curves
    
    if (! is.null(items_to_include) & 'std_pars' %in% items_to_include) {
        
        stdpar_plot_line <- fpar(ftext('Parameters of calibration curves', section_text_style), fp_p = section_par_style)
        
        stdpar_plot <- plot_grid(slope_plot+report_fig_theme,
                                 int_plot+report_fig_theme,
                                 rsq_plot+report_fig_theme, nrow = 1)
        
        report <- report %>%
            body_add_fpar(stdpar_plot_line) %>%
            body_add_gg(stdpar_plot+report_fig_theme, width = 6.5, height = 2.4)
    }
    
    # Progress curves
    
    if (! is.null(items_to_include) & 'progress_curves' %in% items_to_include) {
        
        progcurves_plot_line <- fpar(ftext('Progress curves', section_text_style), fp_p = section_par_style)
        
        report <- body_add_fpar(report, progcurves_plot_line)
        
        for (pc in progress_curves) {
            report <- body_add_gg(report, pc+report_fig_theme, width = 5, height = 4)
        }
    }
    
    # Velocity Data
    
    if (! is.null(items_to_include) & 'velo_data' %in% items_to_include) {
        
        velocity_data_line <- fpar(ftext('Velocity data', section_text_style), fp_p = section_par_style)
        velo_data_table <- cbind(velo_data_table[, 1:3], round(velo_data_table[, 4:7], 3))
        
        report <- report %>%
            body_add_fpar(velocity_data_line)  %>%
            body_add_table(velo_data_table, style = 'table_template')
    }
    
    # MM Plots
    
    if (! is.null(items_to_include) & 'mm_plots' %in% items_to_include) {
        
        mm_plot_line <- fpar(ftext('Michaelis-Menten Plots', section_text_style), fp_p = section_par_style)
        
        report <- report %>%
            body_add_fpar(mm_plot_line) %>%
            body_add_gg(mm_plots+report_fig_theme, width = 6, height = 4.5)
    }
    
    # Km and Vmax Table
    
    km_vmax_line <- fpar(ftext('Km and Vmax Table', section_text_style), fp_p = section_par_style)
    
    report <- report %>%
        body_add_fpar(km_vmax_line) %>%
        body_add_table(result_table, style = 'table_template')
    
    # Km vs Vmax plot
    
    if (! is.null(items_to_include) & 'km_vmax_plot' %in% items_to_include) {
        
        report <- report %>%
            body_add('') %>%
            body_add_gg(km_vmax_plot+report_fig_theme, width = 6, height = 4.5)
    }
    
    # Statistics
    
    if (! is.null(items_to_include) & 'stat' %in% items_to_include) {
        
        vmax_line <- fpar(ftext('Statistics: Vmax', section_text_style), fp_p = section_par_style)
        km_line <- fpar(ftext('Statistics: Km', section_text_style), fp_p = section_par_style)
        
        padjust <- switch (padjust,
                           'no' = 'No',
                           'holm' = 'Holm',
                           'hochberg' = 'Hochberg',
                           'hommel' = 'Hommel',
                           'bonferroni' = 'Bonferroni',
                           'BH' = 'Benjamini & Hochberg',
                           'BY' = 'Benjamini & Yekutieli')
        
        padjust_method_text <- ftext(padjust, bold_text)
        padjust_method_line <- fpar('P-value adjustment method: ', padjust_method_text)
        
        report <- report %>%
            body_add_fpar(vmax_line) %>%
            body_add_table(vmax_stat_table, style = 'table_template') %>%
            body_add_fpar(km_line) %>%
            body_add_table(km_stat_table, style = 'table_template') %>%
            body_add_fpar(padjust_method_line)
    }
    
    print(report, target = path)
    
}

