
library(reshape2)

# path <- 'D:/R projects/Kinetic-MUNANA-App/'
# setwd(path)
# source('./MUNANA2.R')


calculate_bright_diff <- function(path) {
    
    data <- read_table(path)
    cols <- c('conc', 'substrate', 'product', 'buffer')
    for (col in cols) {
        if (! col %in% names(data)) stop(paste('Column', col, 'is missing!'))
        if (! is.numeric(data[, col])) stop(paste('Non-numeric values in', col, 'column!'))
    }
    NC <- geo_mean(data$buffer)
    RFUdata <- melt(data,
                    id.vars = 'conc',
                    measure.vars = c('substrate', 'product'),
                    variable.name = 'type',
                    value.name = 'RFU')
    RFUdata <- subset(RFUdata, complete.cases(RFU))
    RFUdata$RFU <- RFUdata$RFU - NC

    fit <- lm(log10(RFU) - log10(conc) ~ type, RFUdata)
    
    coef_df <- data.frame(type = c('substrate', 'product'),
                          slopes = 1,
                          intercepts = c(coef(fit)[1], sum(coef(fit))))
    
    fig <-
    ggplot(RFUdata, aes(conc, RFU, color = type)) +
        geom_point(size = 2) +
        geom_abline(data = coef_df, aes(intercept = intercepts,
                                        slope = slopes,
                                        color = type),
                    size = 1) +
        scale_x_log10() +
        scale_y_log10() +
        scale_color_discrete(name = '', labels = c('Substrate', 'Product')) +
        plot_theme +
        labs(x = expression('Concentration, '*mu*'M'),
             y = expression('RFU, ' ~log[10]))
    br_diff <- as.numeric(round(10^coef(fit)[2]))
    return(list(br_diff = br_diff,
                figure = fig))
}

save_brightdiff_template <- function(path) {
    df <- data.frame(conc = '',
                     substrate = '',
                     product = '',
                     buffer = '')
    save_table(df, path, open_file = FALSE)
}

# calculate_bright_diff('./Sample Data Sets/4-MU vs MUNANA/4-MU vs MUNANA.xlsx')

spectrum_analysis <- function(path,
                              show_points = FALSE,
                              log_scale = FALSE,
                              add_band = TRUE,
                              bandwidth = 9) {
    
    data <- read_table(path)
    cols <- c('wavelength', 'substrate', 'product', 'buffer')
    for (col in cols) {
        if (! col %in% names(data)) stop(paste('Column', col, 'is missing!'))
        if (! is.numeric(data[, col])) stop(paste('Non-numeric values in', col, 'column!'))
    }
    
    NC <- aggregate(buffer ~ wavelength, data, geo_mean)
    
    RFUdata <- melt(subset(data, select = -buffer), id.vars = 'wavelength', variable.name = 'type', value.name = 'RFU')
    RFUdata <- merge(RFUdata, NC, by = 'wavelength')
    RFUdata$RFU <- RFUdata$RFU - RFUdata$buffer
    product <- aggregate(RFU ~ wavelength,  RFUdata, subset = type == 'product', geo_mean)
    max_br <- product$wavelength[which(product$RFU == max(product$RFU))]
    
    if (log_scale) {
        fig <-  ggplot(RFUdata, aes(wavelength, log10(RFU))) +
            labs(x  = 'Wavelenght, nm', y = expression('RFU, ' ~log[10]))
    } else {
        fig <- ggplot(RFUdata, aes(wavelength, RFU)) +
            labs(x  = 'Wavelenght, nm', y = 'RFU')
    }
    if (add_band) {
        fig <- fig +
            geom_rect(aes(xmin = max_br - bandwidth,
                          xmax = max_br + bandwidth,
                          ymin = -Inf, ymax = Inf),
                      fill = 'grey80', color = 'white',
                      alpha = 0.015)
    }
    fig <- fig +
        stat_summary(fun = 'geo_mean', geom = 'line', aes(color = type), size = 1) +
        geom_vline(xintercept = max_br, size = 1, color = 'red') +
        scale_color_discrete(name = '', labels = c('Substrate', 'Product')) +
        plot_theme 
    if (show_points) {
        fig <- fig + geom_point(aes(color = type))
    }
    return(list(max_br = max_br,
                figure = fig))
}

# spectrum_analysis('./Sample Data Sets/4-MU and MUNANA spectra/4-MU and MUNANA spectra.xlsx', show_points = T, log_scale = T, add_band = T, bandwidth = 1)


save_spectrum_template <- function(path) {
    df <- data.frame(wavelength = '',
                     substrate = '',
                     product = '',
                     buffer = '')
    save_table(df, path, open_file = FALSE)
}


# Generates a sequence of substrate concentrations based on starting concentration and titration step

substrate_titration <- function(start, step, n_steps = 8) signif(start/step^(0:(n_steps - 1)), 3)

 
show_modelling_plot <- function(start_conc, titr_step, n_steps = 8, 
                      Km1, Vmax1,
                      Km2 = NA, Vmax2 = NA,
                      error_sd = 0.0001) {
    velocity <- function(substrate_conc, Km, Vmax) {
        (Vmax * substrate_conc) / (Km + substrate_conc)
    }
    
    if (is.na(start_conc) | is.na(titr_step) | is.na(n_steps)) return(NULL)
    s_conc <- substrate_titration(start_conc, titr_step, n_steps)
    
    if (is.na(Km1) | is.na(Vmax1)) return(NULL)
    velo_data <- 
        data.frame(substrate_conc = s_conc,
                   b = velocity(s_conc, Km = Km1, Vmax = Vmax1) + rnorm(n_steps, mean = 0, sd = error_sd))
    model <- get_nls(velo_data, vmax = Vmax1, km = Km1)
    
    if (is.na(Km2) | is.na(Vmax2)) {
        plot <- show_mm_plot(data = velo_data, model = model)
    } else {
        velo_data2 <- 
            data.frame(substrate_conc = s_conc,
                       b = velocity(s_conc, Km = Km2, Vmax = Vmax2) + rnorm(n_steps, mean = 0, sd = error_sd))
        model2 <- get_nls(velo_data2, vmax = Vmax2, km = Km2)
        velo_data$name <- 'Reference'
        velo_data2$name <- 'Test Sample'
        vd <- rbind(velo_data, velo_data2)
        plot <- show_mm_plots_layout(velocity_data = vd,
                                     nls_models = list('Reference' = model,
                                                       'Test Sample' = model2))
    }
    
    plot
}


# show_modelling_plot(100, n_steps = 5, titr_step = 2, Km1 = 12, Vmax1 = 0.3, Km2 = 22, Vmax2 = 0.6, error_sd = 0.0001)
