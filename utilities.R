
library(reshape2)

# path <- 'D:/Google Drive/Gotheburg University/Diary/!Projects/14. MUNANA app/'
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

spectrum_analysis('./Sample Data Sets/4-MU and MUNANA spectra/4-MU and MUNANA spectra.xlsx', show_points = T, log_scale = T, add_band = T, bandwidth = 1)


save_spectrum_template <- function(path) {
    df <- data.frame(wavelength = '',
                     substrate = '',
                     product = '',
                     buffer = '')
    save_table(df, path, open_file = FALSE)
}