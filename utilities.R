

library(reshape2)


#' Calculate Bright Difference
#'
#' Reads a table from the given file path, performs baseline correction on fluorescence data (RFU)
#' by subtracting the geometric mean of the buffer values, reshapes the data, fits a linear model on
#' the log-transformed data, and creates a log-log plot with fitted lines.
#'
#' @param path Character. The file path to the input data.
#'
#' @return A list with two elements:
#'   - br_diff: Numeric. The calculated bright difference value.
#'   - figure: A ggplot object of the log-log plot showing the data and fitted lines.
#' }

calculate_bright_diff <- function(path) {
    
    # Read the data from the file at 'path'
    data <- read_table(path)
    
    # List of required columns for analysis
    cols <- c('conc', 'substrate', 'product', 'buffer')
    
    # Loop through each required column to verify existence and numeric type
    for (col in cols) {
        if (! col %in% names(data)) 
            stop(paste('Column', col, 'is missing!'))
        if (! is.numeric(data[, col])) 
            stop(paste('Non-numeric values in', col, 'column!'))
    }
    
    # Compute the geometric mean of the buffer values (baseline correction)
    NC <- geo_mean(data$buffer)
    
    # Reshape data from wide to long format for 'substrate' and 'product'
    RFUdata <- melt(data,
                    id.vars = 'conc',
                    measure.vars = c('substrate', 'product'),
                    variable.name = 'type',
                    value.name = 'RFU')
    
    # Remove rows with missing RFU values
    RFUdata <- subset(RFUdata, complete.cases(RFU))
    # Subtract baseline (NC) from each RFU measurement
    RFUdata$RFU <- RFUdata$RFU - NC
    
    # Fit a linear model on the difference in log-transformed RFU and concentration,
    # using 'type' (substrate or product) as a predictor.
    fit <- lm(log10(RFU) - log10(conc) ~ type, RFUdata)
    
    # Build a data frame with parameters for plotting reference lines (slope = 1)
    coef_df <- data.frame(type = c('substrate', 'product'),
                          slopes = 1,
                          intercepts = c(coef(fit)[1], sum(coef(fit))))
    
    # Create a ggplot figure using a log-log scale for both axes
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
    
    # Calculate the bright difference from the model coefficients
    br_diff <- as.numeric(round(10^coef(fit)[2]))
    
    # Return the bright difference value and the plot as a list
    return(list(br_diff = br_diff,
                figure = fig))
}


#' Save Bright Difference Template
#'
#' Creates and saves a template data frame containing the required columns for the bright difference
#' calculation and writes it to the specified path.
#'
#' @param path Character. The file path where the template will be saved.
#'
#' @return None. The function writes a file to disk.

save_brightdiff_template <- function(path) {
    # Create an empty data frame with required columns
    df <- data.frame(conc = '',
                     substrate = '',
                     product = '',
                     buffer = '')
    # Save the template to the given path without opening the file
    save_table(df, path, open_file = FALSE)
}


#' Spectrum Analysis
#'
#' Reads spectral data from a file, performs baseline correction using the buffer column,
#' identifies the wavelength with maximum brightness for the product channel, and creates a spectrum plot.
#' Options include log scaling, adding a shaded band around the maximum brightness wavelength, and displaying
#' data points.
#'
#' @param path Character. The file path to the spectral data.
#' @param show_points Logical. Whether to display data points on the plot. Default is FALSE.
#' @param log_scale Logical. Whether to use a log scale for the RFU values on the plot. Default is FALSE.
#' @param add_band Logical. Whether to add a shaded band around the maximum brightness wavelength. Default is TRUE.
#' @param bandwidth Numeric. The half-width of the band around the maximum brightness wavelength. Default is 9.
#'
#' @return A list with two elements:
#'   - max_br: Numeric. The wavelength corresponding to the maximum brightness (RFU) for the product.
#'   - figure: A ggplot object of the spectral analysis plot.

spectrum_analysis <- function(path,
                              show_points = FALSE,
                              log_scale = FALSE,
                              add_band = TRUE,
                              bandwidth = 9) {
    
    # Read spectral data from file
    data <- read_table(path)
    
    # Verify required columns exist and are numeric
    cols <- c('wavelength', 'substrate', 'product', 'buffer')
    for (col in cols) {
        if (! col %in% names(data)) 
            stop(paste('Column', col, 'is missing!'))
        if (! is.numeric(data[, col])) 
            stop(paste('Non-numeric values in', col, 'column!'))
    }
    
    # Calculate the geometric mean of the buffer for each wavelength
    NC <- aggregate(buffer ~ wavelength, data, geo_mean)
    
    # Reshape data excluding the buffer column to long format for 'substrate' and 'product'
    RFUdata <- melt(subset(data, select = -buffer), id.vars = 'wavelength',
                    variable.name = 'type', value.name = 'RFU')
    # Merge the baseline data with the reshaped RFU data
    RFUdata <- merge(RFUdata, NC, by = 'wavelength')
    # Subtract the baseline (buffer) from the RFU measurements
    RFUdata$RFU <- RFUdata$RFU - RFUdata$buffer
    
    # For the 'product' channel, aggregate the RFU using geometric mean by wavelength
    product <- aggregate(RFU ~ wavelength,  RFUdata, subset = type == 'product', geo_mean)
    # Identify the wavelength with maximum brightness (RFU)
    max_br <- product$wavelength[which(product$RFU == max(product$RFU))]
    
    # Begin constructing the plot based on log_scale option
    if (log_scale) {
        fig <-  ggplot(RFUdata, aes(wavelength, log10(RFU))) +
            labs(x  = 'Wavelenght, nm', y = expression('RFU, ' ~log[10]))
    } else {
        fig <- ggplot(RFUdata, aes(wavelength, RFU)) +
            labs(x  = 'Wavelenght, nm', y = 'RFU')
    }
    
    # Optionally, add a rectangular band around the max_br wavelength with specified bandwidth
    if (add_band) {
        fig <- fig +
            geom_rect(aes(xmin = max_br - bandwidth,
                          xmax = max_br + bandwidth,
                          ymin = -Inf, ymax = Inf),
                      fill = 'grey80', color = 'white',
                      alpha = 0.015)
    }
    
    # Add a summary line using the geometric mean, and mark the max_br with a red vertical line
    fig <- fig +
        stat_summary(fun = 'geo_mean', geom = 'line', aes(color = type), size = 1) +
        geom_vline(xintercept = max_br, size = 1, color = 'red') +
        scale_color_discrete(name = '', labels = c('Substrate', 'Product')) +
        plot_theme 
    
    # Optionally add data points to the plot
    if (show_points) {
        fig <- fig + geom_point(aes(color = type))
    }
    
    # Return the maximum brightness wavelength and the plot in a list
    return(list(max_br = max_br,
                figure = fig))
}


#' Save Spectrum Template
#'
#' Creates and saves a template data frame containing the required columns for spectrum analysis
#' and writes it to the specified file path.
#'
#' @param path Character. The file path where the template will be saved.
#'
#' @return None. The function writes a file to disk.

save_spectrum_template <- function(path) {
    # Create an empty data frame with required columns for spectrum analysis
    df <- data.frame(wavelength = '',
                     substrate = '',
                     product = '',
                     buffer = '')
    # Save the template to the given path without opening the file
    save_table(df, path, open_file = FALSE)
}


#' Generate Substrate Titration Sequence
#'
#' Computes a sequence of substrate concentrations by performing successive dilutions and rounding
#' the values to three significant digits.
#'
#' @param start Numeric. The starting substrate concentration.
#' @param step Numeric. The dilution factor.
#' @param n_steps Integer. The number of titration steps. Default is 8.
#'
#' @return A numeric vector of substrate concentrations.

substrate_titration <- function(start, step, n_steps = 8) 
    signif(start/step^(0:(n_steps - 1)), 3)


#' Display Modelling Plot for Enzyme Kinetics
#'
#' Simulates enzyme kinetics data using the Michaelis-Menten model by generating substrate titration data,
#' adding random error, fitting a nonlinear least squares model, and plotting the resulting curve. If a second set
#' of kinetic parameters is provided, the function compares the reference sample to a test sample.
#'
#' @param start_conc Numeric. The starting concentration for substrate titration.
#' @param titr_step Numeric. The dilution factor for titration.
#' @param n_steps Integer. The number of titration steps. Default is 8.
#' @param Km1 Numeric. The Michaelis constant for the reference sample.
#' @param Vmax1 Numeric. The maximum velocity for the reference sample.
#' @param Km2 Numeric. The Michaelis constant for the test sample (optional, NA if not provided).
#' @param Vmax2 Numeric. The maximum velocity for the test sample (optional, NA if not provided).
#' @param error_sd Numeric. The standard deviation of the random error added to the simulated data. Default is 0.0001.
#'
#' @return A ggplot object representing the Michaelis-Menten plot.

show_modelling_plot <- function(start_conc, titr_step, n_steps = 8, 
                                Km1, Vmax1,
                                Km2 = NA, Vmax2 = NA,
                                error_sd = 0.0001) {
    # Define an inner function for calculating reaction velocity using Michaelis-Menten kinetics
    velocity <- function(substrate_conc, Km, Vmax) {
        (Vmax * substrate_conc) / (Km + substrate_conc)
    }
    
    # Check for valid input for titration parameters; return NULL if any are missing
    if (is.na(start_conc) | is.na(titr_step) | is.na(n_steps)) return(NULL)
    
    # Generate substrate concentrations for the titration
    s_conc <- substrate_titration(start_conc, titr_step, n_steps)
    
    # Ensure valid enzyme kinetics parameters for the reference sample
    if (is.na(Km1) | is.na(Vmax1)) return(NULL)
    # Simulate velocity data for the reference sample by adding normally distributed error
    velo_data <- 
        data.frame(substrate_conc = s_conc,
                   b = velocity(s_conc, Km = Km1, Vmax = Vmax1) + rnorm(n_steps, mean = 0, sd = error_sd))
    # Fit a nonlinear least squares model to the simulated data
    model <- get_nls(velo_data, vmax = Vmax1, km = Km1)
    
    # If no second set of parameters is provided, plot the Michaelis-Menten curve for the reference sample
    if (is.na(Km2) | is.na(Vmax2)) {
        plot <- show_mm_plot(data = velo_data, model = model)
    } else {
        # Otherwise, simulate data for a test sample with a different set of parameters
        velo_data2 <- 
            data.frame(substrate_conc = s_conc,
                       b = velocity(s_conc, Km = Km2, Vmax = Vmax2) + rnorm(n_steps, mean = 0, sd = error_sd))
        # Fit a model to the test sample data
        model2 <- get_nls(velo_data2, vmax = Vmax2, km = Km2)
        # Label the datasets for clarity in the plot
        velo_data$name <- 'Reference'
        velo_data2$name <- 'Test Sample'
        # Combine the two datasets for comparative plotting
        vd <- rbind(velo_data, velo_data2)
        # Plot the Michaelis-Menten curves for both the reference and test sample
        plot <- show_mm_plots_layout(velocity_data = vd,
                                     nls_models = list('Reference' = model,
                                                       'Test Sample' = model2))
    }
    
    # Return the resulting plot
    plot
}
