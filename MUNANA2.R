

library(ggplot2)
library(stringr)


######## READING AND WRITING TABLES ########


# Converts time to numeric vector representing time in minutes.
# x : numeric vector, time values in POSIXct format;
# format : character of length 1, time format of x vector:
#       'excel' : MS Excel format where time is represented as a fraction of a day,
#       'sec'   : time in seconds,
#       'min'   : time in minutes,
#       'hour'  : time in hours;
# return : numeric vector, time points of measurements.

time_to_min <- function(x, format = 'excel') {
    switch (format,
            'excel' = return(round(x * 1440, 3)),
            'sec' = return(x / 60),
            'min' = return(x),
            'hour' = return(x * 60)
    )
}


# Generates well names of 96-well plate.
# columns : integer vector, column numbers of wells to generate;
# return : character vector, well names.

generate_well_names <- function(columns = 1:12) {
    if (! is.integer(columns)) columns <- as.integer(columns)
    if (min(columns) < 1) stop('Column number cannot be less than 1')
    if (max(columns) > 12) stop('Column number cannot be larger than 12!')
    nums <- unique(columns)
    lets <- LETTERS[1:8]
    paste0(lets, rep(nums, each = 8))
}


# Reads .csv or .xlsx files. Automatically determines file format by extention.
# Reads MS Excel csv dialect.
# path: character, path to the file to read;
# return : data.frame, table content.

read_table <- function(path) {
    if (!file.exists(path)) stop('File not found!')
        if (endsWith(path, '.csv')) {
            df <- read.csv(path, sep = ';', dec = ',')
        } else if(endsWith(path, '.xlsx'))
            df <- openxlsx::read.xlsx(path)
        else {
            stop('Check file format!')
        }
    df
}


# Reads RFU data from a .csv or .xlsx file.
# Reading table should contain
#       - Time column (from capital T),
#       - columns with RFU data titled by the name of corresponding wells.
# The function checks the the following criteria of the file:
#       - the presence of Time column;
#       - the presence of at least on well column.
# Any non-numeric values in well columns are deprecated. Corresponding message
# is sent to console.
# path : character, path to the file;
# time_format: character, see description of time_to_min function.
# return : data.frame, long-format table with 3 columns:
#       - well, well name;
#       - Time, in min;
#       - RFU, numeric.

read_RFU_data_table <- function(path, time_format = 'excel') {
    # Required parameters:
    wells <- generate_well_names()
    # Loading table
    df <- read_table(path)
    if (! 'Time' %in% names(df)) stop('Time column missing!')
    # Selecting used wells
    nrows <- dim(df)[1]
    wells_used <- character(0)
    for (w in wells) {
        if (w %in% names(df)) {
            if (!all(is.na(df[, w]))) {
                wells_used <- append(wells_used, w)
            }
        }
    }
    if (length(wells_used) == 0) stop('No well with RFU data found!')
    # Melting df
    molten_df <- reshape2::melt(df[, c('Time', wells_used)],
                                id.vars = c('Time'),
                                variable.name = 'well',
                                value.name = 'RFU')
    # Time formatting
    molten_df$Time <- time_to_min(molten_df$Time, time_format)
    # Removing non-numeric values
    nrows <- dim(molten_df)[1]
    RFU <- numeric(nrows)
    for (i in 1:nrows) {
        val <- molten_df$RFU[i]
        new_val <- tryCatch(as.numeric(val),
                            warning = function(w) {
                                time <- as.POSIXct(molten_df$Time[i]*60,
                                                   origin = '2021-01-01 0:00:00',
                                                   tz = 'UTC')
                                time_str <- strftime(time,
                                                     format = '%H:%M:%S',
                                                     tz = 'UTC')
                                mess <- paste('Non-numeric value in well',
                                              molten_df$well[i], 'at',
                                              time_str, '\n')
                                cat(mess)
                                return(NA)
                            })
        RFU[i] <- new_val
    }
    molten_df$RFU <- RFU
    molten_df[order(molten_df$well, molten_df$Time), c('well', 'Time', 'RFU')]
}


# Reads a .csv or .xlsx file with standard data.
# Reading table should contain:
#       - well column, containing well names (like A1, B2, etc.);
#       - type column, containing type of sample present in wells,
#         should be either 'standard' for wells with known reaction product concentration,
#         or 'NC' for wells containing working buffer only;
#       - conc column, containing concentration of standard (in uM) in corresponding wells.
# Any other column depricated.
# The function checks the the following criteria of the file:
#       - the presence of all required columns;
#       - correctness of well names;
#       - correctness of type column values, should be at least one NC well;
#       - correctness of conc column filling: the presence of numeric values differing from 0.
# path : character, path to the file;
# return : data.frame, long-format table with 3 columns:
#       - well, well name;
#       - type, sample types (see above);
#       - conc, standard concentrations (in uM).

read_standard_table <- function(path) {
    # Required parameters:
    col_names_ref <- c('well', 'type', 'conc')
    type_value_ref <- c('standard', 'NC')
    well_names_ref <- generate_well_names()
    # Loading table
    df <- read_table(path)
    # Cheking column names
    col_names <- names(df)
    for (cnr in col_names_ref) {
        if (! cnr %in% col_names_ref) stop(paste('Column', cnr, 'missing!'))
    }
    # Checking well names
    well_names <- unique(df$well)
    for (wn in well_names) {
        if (! wn %in% well_names_ref) stop(paste('Incorrect well name:', wn))
    }
    # Checking type column
    for (tv in unique(df$type)) {
        if (! tv %in% type_value_ref) stop(paste('Incorrect value in type column:', tv))
    }
    
    # Checking conc column
    if (! is.numeric(df$conc)) stop('Conc column contains non-numeric values')
    if (all(df$conc == 0)) stop('Conc column is empty!')
    if (all(is.na(df$conc))) stop('Conc column is empty!')
    df[, col_names_ref]
}


# Reads a .csv or .xlsx file with sample data.
# Reading table should contain:
#       - well column, containing well names (like A1, B2, etc.);
#       - name column, sample name; should be identical for wells representing one sample;;
#       - substrate_conc column, containing concentration of standard (in uM) in corresponding wells.
# Any other column depricated.
# The function checks the the following criteria of the file:
#       - the presence of all required columns;
#       - correctness of well names;
#       - correctness of substrate+conc column filling: the presence of numeric values differing from 0.
# path : character, path to the file;
# return : data.frame, long-format table with 3 columns:
#       - well, well name;
#       - name, sample names (see above);
#       - substrate_conc, standard concentrations (in uM).

read_sample_table <- function(path) {
    # Required parameters:
    col_names_ref <- c('well', 'name', 'substrate_conc')
    well_names_ref <- generate_well_names()
    # Loading table
    df <- read_table(path)
    # Checking well names
    well_names <- unique(df$well)
    for (wn in well_names) {
        if (! wn %in% well_names_ref) stop(paste('Incorrect well name:', wn))
    }
    # Checking substrate_conc column
    if (! is.numeric(df$substrate_conc)) stop('substrate_conc column contains non-numeric values')
    if (all(df$substrate_conc == 0)) stop('substrate_conc column is empty!')
    if (all(is.na(df$substrate_conc))) stop('substrate_conc column is empty!')
    df[, col_names_ref]
}


# Saves a table into xlsx file.
# x : data.frame, tabe to save;
# path : character, a path to a new file;
# open_file : logical, should the file to be open automatically after saving.
# return : NULL

save_table <- function(x, path, open_file = TRUE) {
    xlsx::write.xlsx(x, path, row.names = FALSE)
    if (open_file) shell.exec(tools::file_path_as_absolute(path))
}


# Saves a template table describing standard positions in a 96-well plate and product concentrations.
# path : character, a path to a new file;
# columns : integer, a vector containing columns number used for standard (ranges from 1 to 12);
# open_file : logical, should the file to be open automatically after saving.
# return : NULL

save_standard_template <- function(path, columns, open_file = TRUE) {
    len <- length(columns)
    if (len == 0 | len > 11) stop('Incorrect columns value!')
    df <- data.frame(well = generate_well_names(columns),
                     type = rep(c(rep('standard', times = 7), 'NC'), times = len),
                     conc = 0)
    save_table(df, path, open_file)
}


# Saves a template table describing sample positions in a 96-well plate.
# path : character, a path to a new file;
# columns : integer, a vector containing columns number used for standard (ranges from 1 to 12);
# open_file : logical, should the file to be open automatically after saving.
# return : NULL

save_sample_template <- function(path, columns, open_file = TRUE) {
    len <- length(columns)
    if (len == 0 | len > 11) stop('Incorrect columns value!')
    df <- data.frame(well = generate_well_names(columns),
                     name = '',
                     substrate_conc = 100)
    save_table(df, path, open_file)
}


# Saves a template table for optical data obtained in an experiment.
# path : character, a path to a new file;
# columns : integer, a vector containing columns number used for standard (ranges from 1 to 12);
# open_file : logical, should the file to be open automatically after saving.
# return : NULL

save_RFU_template <- function(path, columns = 1:12, open_file = TRUE) {
    len <- length(columns)
    if (len == 0 | len > 12) stop('Incorrect columns value!')
    col_names <- c('Time', generate_well_names(columns))
    df <- data.frame(matrix('', ncol = 8*len + 1, nrow = 1))
    colnames(df) <- col_names
    save_table(df, path, open_file)
}


######## CALCULATION OF LINEAR MODELS ########

# Calculates geometic mean.
# x : numeric vector, values to calculate geometric mean
# return : numeric, geometric mean.

geo_mean <- function(x) 10^mean(log10(x), na.rm = TRUE)


# Returns a data.frame containing background signal at each time point
# of the assay.
# std_data : data.frame containing standard data (see read_standard_table function);
# RFU_data : data.frame containing RFU data.
# return : data.frame, table containig two columns:
#       - Time, time since the begining of the assay, in min;
#       - RFU, value of background signal.

get_NC_data <- function(std_data, RFU_data) {
    NC <- subset(std_data, type == 'NC')
    merged <- merge(NC, RFU_data)[, c('Time', 'RFU')]
    result <- aggregate(RFU ~ Time, merged, geo_mean)
    result[order(result$Time), c('Time', 'RFU')]
}


# Returns linear models to calibrate the assay at every time point.
# std_data : data.frame containing standard data (see read_standard_table function);
# RFU_data : data.frame containing RFU data  (see read_RFU_data_table() function);
# method : character, method of data transformation prior calibration:
#       - 'linear', no transformation is applied;
#       - 'log-linear', dependent and independent variables are log-transformed
#          before caclulation of linear regression.
# return : list, processed standard data; 
#       $method, method of data transformation;
#       $std_invdividual_data, data.frame containing optical from individual wells,
#       $std_averaged_data, data.frame containing geometrical means of signals obtained from
#        technical replicas of the standard;
#       $models, list of lm objects, names of the list correspond to time points of the assay.

get_calibration <- function(std_data, RFU_data, method = 'log-linear') {
    get_loglinear_model <- function(time_point) {
        lm(log10(conc) ~ log10(RFU), std_aggr, subset = Time == time_point)
    }
    get_linear_model <- function(time_point) {
        lm(conc ~ RFU, std_aggr, subset = Time == time_point)
    }
    NC <- get_NC_data(std_data, RFU_data)
    std <- merge(subset(std_data, type == 'standard', select = c(well, conc)),
                 RFU_data, by = 'well')
    std <- merge(std, NC, by = 'Time', suffixes = c('', '.NC'))
    std$RFU <- std$RFU - std$RFU.NC
    std_aggr <- aggregate(RFU ~ conc + Time, std, geo_mean)
    time_points <- sort(unique(NC$Time))
    models <- switch(method,
        'log-linear' = lapply(time_points, get_loglinear_model),
        'linear' = lapply(time_points, get_linear_model),
        stop(paste('Wrong method value:', method)))
    names(models) <- time_points
    list(method = method,
         std_individual_data = std[, c('Time', 'well', 'conc', 'RFU')],
         std_averaged_data = std_aggr,
         models = models)
}


# Builds progress curves based on RFU data using calibrating models.
# sample_data : data.frame, containing sample description (see read_sample_data() function);
# RFU_data : data.frame containing RFU data (see read_RFU_data_table() function);
# NC_data : data.frame containing background values for each time point (see
#           get_NC_data() function);
# calib : list with calibrating lm models (see get_calibration() function).
# Technical repliations are averaged by applying geo_mean() function.
# returns : data.frame containing following columns:
#       - Time, time since the beginning of the assay in minutes;
#       - well, well names;
#       - name, names of samples;
#       - substrate_conc, concentration of the substrate (in uM);
#       - conc, calculated concentration of reaction product in uM.

get_progress_curves <- function(sample_data, RFU_data, NC_data, calib) {
    samples <- merge(sample_data, RFU_data, by = 'well')
    samples <- merge(samples, NC_data, by = 'Time', suffixes = c('', '.NC'))
    samples$RFU <- samples$RFU - samples$RFU.NC
    df <- data.frame()
    for (tp in names(calib$models)) {
        smpl_tp <- subset(samples, Time == as.numeric(tp))
        smpl_tp$conc <- switch (calib$method,
            'log-linear' = 10^predict(calib$models[[tp]], smpl_tp),
            'linear' = predict(calib$models[[tp]], smpl_tp))
        df <- rbind(df, smpl_tp)
    }
    df[, c('Time', 'well', 'name', 'substrate_conc', 'RFU', 'conc')]
}


# Replaceces conc column in progress curved data.frame
# by corrected concentration values.
# progress_data : data.frame, containing progress curves data;
# the data.frame should contain column substrate_conc;
# bright : numeric, fold difference in brightness between the substrate and
#          the product of the reaction.
# return : data.frame, containing corrected values in conc column.

correct_conc <- function(progress_data, bright) {
    progress_data$conc <- with(progress_data, (conc * bright - substrate_conc) / (bright - 1))
    progress_data
}


# Returns key parameters of linear regression model:
# intercept, slope, and R squared.
# model : lm object, linear model;
# return : data.frame, linear regression model parameters.

get_slope_int_Rsq <- function(model) {
    inter <- coef(model)[1]
    slope <- coef(model)[2]
    Rsq <- summary(model)$r.squared
    data.frame(inter = inter,
               slope = slope,
               Rsq = Rsq)
}


# Returns key parameters of linear or quadric regression model:
# a, b, and c (coefficents of quadric and linear components,
# and intercept, respectively).
# model : lm object, quadric model.
# return : data.frame, quadric regression model parameters.

get_abc_Rsq <- function(model) {
    coefs <- coef(model)
    if (length(coefs) == 3) {
        a <- coefs[2]
        b <- coefs[3]
        c <- coefs[1]
    } else {
        a <- NA
        b <- coefs[2]
        c <- coefs[1]
    }
    Rsq <- summary(model)$r.squared
    data.frame(a = a,
               b = b,
               c = c,
               Rsq = Rsq)
}



# Calculates progress curves based on raw data provided as .csv or .xlsx tables.
# standard_table : character, path to a file containing standard table;
# sample_table : character, path to a file containing description of samples;
# RFU_table : character, path to a file with optical data;
# time_format : character, time format used in file with optical data;
#       'excel' : MS Excel format where time is represented as a fraction of a day,
#       'sec'   : time in seconds,
#       'min'   : time in minutes,
#       'hour'  : time in hours;
# calibration_method : chatacter, method of data transformation prior calibration:
#       - 'linear', no transformation is applied;
#       - 'log-linear', dependent and independent variables are log-transformed
#          before caclulation of linear regression;
# bright : numeric, fold difference in brightness between the substrate and
#          the product of the reaction; if NULL, no correction is applied to the data.
# return : list, containing assay data:
# $NC_data : data.frame, background levels throughout the assay:
#            Time, time point of the measurement in minuntes,
#            RFU, level of blank signal.
# $standard_data_indiv : data.frame, calibration data from individual wells:
#            Time, time point of the measurement in minuntes;
#            well, well name;
#            conc, concentration of the standard in individual wells;
#            RFU, optical data;
# $standard_data_averaged : data.frame, similar to the previuos data.frame,
#            except geometrical means are provided form technical replicas of the standard;
# $calibration_pars : data.frame, containing linear regression parameters (intercept, slope, and
#            R squared) of standard titration for each time point (provided as row.names);
# $progress_curves : data.frame containing progress curves data:
#            Time, time point of the measurement in minuntes;
#            well, well names;
#            name, sample names;
#            substrate_conc, concentration of substrate (in uM);
#            RFU, optical data from samples;
#            conc, product concentration (in uM).

assay <- function(standard_data,
                  sample_data,
                  RFU_data,
                  calibration_method = 'log-linear',
                  bright = NULL) {
    NC_data <- get_NC_data(std_data = standard_data,
                           RFU_data = RFU_data)
    calibration <- get_calibration(std_data = standard_data,
                                   RFU_data = RFU_data,
                                   method = calibration_method)
    model_pars <- do.call(rbind, lapply(calibration$models, get_slope_int_Rsq))
    progress_curves <- get_progress_curves(sample_data = sample_data,
                                           RFU_data =  RFU_data,
                                           NC_data = NC_data,
                                           calib = calibration)
    if (!is.null(bright)) {
        progress_curves <- correct_conc(progress_curves, bright)
    }
    list(NC_data = NC_data,
         standard_data_indiv = calibration$std_individual_data,
         standard_data_averaged = calibration$std_averaged_data,
         calibration_pars = model_pars,
         progress_curves = progress_curves)
}


# Fits quadratic regression models to progress curve data.
# assay : list containing progress_curve data (returned by assay()).
# return : data.frame containing model's coefficients and R squared.

model_progress_curves <- function(assay) {
    df <- dplyr::distinct(assay$progress_curves, well, name, substrate_conc)
    result <- data.frame()
    for (i in 1:dim(df)[1]) {
        sub_data <- subset(assay$progress_curves,
                           name == df$name[i] & substrate_conc == df$substrate_conc[i] & well == df$well[i])
        fit <- lm(conc ~ I(Time^2) + Time, sub_data)
        fit_summary <- summary(fit)$coefficients
        a_coef <- fit_summary[2, 1]
        a_pr_value <- fit_summary[2, 4]
        if (a_coef > 0 & a_pr_value < 0.01) {
            fit <- lm(conc ~ Time, sub_data)
        }
        pars <- cbind(df[i, ], get_abc_Rsq(fit))
        result <- rbind(result, pars)
    }
    result[order(result$name, result$substrate_conc, decreasing = c(F, T), method = 'radix'),]
}


# Makes initial guess of vmax value based on provided velocity data.
# velocity_data : data.frame containing coefficients from quadratic regression model;
# vmax : initial guess of vmax, if NULL returns the highest velocity as the initial guess.

guess_vmax <- function(velocity_data, vmax = NULL) {
    if (!is.null(vmax)) return(vmax)
    max(velocity_data$b, na.rm = TRUE)
}


# Makes initial guess of Km value based on provided velocity data.
# velocity_data : data.frame containing coefficients from quadratic regression model;
# km : initial guess of Km, if NULL returns the geometrical mean of substrate range.

guess_km <- function(velocity_data, km = NULL) {
    if (!is.null(km)) return(km)
    min_max <- range(velocity_data$substrate_conc)
    geo_mean(min_max)
}


# Fits a nonlinear least squares model to velocity data returned by model_progress_curves() function
# using Michaelis-Menten equation.
# velocity_data : data.frame containing coefficients from quadratic regression model;
# vmax : numeric, approximate Vmax value;
# km : numeric, approximate Km value.
# return : nls models or NULL if velocity_data has less then 3 rows.

get_nls <- function(velocity_data, vmax = NULL, km = NULL) {
    if (dim(velocity_data)[1] < 3) return(NULL)
    vmax = guess_vmax(velocity_data, vmax)
    km = guess_km(velocity_data, km)
    nls(b ~ Vmax * substrate_conc / (Km + substrate_conc),
        start = c(Vmax = vmax, Km = km),
        data = velocity_data, trace = F)
}




######## DATA VISUALISATION ########

# Plot attributes used in the application.

plot_theme <-
    theme_bw() +
    theme(strip.text = element_text(size = 16, face = "plain"),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 14),
          legend.key.size = unit(1, "line"),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 14, color = "black"),
          panel.grid = element_blank(),
          strip.background = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(colour = "black"))


# Makes a plot showing signals obtained from calibrating titration during the experiment.
# assay : list returned by the assay() function;
# mode : character, takes two values: 'individual' (shows data from each well) or 'averaged'
#        (shows averaged data for each concentration level of the calibrating reagent).
# return : ggplot object.

show_standard_data <- function(assay, mode = 'individual') {
    conc_levels <- as.character(sort(unique(assay$standard_data_indiv$conc), decreasing = T))
    plot <- switch (mode,
                    'individual' = ggplot(assay$standard_data_indiv,
                                          aes(Time, RFU, group = well)) +
                        geom_line(aes(color = factor(conc, levels = conc_levels))),
                    'averaged' = ggplot(assay$standard_data_averaged,
                                        aes(Time, RFU)) +
                        geom_line(aes(color = factor(conc, levels = conc_levels))),
                    stop(paste('Wrong value of mode argument:', mode))
    )
    plot <- plot +
        scale_y_log10() +
        scale_color_discrete(name = expression('Product\nconcentration, '*mu*'M')) +
        labs(x = 'Time, min', y = 'RFU') +
        plot_theme
    plot
}


# Makes a plot showing how regression parameters of calibrating curve changed during the experiment.
# assay : list returned by the assay() function;
# parmeter : character, a regression parameter to plot; takes three possible values:
#            'slope', 'intercept', or 'Rsq';
# y_limits : numeric vector of length 2 describing limits of the Y axis. If NA values provided,
#            these values will be picked up automatically.
# return : ggplot object.

show_calibration_pars <- function(assay, parameter, y_limits = c(NA, NA)) {
    calib <- assay$calibration_pars
    calib$time <- as.numeric(row.names(calib))
    plot <- switch(parameter,
                   'slope' = ggplot(calib, aes(time, slope)) +
                       labs(x = 'Time, min', y = 'Slope'),
                   'intercept' = ggplot(calib, aes(time, inter)) +
                       labs(x = 'Time, min', y = 'Intercept'),
                   'Rsq' = ggplot(calib, aes(time, Rsq)) +
                       labs(x = 'Time, min', y = expression(R^2)),
                   stop(paste('Wrong parameter value:', parameter)))
    plot <- plot + geom_line() + 
        scale_y_continuous(limits = y_limits) +
        plot_theme
    plot
}


# Makes a plot showing progress curves.
# assay : list returned by the assay() function;
# mode : character, the mode how progress curves to be shown; takes one of three values:
#        'all' - all curves in one plot;
#        'by sample' - all curves for each sample on one plot;
#        'by sample & conc' - progress curves from each sample from each substrate concentration 
#                on a separate plot;
# show_velocities : logical, if initial reaction velocities to be shown
#                   by dashed lines on the plots;
# curve_models : data.frame returned by the model_progress_curves() function.
# return : list of ggplot objects.

show_progress_curves <- function(assay,
                                 mode = 'all',
                                 show_velocities = FALSE,
                                 curve_models = NULL) {
    substr_conc_levels <- sort(unique(assay$progress_curves$substrate_conc), decreasing = T)
    samples <- unique(assay$progress_curves$name)
    plots <- list()
    if (mode == 'all') {
        plot <- ggplot(assay$progress_curves, aes(Time, conc, group = well))
        plots[['plot_1']] <- plot
    } else if (mode == 'by sample') {
        for (smpl in samples) {
            df <- subset(assay$progress_curves, name == smpl)
            plot <- ggplot(df, aes(Time, conc, group = well)) +
                facet_wrap(~ name)
            plots[[paste0('plot_', smpl)]] <- plot
        }
    } else if (mode == 'by sample & conc') {
        vars <- expand.grid(samples, substr_conc_levels)
        for (i in 1:dim(vars)[1]) {
            df <- subset(assay$progress_curves, name == vars[i,1] & substrate_conc == vars[i,2])
            if (dim(df)[1] > 0) {
                plot <- ggplot(df, aes(Time, conc, group = well)) +
                    facet_wrap(~ paste(name, substrate_conc, sep = ': '))
                plots[[paste0('plot_', i)]] <- plot
            }
        }
    } else {
        stop(paste('Wrong value of mode:', mode))
    }
    for (i in 1:length(plots)) {
        if (mode != 'by sample & conc') {
            plots[[i]] <- plots[[i]] +
                geom_line(aes(color = factor(substrate_conc, levels = substr_conc_levels)))
        } else {
            plots[[i]] <- plots[[i]] +
                geom_line()
        }
        plots[[i]] <- plots[[i]] +
            scale_color_discrete(name = expression('Substrate\nconcentration, '*mu*'M')) +
            plot_theme +
            labs(x = 'Time, min', y = expression('Product concentration, '*mu*'M'))
    }
    # addition velocities:
    if (show_velocities & ! is.null(curve_models)) {
        curve_models$substrate_conc <- factor(curve_models$substrate_conc, levels = substr_conc_levels)
        if (mode == 'all') {
            plots[[1]] <- plots[[1]] +
                geom_abline(data = curve_models, aes(slope = b, intercept = c,
                                                     color = substrate_conc,
                                                     group = well),
                            linetype = 2)
        } else if (mode == 'by sample') {
            for (smpl in samples) {
                plot_name <- paste0('plot_', smpl)
                plots[[plot_name]] <- plots[[plot_name]] +
                    geom_abline(data = subset(curve_models, name == smpl),
                                aes(slope = b, intercept = c,
                                    color = substrate_conc,
                                    group = well),
                                linetype = 2)
            }
        } else if (mode == 'by sample & conc') {
            for (nm in names(plots)) {
                i <- as.numeric(str_split(nm, '_')[[1]][2])
                smpl_name = vars[i, 1]
                conc <- vars[i, 2]
                plots[[nm]] <- plots[[nm]] +
                    geom_abline(data = subset(curve_models, name == smpl_name & substrate_conc == conc),
                                aes(slope = b, intercept = c,
                                    group = well),
                                linetype = 2)
            }
        }
    }
    plots
}


# Calculates Michaelis-Mentent curves based on nls model provided.
# model : nls model;
# max_conc : numeric, maximal substrate concentration to put into a model.
# return : data.frame containing two columns: 'substrate_conc' (substrate concentration)
#          and 'velocity' (reaction velocity at current substrate concentration).

get_mm_model <- function(model, max_conc) {
    df <- data.frame(substrate_conc = seq(0, max_conc, by = 0.5))
    df$velocity <- predict(model, df)
    df
}


# Makes a Michaelis-Mentent plot.
# data : data.frame containing reaction velocity data (comes from the model_progress_curves() function);
# model : nls model to be shown; if NULL, no moedel appears, only individual data points.
# return : ggplot object.

show_mm_plot <- function(data, model = NULL) {
    if (dim(data)[1] == 0) return(NULL)
    fig <- ggplot(data, aes(substrate_conc, b)) +
        geom_point(size = 2) +
        scale_y_continuous(limits = c(0, NA)) +
        plot_theme +
        labs(x = expression('Substrate concentration, '*mu*'M'),
             y = expression('Velocity, '*mu*'M/min'))
    if (is.null(model)) return(fig)
    max_conc = max(data$substrate_conc)
    df <- get_mm_model(model, max_conc)
    X <- coef(model)
    horiz_line <- data.frame(x = c(-Inf, X[2]),
                             y = c(X[1]/2, X[1]/2))
    vert_line <- data.frame(x = c(X[2], X[2]),
                            y = c(-Inf, X[1]/2))
    fig + geom_line(data = df, aes(substrate_conc, velocity)) +
        geom_point(x = X[2], y = X[1]/2, color = 'red', size = 2) +
        geom_line(data = horiz_line, aes(x = x, y = y), color = 'red', linetype = 2) +
        geom_line(data = vert_line, aes(x = x, y = y), color = 'red', linetype = 2)
}


# Shows several Michaelis-Mentent curves in one plot.
# velocity_data : data.frame containing reaction velocity data (comes from the model_progress_curves() function);
# model : list of nls model to be show;
# samples_include : charcter vector, a list of samples to show; if NULL, all curves will be shown.
# return : ggplot object.

show_mm_plots_layout <- function(velocity_data,
                                 nls_models,
                                 samples_include = NULL) {
    if (!is.null(samples_include)) {
        velocity_data <- subset(velocity_data, name %in% samples_include)
        smpl_list <- samples_include
    } else {
        smpl_list <- names(nls_models)
    }
    predicted_by_models <- data.frame()
    for (smpl in smpl_list) {
        model <- nls_models[[smpl]]
        if (!is.null(model)) {
            max_conc <- max(velocity_data$substrate_conc[velocity_data$name == smpl])
            df <- get_mm_model(model, max_conc)
            df$name <- smpl
            predicted_by_models <- rbind(predicted_by_models, df)
        }
    }
    fig <- ggplot(velocity_data, aes(substrate_conc, b)) +
        geom_point(aes(color = name), size = 2) +
        plot_theme +
        labs(x = expression('Substrate concentration, '*mu*'M'),
             y = expression('Velocity, '*mu*'M/min'))
    if (dim(predicted_by_models)[1] > 0) {
        fig <- fig + geom_line(data = predicted_by_models,
                               aes(x = substrate_conc,
                                   y = velocity,
                                   color = name))
    }
    fig <- fig + scale_color_discrete(name = 'Sample')
    fig
}


# Makes a scatter plot with Km and Vmax values of samples provided.
# data : data.frame containing Km and Vmax data to be shown.
# return : ggplot object.

show_km_vmax <- function(data, show_error_bars = TRUE) {
    plot <- ggplot(data, aes(Km, Vmax, color = sample)) +
        geom_point(size = 3) +
        scale_color_discrete(name = 'Sample') +
        plot_theme +
        labs(x = expression('Substrate concentration, '*mu*'M'),
             y = expression('Vmax, '*mu*'M/min'))
    if (show_error_bars) plot <- plot +
            geom_errorbarh(aes(xmin = Km_lower, xmax = Km_upper)) +
            geom_errorbar(aes(ymin = Vmax_lower, ymax = Vmax_upper)) 
    plot
}


##################

# setwd('D:/Google Drive/Gotheburg University/Diary/!Projects/14. MUNANA app/')
# 
# std_data <- read_standard_table('./2021-06-20_standard_table.xlsx')
# smpl_data <- read_sample_table('./2021-06-20_sample_table.xlsx')
# rfu_data <- read_RFU_data_table('./2021-06-20 MUNANA.xlsx')
# 
# 
# test <- assay(standard_data = std_data,
#               sample_data = smpl_data,
#               RFU_data = rfu_data,
#               calibration_method = 'log-linear',
#               bright = 217)
# 
# 
# show_standard_data(test, mode = 'individual')
# standard_plot <- show_standard_data(test, mode = 'averaged')
# 
# 
# 
# slope_plot <- show_calibration_pars(test, parameter = 'slope', y_limits = c(1, 1.1))
# int_plot <- show_calibration_pars(test, parameter = 'intercept', y_limits = c(-7, 8))
# rsq_plot <- show_calibration_pars(test, parameter = 'Rsq', y_limits = c(0.995, 1.0))
# 
# 
# test$progress_curves
# 
# show_progress_curves(test, mode = 'all')
# plots <- show_progress_curves(test, mode = 'by sample & conc' )
# plots[[1]]
# plots[[2]]
# 
# velo_data_table <- model_progress_curves(test)
# 
# show_progress_curves(test, mode = 'all', show_velocities = T, curve_models = models)
# progress_curves <- show_progress_curves(test, mode = 'by sample', show_velocities = T, curve_models = models)
# ## plots2 <- show_progress_curves(test, mode = 'by sample & conc', show_velocities = T, curve_models = models)
# 
# models
# 
# nls_model <- get_nls(models[models$name == 'PR8 virus',])
# nls_model
# 
# show_mm_plot(models[models$name == 'PR8 virus',])
# show_mm_plot(models[models$name == 'PR8 virus',], nls_model)
