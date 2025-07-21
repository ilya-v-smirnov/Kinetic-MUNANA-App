
library(ggplot2)
library(stringr)

####################### READING AND WRITING TABLES #############################

#' Convert Time Values to Minutes
#'
#' This function converts a numeric time vector into minutes. The input time can be
#' in one of several formats:
#'   - "excel": Time is represented as a fraction of a day (typical in MS Excel)
#'   - "sec": Time is given in seconds
#'   - "min": Time is already in minutes
#'   - "hour": Time is given in hours
#'
#' @param x Numeric vector of time values.
#' @param format Character string of length 1 specifying the format of x.
#' @return Numeric vector where each element is the time in minutes.

time_to_min <- function(x, format = 'excel') {
    switch(format,
           'excel' = return(round(x * 1440, 3)),  # 1 day = 1440 minutes
           'sec'   = return(x / 60),
           'min'   = return(x),
           'hour'  = return(x * 60))
}


#' Generate 96-Well Plate Names
#'
#' This function returns a character vector of well names for a standard 96-well plate.
#' Each well is designated by a letter (rows A–H) and a column number.
#'
#' @param columns Integer vector indicating which columns to generate (default: 1:12).
#' @return Character vector of well names (e.g., "A1", "B1", …, "H12").

generate_well_names <- function(columns = 1:12) {
    # Ensure columns are integers
    if (! is.integer(columns)) columns <- as.integer(columns)
    if (min(columns) < 1) stop('Column number cannot be less than 1')
    if (max(columns) > 12) stop('Column number cannot be larger than 12!')
    nums <- unique(columns)
    lets <- LETTERS[1:8]
    paste0(lets, rep(nums, each = 8))
}


#' Read Table from File (CSV or XLSX)
#'
#' This function reads a table from a file, automatically detecting the file type by
#' its extension. For CSV files, it assumes the MS Excel dialect (semicolon-separated,
#' comma as decimal separator).
#'
#' @param path Character string representing the path to the file.
#' @return A data.frame with the contents of the file.

read_table <- function(path) {
    if (!file.exists(path)) stop('File not found!')
    if (endsWith(path, '.csv')) {
        df <- read.csv(path, sep = ';', dec = ',')
    } else if (endsWith(path, '.xlsx')) {
        df <- openxlsx::read.xlsx(path)
    } else {
        stop('Check file format!')
    }
    df
}


#' Read RFU Data from a Table File
#'
#' Reads RFU (Relative Fluorescence Units) data from a CSV or XLSX file. The file must
#' contain a "Time" column and one or more columns with well names (e.g., "A1", "B2", etc.).
#' The function validates the presence of these columns, melts the data into long format,
#' converts time to minutes, and filters out non-numeric RFU values.
#'
#' @param path Character string, path to the file.
#' @param time_format Character specifying the time format ("excel", "sec", "min", "hour").
#' @return A data.frame in long format with columns:
#'         - well: Well name.
#'         - Time: Time in minutes.
#'         - RFU: Numeric RFU value.

read_RFU_data_table <- function(path, time_format = 'excel') {
    # Define valid well names for a 96-well plate.
    wells <- generate_well_names()
    # Load data table from file.
    df <- read_table(path)
    if (! 'Time' %in% names(df)) stop('Time column missing!')
    
    # Identify well columns that contain data (non-NA values).
    wells_used <- character(0)
    for (w in wells) {
        if (w %in% names(df)) {
            if (!all(is.na(df[, w]))) {
                wells_used <- append(wells_used, w)
            }
        }
    }
    if (length(wells_used) == 0) stop('No well with RFU data found!')
    
    # Melt the data frame to long format.
    molten_df <- reshape2::melt(df[, c('Time', wells_used)],
                                id.vars = c('Time'),
                                variable.name = 'well',
                                value.name = 'RFU')
    # Convert time values to minutes.
    molten_df$Time <- time_to_min(molten_df$Time, time_format)
    
    # Convert RFU values to numeric, reporting any non-numeric entries.
    nrows <- nrow(molten_df)
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


#' Read Standard Data Table
#'
#' Reads a file containing standard data for calibration. The input table must include:
#'   - well: Well names (e.g., "A1", "B2").
#'   - type: Sample type, either "standard" (with known concentration) or "NC" (no
#'     concentration, blank).
#'   - conc: Numeric standard concentration (in µM).
#' The function validates column presence, well name correctness, and that at least one NC
#' well is present, along with valid numeric concentrations.
#'
#' @param path Character string, path to the file.
#' @return A data.frame with columns: well, type, and conc.

read_standard_table <- function(path) {
    # Reference column names, allowed type values, and valid well names.
    col_names_ref <- c('well', 'type', 'conc')
    type_value_ref <- c('standard', 'NC')
    well_names_ref <- generate_well_names()
    # Load table.
    df <- read_table(path)
    
    # Validate presence of required columns.
    for (cnr in col_names_ref) {
        if (! cnr %in% names(df)) stop(paste('Column', cnr, 'missing!'))
    }
    # Validate that all well names in the file are correct.
    well_names <- unique(df$well)
    for (wn in well_names) {
        if (! wn %in% well_names_ref) stop(paste('Incorrect well name:', wn))
    }
    # Validate that type column contains only permitted values.
    for (tv in unique(df$type)) {
        if (! tv %in% type_value_ref) stop(paste('Incorrect value in type column:', tv))
    }
    # Validate concentration column.
    if (! is.numeric(df$conc)) stop('Conc column contains non-numeric values')
    if (all(df$conc == 0)) stop('Conc column is empty!')
    if (all(is.na(df$conc))) stop('Conc column is empty!')
    df[, col_names_ref]
}


#' Read Sample Data Table
#'
#' Reads a file with sample information. The table must contain:
#'   - well: Well names.
#'   - name: Sample name (should be the same for replicate wells).
#'   - substrate_conc: Numeric substrate concentration (in µM).
#' The function validates well names and substrate concentration values.
#'
#' @param path Character string, path to the file.
#' @return A data.frame with columns: well, name, and substrate_conc.

read_sample_table <- function(path) {
    # Reference required column names and valid well names.
    col_names_ref <- c('well', 'name', 'substrate_conc')
    well_names_ref <- generate_well_names()
    # Load table.
    df <- read_table(path)
    
    # Validate well names.
    well_names <- unique(df$well)
    for (wn in well_names) {
        if (! wn %in% well_names_ref) stop(paste('Incorrect well name:', wn))
    }
    # Validate substrate concentration column.
    if (! is.numeric(df$substrate_conc)) stop('substrate_conc column contains non-numeric values')
    if (all(df$substrate_conc == 0)) stop('substrate_conc column is empty!')
    if (all(is.na(df$substrate_conc))) stop('substrate_conc column is empty!')
    
    # Ensure sample names are factors with levels in order of appearance.
    df$name <- factor(df$name, levels = unique(df$name))
    df[, col_names_ref]
}


#' Save a Data Table to an XLSX File
#'
#' Saves a given data.frame as an XLSX file at the specified path. Optionally, the file
#' can be opened immediately after saving.
#'
#' @param x Data.frame to be saved.
#' @param path Character string specifying the file path for the new XLSX file.
#' @param open_file Logical indicating whether to automatically open the file after saving.
#' @return NULL


save_table <- function(x, path, open_file = TRUE) {
    writexl::write_xlsx(x, path)
    if (open_file && interactive()) shell.exec(tools::file_path_as_absolute(path))
}


#' Save Standard Template Table for a 96-Well Plate
#'
#' Creates and saves a template XLSX file describing standard positions and product concentrations.
#'
#' @param path Character string, file path for the new template.
#' @param columns Integer vector of column numbers (1 to 12) used for standards.
#' @param open_file Logical indicating whether to automatically open the file after saving.
#' @return NULL

save_standard_template <- function(path, columns, open_file = TRUE) {
    len <- length(columns)
    if (len == 0 | len > 11) stop('Incorrect columns value!')
    df <- data.frame(well = generate_well_names(columns),
                     type = rep(c(rep('standard', times = 7), 'NC'), times = len),
                     conc = 0)
    save_table(df, path, open_file)
}


#' Save Sample Template Table for a 96-Well Plate
#'
#' Creates and saves a template XLSX file describing sample positions.
#'
#' @param path Character string, file path for the new template.
#' @param columns Integer vector of column numbers (1 to 12) used for samples.
#' @param open_file Logical indicating whether to automatically open the file after saving.
#' @return NULL

save_sample_template <- function(path, columns, open_file = TRUE) {
    len <- length(columns)
    if (len == 0 | len > 11) stop('Incorrect columns value!')
    df <- data.frame(well = generate_well_names(columns),
                     name = '',
                     substrate_conc = 100)
    save_table(df, path, open_file)
}


#' Save RFU Template Table for Optical Data
#'
#' Creates and saves a template XLSX file for optical data measured in an experiment.
#'
#' @param path Character string, file path for the new template.
#' @param columns Integer vector of column numbers to include (default: 1 to 12).
#' @param open_file Logical indicating whether to automatically open the file after saving.
#' @return NULL

save_RFU_template <- function(path, columns = 1:12, open_file = TRUE) {
    len <- length(columns)
    if (len == 0 | len > 12) stop('Incorrect columns value!')
    col_names <- c('Time', generate_well_names(columns))
    df <- data.frame(matrix('', ncol = 8 * len + 1, nrow = 1))
    colnames(df) <- col_names
    save_table(df, path, open_file)
}


################### CALCULATION OF LINEAR MODELS ###############################

#' Calculate the Geometric Mean
#'
#' Computes the geometric mean of a numeric vector using base-10 logarithms.
#'
#' @param x Numeric vector.
#' @return Numeric value representing the geometric mean.

geo_mean <- function(x) 10^mean(log10(x), na.rm = TRUE)


#' Extract Background (NC) Data
#'
#' Calculates the background signal at each time point based on NC (negative control)
#' standard wells. It merges standard and RFU data, computes the geometric mean per time point,
#' and returns an ordered data.frame.
#'
#' @param std_data Data.frame with standard data (see read_standard_table).
#' @param RFU_data Data.frame with RFU measurements.
#' @return Data.frame with columns:
#'         - Time: Measurement time (in minutes).
#'         - RFU: Background signal.

get_NC_data <- function(std_data, RFU_data) {
    NC <- subset(std_data, type == 'NC')
    merged <- merge(NC, RFU_data)[, c('Time', 'RFU')]
    result <- aggregate(RFU ~ Time, merged, geo_mean)
    result[order(result$Time), c('Time', 'RFU')]
}


#' Compute Calibration Models for the Assay
#'
#' This function calibrates the assay by fitting linear models at each time point. It first
#' extracts the background signal (NC), adjusts the RFU data by subtracting the background,
#' aggregates standard data using the geometric mean, and then fits either a log-linear or
#' linear model at each time point.
#'
#' @param std_data Data.frame with standard data (from read_standard_table).
#' @param RFU_data Data.frame with RFU measurements (from read_RFU_data_table).
#' @param method Character specifying the transformation method:
#'        "linear" (no transformation) or "log-linear" (log10-transformation of both variables).
#' @return A list containing:
#'         - method: The method used.
#'         - std_individual_data: Raw data from individual wells.
#'         - std_averaged_data: Geometric means for technical replicates.
#'         - models: A named list of lm objects for each time point.

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


#' Generate Progress Curves Based on Calibration Models
#'
#' Constructs progress curves by applying calibration models to sample data. It merges the sample
#' information with RFU measurements and background signal (NC), subtracts the background,
#' and then calculates the product concentration for each time point using the corresponding model.
#'
#' @param sample_data Data.frame containing sample descriptions (from read_sample_table).
#' @param RFU_data Data.frame with RFU measurements.
#' @param NC_data Data.frame with background (NC) signal.
#' @param calib List with calibration models (from get_calibration).
#' @return Data.frame with columns:
#'         - Time: Measurement time (minutes).
#'         - well: Well name.
#'         - name: Sample name.
#'         - substrate_conc: Substrate concentration (µM).
#'         - RFU: Adjusted optical measurement.
#'         - conc: Calculated product concentration (µM).

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


#' Correct Concentration Values in Progress Curve Data
#'
#' Adjusts the calculated product concentrations using a brightness correction factor.
#' The formula accounts for the difference in brightness between the substrate and product.
#'
#' @param progress_data Data.frame containing progress curves; must include substrate_conc.
#' @param bright Numeric brightness factor (fold difference).
#' @return Data.frame with the corrected conc column.

correct_conc <- function(progress_data, bright) {
    progress_data$conc <- with(progress_data, (conc * bright - substrate_conc) / (bright - 1))
    progress_data
}


#' Extract Key Linear Regression Parameters
#'
#' Retrieves the intercept, slope, and R-squared value from a linear regression model.
#'
#' @param model An lm object.
#' @return Data.frame with columns: inter (intercept), slope (slope),
#'         and Rsq (R-squared).

get_slope_int_Rsq <- function(model) {
    inter <- coef(model)[1]
    slope <- coef(model)[2]
    Rsq <- summary(model)$r.squared
    data.frame(inter = inter,
               slope = slope,
               Rsq = Rsq)
}


#' Extract Coefficients from a Quadratic Regression Model
#'
#' For a quadratic (or linear) model, extracts coefficients a, b, and c. For a quadratic model,
#' a is the coefficient of the squared term, b is the coefficient of the linear term, and c is the intercept.
#' For a linear model (when quadratic term is absent), a is set to NA.
#'
#' @param model An lm object.
#' @return Data.frame with columns: a, b, c, and Rsq (R-squared).

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


#' Run Complete Assay Analysis
#'
#' This function orchestrates the entire assay workflow: it reads the standard data,
#' computes the background (NC) signal, fits calibration models, calculates progress curves,
#' and optionally applies a brightness correction.
#'
#' @param standard_data Data.frame with standard information (from read_standard_table).
#' @param sample_data Data.frame with sample descriptions (from read_sample_table).
#' @param RFU_data Data.frame with optical measurements (from read_RFU_data_table).
#' @param calibration_method Character indicating calibration transformation ("linear" or "log-linear").
#' @param bright Numeric brightness factor for correction; if NULL, no correction is applied.
#' @return A list containing:
#'         - NC_data: Background signal data.
#'         - standard_data_indiv: Raw standard data from individual wells.
#'         - standard_data_averaged: Aggregated standard data (geometric means).
#'         - calibration_pars: Calibration parameters for each time point.
#'         - progress_curves: Calculated progress curves.

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


#' Fit Quadratic Regression Models to Progress Curve Data
#'
#' For each unique sample (identified by name, substrate_conc, and well),
#' this function fits a quadratic model to the progress curve data. If the quadratic term is
#' significantly positive (p-value < 0.01), it switches to a simpler linear model.
#'
#' @param assay A list containing progress curve data (returned by assay()).
#' @return Data.frame with regression coefficients and R-squared values for each sample.

model_progress_curves <- function(assay) {
    df <- dplyr::distinct(assay$progress_curves, well, name, substrate_conc)
    result <- data.frame()
    for (i in 1:nrow(df)) {
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
    result[order(result$name, result$substrate_conc, decreasing = c(FALSE, TRUE), method = 'radix'), ]
}


#################### CALCULATION OF NON-LINEAR MODELS ##########################

#' Initial Guess for Vmax and Km
#'
#' Generates initial estimates for Vmax and Km based on a linear regression applied to
#' the inverse of velocity data. These estimates are used to initialize nonlinear fitting.
#'
#' @param velocity_data Data.frame with a column b (velocity) and substrate_conc.
#' @return Numeric vector of length 2 containing estimates for Vmax and Km.

guess_vmax_km <- function(velocity_data) {
    if (nrow(velocity_data) > 0) {
        model <- lm(1/b ~ I(1/substrate_conc), data = velocity_data)
        lin_coef <- coef(model)
        Vmax_guess <- as.numeric(1/lin_coef[1]) 
        Km_guess <- as.numeric(lin_coef[2] / lin_coef[1])
        if (Vmax_guess <= 0) {
            Vmax_guess <- max(velocity_data$b)
        }
        if (Km_guess <= 0) {
            r <- range(velocity_data$substrate_conc)
            Km_guess <- 10^(log10(r[1]) + (log10(r[2]) - log10(r[1]))/2)
        }
        return(c(Vmax_guess, Km_guess))
    }
    numeric()
}


#' Fit Michaelis-Menten Model using Nonlinear Least Squares
#'
#' Fits a nonlinear least squares (nls) model to velocity data using the Michaelis-Menten equation.
#'
#' @param velocity_data Data.frame with velocity data (from model_progress_curves()).
#' @param vmax Numeric initial guess for Vmax.
#' @param km Numeric initial guess for Km.
#' @return An nls model object or NULL if there are too few data points.

get_nls <- function(velocity_data, vmax, km) {
    nls(b ~ Vmax * substrate_conc / (Km + substrate_conc),
        start = c(Vmax = vmax, Km = km),
        data = velocity_data, trace = FALSE)
}


#' Compare Vmax and Km Values between Samples
#'
#' This function statistically compares Vmax and Km parameters from Michaelis-Menten fits
#' between a reference sample and other samples. It fits a combined nonlinear model and
#' extracts significance values (with optional p-value adjustment).
#'
#' @param velo_data Data.frame containing velocity data.
#' @param vmax_km_data Data.frame with Vmax and Km estimates for each sample.
#' @param ref_sample Character string, name of the reference sample.
#' @param p.adjust_method Character specifying the p-value adjustment method (default: "bonferroni").
#'        Use "no" to disable adjustment.
#' @return A list with two data.frames containing comparisons for Vmax and Km.

compare_vmax_km <- function(velo_data, vmax_km_data,
                            ref_sample, p.adjust_method = 'bonferroni') {
    
    nls_formula <- function(ref_sample) {
        as.formula(
            paste0("b ~ (Vmax0 + Vmax1 * (name != '",
                   ref_sample,
                   "')) * substrate_conc / ((Km0 + Km1 * (name != '",
                   ref_sample,
                   "')) + substrate_conc)"))
    }
    
    get_comp_nls <- function(velo_data, ref_sample, Vmax0, Vmax1, Km0, Km1) {
        nls(nls_formula(ref_sample),
            data = velo_data,
            start = c(Vmax0 = Vmax0, Vmax1 = Vmax1, Km0 = Km0, Km1 = Km1))
    }
    
    # Helper to convert p-values into significance stars.
    signif_stars <- function(pval) {
        sapply(pval, function(x) {
            if (is.na(x)) return('')
            if (x >= 0.1) return('')
            if (x >= 0.05) return('.')
            if (x >= 0.01) return('*')
            if (x >= 0.001) return('**')
            return('***')
        })
    }
    
    samples <- unique(vmax_km_data$name)
    vmax_km_data <- subset(vmax_km_data, select = c('name', 'Vmax', 'Km'))
    Vmax_table <- data.frame()
    Km_table <- data.frame()
    Vmax0 <- vmax_km_data$Vmax[vmax_km_data$name == ref_sample][1]
    Km0 <- vmax_km_data$Km[vmax_km_data$name == ref_sample][1]
    for (smpl in samples) {
        if (smpl == ref_sample) next
        sub_data <- subset(velo_data, name %in% c(ref_sample, smpl))
        Vmax1 <- vmax_km_data$Vmax[vmax_km_data$name == smpl][1]
        Km1 <- vmax_km_data$Km[vmax_km_data$name == smpl][1]
        model <- get_comp_nls(sub_data, ref_sample = ref_sample,
                              Vmax0 = Vmax0, Vmax1 = Vmax1,
                              Km0 = Km0, Km1 = Km1)
        mod_summary <- summary(model)
        summary_table <- as.data.frame(mod_summary$parameters, row.names = NULL)
        # Compute fold-change relative to the reference sample.
        Vmax_fold <- Vmax1 / Vmax0
        Km_fold <- Km1 / Km0
        # Format output for Vmax comparison.
        Vmax_line <- summary_table[2, ]
        Vmax_line$name <- smpl
        Vmax_line$fold <- Vmax_fold
        Vmax_table <- rbind(Vmax_table, Vmax_line)
        # Format output for Km comparison.
        Km_line <- summary_table[4, ]
        Km_line$name <- smpl
        Km_line$fold <- Km_fold
        Km_table <- rbind(Km_table, Km_line)
    }
    
    Vmax_table <- Vmax_table[,  c(5, 1, 6, 4)]
    Km_table <- Km_table[,  c(5, 1, 6, 4)]
    names(Vmax_table) <- c('name', 'change', 'fold', 'p.value')
    names(Km_table) <- c('name', 'change', 'fold', 'p.value')
    Vmax_table <- merge(vmax_km_data[, c('name', 'Vmax')], Vmax_table, all = TRUE)
    Km_table <- merge(vmax_km_data[, c('name', 'Km')], Km_table, all = TRUE)
    
    if (p.adjust_method != 'no') {
        Vmax_table$p.adjust <- p.adjust(Vmax_table$p.value, method = p.adjust_method)
        Km_table$p.adjust <- p.adjust(Km_table$p.value, method = p.adjust_method)
        Vmax_table$stars <- signif_stars(Vmax_table$p.adjust)
        Km_table$stars <- signif_stars(Km_table$p.adjust)
        Vmax_table[, 2:6] <- signif(Vmax_table[, 2:6], 4)
        Km_table[, 2:6] <- signif(Km_table[, 2:6], 4)
    } else {
        Vmax_table$stars <- signif_stars(Vmax_table$p.value)
        Km_table$stars <- signif_stars(Km_table$p.value)
        Vmax_table[, 2:5] <- signif(Vmax_table[, 2:5], 4)
        Km_table[, 2:5] <- signif(Km_table[, 2:5], 4)
    }
    
    return(list(Vmax = Vmax_table,
                Km = Km_table))
}


########################## NLME MODELS #########################################

#' Michaelis-Menten Equation
#'
#' Calculates the reaction velocity given Vmax, Km, and substrate concentration.
#'
#' @param Vmax Numeric, maximum reaction velocity.
#' @param Km Numeric, Michaelis constant.
#' @param substrate_conc Numeric, substrate concentration.
#' @return Numeric, reaction velocity.

mm <- function(Vmax, Km, substrate_conc) {
    Vmax * substrate_conc / (Km + substrate_conc)
}


#' Initial Parameter Estimates for Self-Starting NLS Models
#'
#' This helper function computes initial estimates for the Michaelis-Menten parameters (Vmax and Km)
#' which are required for self-starting nonlinear least squares (nls) models.
#'
#' @param mCall The matched call from nls().
#' @param data Data provided to the nls() call.
#' @param LHS The left-hand side variable name (velocity) in the nls formula.
#' @param ... Additional arguments.
#' @return Named numeric vector with initial guesses for Vmax and Km.

mmInit <- function(mCall, data, LHS, ...) {
    if (class(data) == 'list') {
        velo <- data[[LHS]]
        substrate_conc <- data[['substrate_conc']]
    } else {
        velo <- data[, as.character(LHS)]
        substrate_conc <- data[, 'substrate_conc']
    }
    vmax_km <- guess_vmax_km(data.frame(b = velo,
                                        substrate_conc = substrate_conc))
    names(vmax_km) <- mCall[c('Vmax', 'Km')]
    return(vmax_km)
}


####################### DATA VISUALISATION #####################################

#' Custom Plot Theme for the Application
#'
#' Defines a ggplot2 theme that is consistently used across the application for better aesthetics.

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


#' Plot Standard Data from Calibration Titration
#'
#' Generates a ggplot showing the RFU signals from the standard titration data.
#' Depending on the mode, the plot can display individual well data or averaged data.
#'
#' @param assay List returned by assay().
#' @param mode Character string: either "individual" (plot each well's data) or "averaged"
#'        (plot geometric means for each concentration level).
#' @return A ggplot object.

show_standard_data <- function(assay, mode = 'individual') {
    conc_levels <- as.character(sort(unique(assay$standard_data_indiv$conc), decreasing = TRUE))
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


#' Plot Calibration Parameters over Time
#'
#' Displays how one of the regression parameters (slope, intercept, or R-squared) changes
#' during the assay.
#'
#' @param assay List returned by assay().
#' @param parameter Character string: "slope", "intercept", or "Rsq".
#' @param y_limits Numeric vector of length 2 for Y-axis limits. NA values will be auto-determined.
#' @return A ggplot object.

show_calibration_pars <- function(assay, parameter, y_limits = c(NA, NA)) {
    calib <- assay$calibration_pars
    calib$time <- as.numeric(rownames(calib))
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


#' Plot Progress Curves for the Assay
#'
#' Constructs one or several ggplot objects that display the progress curves (product concentration
#' vs. time). The plotting mode can be:
#'   - "all": All progress curves in a single plot.
#'   - "by sample": Separate plot for each sample.
#'   - "by sample & conc": Separate plot for each combination of sample and substrate concentration.
#' Optionally, initial reaction velocities can be overlaid as dashed lines.
#'
#' @param assay List returned by assay().
#' @param mode Character indicating the layout mode ("all", "by sample", "by sample & conc").
#' @param show_velocities Logical indicating whether to overlay initial reaction velocities.
#' @param curve_models Data.frame of regression parameters from model_progress_curves().
#' @return A list of ggplot objects.

show_progress_curves <- function(assay,
                                 mode = 'all',
                                 show_velocities = FALSE,
                                 curve_models = NULL) {
    substr_conc_levels <- sort(unique(assay$progress_curves$substrate_conc), decreasing = TRUE)
    samples <- levels(assay$progress_curves$name)
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
        for (i in 1:nrow(vars)) {
            df <- subset(assay$progress_curves, name == vars[i, 1] & substrate_conc == vars[i, 2])
            if (nrow(df) > 0) {
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
    # Optionally add velocity lines as dashed lines.
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
                smpl_name <- vars[i, 1]
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


#' Compute Predicted Michaelis-Menten Curve
#'
#' Given an nls model fitted to Michaelis-Menten kinetics, this function generates predicted
#' reaction velocities over a sequence of substrate concentrations.
#'
#' @param model An nls model object.
#' @param max_conc Numeric, maximum substrate concentration.
#' @return Data.frame with columns:
#'         - substrate_conc: Substrate concentration values.
#'         - velocity: Predicted reaction velocity.

get_mm_model <- function(model, max_conc) {
    df <- data.frame(substrate_conc = seq(0, max_conc, by = 0.5))
    df$velocity <- predict(model, df)
    df
}


#' Plot Michaelis-Menten Curve with Data Points
#'
#' Displays a scatter plot of reaction velocity data along with a fitted Michaelis-Menten curve.
#' If a model is provided, the predicted curve and the half‑Vmax point are overlaid.
#'
#' @param data Data.frame containing reaction velocity data (e.g., from model_progress_curves()).
#' @param model An optional nls model for the Michaelis-Menten curve.
#' @return A ggplot object.

show_mm_plot <- function(data, model = NULL) {
    if (nrow(data) == 0) return(NULL)
    fig <- ggplot(data, aes(substrate_conc, b)) +
        geom_point(size = 2) +
        scale_y_continuous(limits = c(0, NA)) +
        plot_theme +
        labs(x = expression('Substrate concentration, '*mu*'M'),
             y = expression('Velocity, '*mu*'M/min'))
    if (is.null(model)) return(fig)
    max_conc <- max(data$substrate_conc)
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


#' Layout Multiple Michaelis-Menten Plots
#'
#' Creates a combined layout of Michaelis-Menten plots for several samples. Data points and
#' predicted curves (if available) are plotted for each sample.
#'
#' @param velocity_data Data.frame containing reaction velocity data.
#' @param nls_models A list of nls models for the Michaelis-Menten fits, indexed by sample name.
#' @param samples_include Optional character vector specifying which samples to include.
#' @return A ggplot object with the combined layout.

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
    if (nrow(predicted_by_models) > 0) {
        fig <- fig + geom_line(data = predicted_by_models,
                               aes(x = substrate_conc,
                                   y = velocity,
                                   color = name))
    }
    fig + scale_color_discrete(name = 'Sample')
}


#' Plot Km vs. Vmax with Optional Error Bars
#'
#' Creates a scatter plot of Km and Vmax values for different samples. Optionally, error bars
#' can be displayed if lower/upper bounds are provided.
#'
#' @param data Data.frame containing columns Km, Vmax, and optionally error bounds.
#' @param show_error_bars Logical indicating whether to show error bars.
#' @return A ggplot object.

show_km_vmax <- function(data, show_error_bars = TRUE) {
    plot <- ggplot(data, aes(Km, Vmax, color = name)) +
        geom_point(size = 3) +
        scale_color_discrete(name = 'Sample') +
        plot_theme +
        labs(x = expression('Km, '*mu*'M'),
             y = expression('Vmax, '*mu*'M/min'))
    if (show_error_bars) {
        x_range <- range(c(data$Vmax_lower, data$Vmax_upper))
        y_range <- range(c(data$Km_lower, data$Km_upper))
        h <- (x_range[2] - x_range[1]) / 25
        w <- (y_range[2] - y_range[1]) / 25
        plot <- plot +
            geom_errorbarh(aes(xmin = Km_lower, xmax = Km_upper), height = h) +
            geom_errorbar(aes(ymin = Vmax_lower, ymax = Vmax_upper), width = w) 
    }
    plot
}
