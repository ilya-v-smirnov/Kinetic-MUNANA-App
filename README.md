# Kinetic MUNANA App

The Kinetic MUNANA App has been designed to study enzymatic activity of influenza A neuraminidase (NA)
and the effects of specific monoclonal antibodies on its function in kinetic MUNANA assay. During the
assay a weakly fluorescent 2′-(4-Methylumbelliferyl)-α-D-N-acetylneuraminic acid (MUNANA) is converted
to a brightly fluorescent 4-methylumbelliferone (4-MU). Measuring fluorescence intensities in real time
allows one to estimate reaction velocities at different substrate concentrations. Knowing these parameters,
one can calculate parameters of Michaelis-Menten equation (Km and Vmax) of the enzyme and how they change
in the presence of antibodies.

The app estimates values of Km and Vmax, the parameters of Michaelis-Menten equation, and their 95% confidence
intervals fitting non-linear least-squares (NLS) models to obtained velocity data. Initial guesses of Km and
Vmax values for the NLS models are picked up automatically based on regression parameters of reciprocal values.
However, a user has an option to correct initial guesses if automatic algorithm leads to non-converting models.
If two or more samples are studied the app makes statistical inference about Km and Vmax values comparing them
with a selected reference.

All plots and data could be saved into a MS Word file or exported as an Excel table for further analysis.

## Table of Contents

- [Features]
- [Getting Started]
  - [Prerequisites]
  - [Installation]
- [Contributing]
- [License]


## Features

List the key features of your Shiny app.

- Reading Excel Files with optical data
- Correction of the optical signal for weak MUNANA fluorescence (optional).
- Transformation of optical signals into product concentrations using a linear or logarithmic method.
- Automatic assessment of inintial reaction velocities by fitting linear regression.
- Automatic assessment of Km and Vmax values and their 95% confidence intervals.
- Statistical comparison of Michaelis-Menten curves if more than two samples are provided.
- Automatic generation of a Word files with assay report.

## Getting Started

Refer to the ./doc/User Manual.pdf file to get started.
Use sample data sets to test the app functionality.


### Prerequisites

- R (version ≥4.0.5)
- shiny (version ≥1.7.1)
- ggplot2 (version ≥3.3.6)
- stringr (version ≥1.4.0)
- xlsx (version ≥0.6.5)
- officer (version ≥0.4.2)
- cowplot (version ≥1.1.1)
- reshape2 (version ≥1.4.4)


### Installation

The app could be locally installed. Data processing is implemented using R programming language. The graphical
user interface requires Java to function. Therefore, both should be installed on a computer to launch the app.

The following instruction is valid for Windows machines:

1. Clone this repository.

2. Install R (version 4.0.5 or higher): https://cran.r-project.org/bin/windows/base/

3. (Optional) Install RStudio: https://www.rstudio.com/

4. Install Java for Windows (64-bit version) if it is not already present on the machine:
https://www.java.com/en/download/manual.jsp

5. Download the folder with the app from our git-hub account (https://github.com/ilya-v-smirnov/Kinetic-MUNANA-App)
and copy it into the desired location.

6. Execute the before_first_start.R script. If the rtools package is properly installed (no warning messages popes up).
If it is not installed, try to install it separately from here: https://cran.r-project.org/bin/windows/Rtools/. Next,
re-run before_first_start.R script.

7. Open the App.R script in RStudio or any other text editor. On the top of the file, change the path variable to the
current location of this file. Note the direction of slash symbols in the path value (should be ‘/’, not ‘\’).

8. Repeat the previous step with the run.r file.

9. Open the MUNANA App.bat file in any text editor. Change the first address for the location of R.exe file in your machine,
and the second address for current location of the run.r file.

10. Double click on MUNANA App.bat file to launch the application. It should appear in default web browser.


## Contributing

- Ilya Smirnov (smirnov.iv.mail@gmail.com)


## License

This software is distributed under the MIT license.
