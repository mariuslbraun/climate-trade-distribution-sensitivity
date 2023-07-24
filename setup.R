# This script sets up a renv for the project and installs required packages

# We are using the daily CRAN snapshots from RStudio Package Manager: 
# https://packagemanager.rstudio.com/client/#/repos/1/overview
# Currently, we are using the snapshot from March 17, 2023:
# https://packagemanager.rstudio.com/cran/2023-03-17

# Select the repo snapshot:
options(repos = c(
  REPO_NAME = "https://packagemanager.rstudio.com/cran/2023-03-17"
  ))

# Install renv
install.packages("renv")

# Initialize renv for the project
# bare = TRUE: instead of installing dependencies automatically, we install packages manually
renv::init(bare = TRUE)

# Install the packages
install.packages(c(
  "ggplot2", "readr", "extrafont", "openxlsx", "Rcpp", "tictoc", "moments",
  "confintr", "dplyr", "stringr", "foreach", "doSNOW", "parallel"
  ))

# Take a snapshot of the renv
renv::snapshot()
