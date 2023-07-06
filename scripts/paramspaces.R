# This R code generates parameter spaces for the distributional
# sensivitity analyses of the main results of Hübler et al. (2022).
# More specifically, 1000 random draws from a +-10% interval around 
# each of the sector-specific elasiticity estimates are generated.
# This results in 1000 sets of sectoral parameter values to be used
# in a parameter sweep in Snakemake.
#
# A separate parameter space is generated for each of the three sets
# of sector-specific elasticity parameters present in the model:
# - esubd(i): elasticities between domestically produced versus
#   imported goods
# - esubm(i): Armington elasticities
# - esubva(j): elasticities between production factors

# load packages
library(ggplot2)
library(readr)
library(extrafont)
library(openxlsx)
library(Rcpp)
library(tictoc)
library(moments)
library(confintr)
library(stringr)
library(dplyr)

# clear workspace
rm(list = ls())

num_draws = 1000  # number of draws from interval
set.seed(127)   # set random seed
var = 0.1 # size of variation
num_decimals = 3 # max. number of decimals (important due to file path length restrictions)

# create vector of elasticity names
elasticities = c("esubd", "esubm", "esubva")

# create parameter spaces
make_paramspace = function(elasticity_type) {
  # load CSV file with baseline parameter values
  baseline = read.csv(
    paste0("paramspaces/", elasticity_type, "_baseline.csv"),
    header = T
  )
  # vary baseline values by +- var %
  baseline$elasticity_minus_10p = baseline$elasticity * (1 - var)
  baseline$elasticity_plus_10p = baseline$elasticity * (1 + var)
  
  paramspace = data.frame(matrix(nrow = num_draws, ncol = 0))
  
  # generate draws from a uniform distribution of +- var % around each estimate
  make_draws = function(sector) {
    sector_draws = round(
      x = runif(
        n = nrow(paramspace),
        min = baseline["elasticity_minus_10p"][sector, ],
        max = baseline["elasticity_plus_10p"][sector, ]
      ),
      digits = num_decimals
    )
    paramspace = cbind(paramspace, sector_draws)
  }
  paramspace = as.data.frame(lapply(X = 1:nrow(baseline), FUN = make_draws))
  
  # write parameter space to CSV file
  colnames(paramspace) = baseline$X...sector
  write.csv(
    paramspace,
    paste0("paramspaces/paramspace_", elasticity_type, ".csv"),
    row.names = FALSE
  )
}
lapply(X = elasticities, FUN = make_paramspace)