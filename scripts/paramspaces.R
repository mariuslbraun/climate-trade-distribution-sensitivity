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
# 
# Marius Braun, April 2023

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
variation = 0.1 # size of variation
num_decimals = 3 # max. number of decimals (important due to file path length restrictions)

# create vector of elasticity names
elasticities = c("esubd", "esubm", "esubva")

for(i in 1:length(elasticities)) {
  # load CSV file with baseline parameter values
  filename = paste("intervals_10p", elasticities[i], sep = "_")
  file = read.csv(
    paste0("paramspaces/", elasticities[i], "_baseline.csv"),
    header = T
  )
  # vary baseline values by +- 10%
  file$elasticity_minus_10p = file$elasticity * (1 - variation)
  file$elasticity_plus_10p = file$elasticity * (1 + variation)
  
  assign(
    x = filename,
    value = file
  )
  
  num_sectors = nrow(get(filename))
  paramspace = data.frame(matrix(nrow = num_draws, ncol = 0))
  
  # generate draws from a uniform distribution of +- 10 % around each estimate
  for(j in 1:num_sectors){
    paramspace = cbind(
      paramspace, 
      round(
        runif(
        num_draws,
        min = (get(filename))["elasticity_minus_10p"][j, ],
        max = (get(filename))["elasticity_plus_10p"][j, ]),
        num_decimals
        )
      )
  }
  
  # write parameter space to CSV file
  colnames(paramspace) = (get(filename))$X...sector
  write.csv(
    paramspace,
    paste0("paramspaces/paramspace_", elasticities[i], ".csv"),
    row.names = FALSE
    )
}
rm(i, j, filename, file, paramspace)