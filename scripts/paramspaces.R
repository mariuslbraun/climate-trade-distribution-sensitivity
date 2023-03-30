# This R code generates random draws from a +-10% interval around the 
# elasticities of substitution esubd(i) for each sector in the Pothen and
# Huebler model (2018). It then writes these draws to a CSV file to be 
# used as the parameter space for sensitivity analysis in Snakemake.
# 
# Marius Braun, May 2022

library(readr)
library(dplyr)

rm(list = ls())

num_draws = 1000  # number of draws from interval
set.seed(127)   # set random seed
variation = 0.1
num_decimals = 3

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
  