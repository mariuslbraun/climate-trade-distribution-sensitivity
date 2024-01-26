# This R code extracts welfare effects from the output files of the
# distributional sensitivity analysis of the main results of Hübler
# et al. (2022). Welfare effects are then stored in a separate data
# frame for each set of parameters considered and saved in RDS files
# to be used in subsequent analyses.

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
library(doSNOW)
library(foreach)
library(parallel)

# clear workspace
rm(list = ls())



#### create parameter, policy and income group name vectors ####

# this is where you put the output files from the sensitivity analysis
# (use separate subdirectories for each set of parameters)
output_dir = "output_sensitivity"

# string vector for parameter names based on subdirectory names
params = str_split(
  list.dirs(output_dir, recursive = FALSE),
  "/",
  simplify = T
)[, 2]
# save parameter name vector
saveRDS(params, "interim/params.rds")

# string vectors for policy scenarios and income group names
scenarios = c("policy", "cbam")
income_groups = c("lo", "mi", "hi")



#### create welfare effects dataframes ####

# parallelize
cl = makeCluster(detectCores() - 2, type = "SOCK") # make cluster
registerDoSNOW(cl) # register cluster
packages = c( # load packages
  "ggplot2", "readr", "extrafont", "openxlsx", "Rcpp", "tictoc", "moments",
  "confintr", "dplyr", "stringr", "foreach", "doSNOW", "parallel"
)

# iterate over vector of parameter names
welf_output = foreach(
  i = 1:length(params),
  .packages = packages
  ) %dopar% {
    # load functions
    source("scripts/functions.R")

    make_welf_df(
      dir = output_dir,
      param = params[i],
      policies = scenarios,
      inc_groups = income_groups
    )
}
stopCluster(cl)

# unlist data frames
for(i in 1:length(params)) {
  assign(
    x = paste("welf", params[i], sep = "_"),
    value = welf_output[[i]]
  )
}
rm(i, welf_output)
