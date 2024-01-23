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

# this is where you put the output files from the sensitivity analysis
# (use separate subdirectories for each set of parameters)
dir = "output_sensitivity"

# string vector for parameter names based on subdirectory names
params = str_split(
  list.dirs(dir, recursive = FALSE),
  "/",
  simplify = T
)[, 2]
# save parameter name vector
saveRDS(params, "interim/params.rds")

# string vectors for policy scenario and income group names
policies = c("policy", "cbam")
inc_groups = c("lo", "mi", "hi")

# parallelize
cl = makeCluster(detectCores() - 2, type = "SOCK") # make cluster
registerDoSNOW(cl) # register cluster
packages = c( # load packages
  "ggplot2", "readr", "extrafont", "openxlsx", "Rcpp", "tictoc", "moments",
  "confintr", "dplyr", "stringr", "foreach", "doSNOW", "parallel"
)

# create welfare effects data frames
welf_output = foreach(
  param = 1:length(params),
  .packages = packages
  ) %dopar% {
  # load file paths of output files
  filenames = as.data.frame(
    list.files(
      file.path(
        dir,
        params[param]
      ),
      pattern = "output.xlsx",
      full.names = T,
      recursive = T)
  )
  colnames(filenames) = "files"
  
  # create empty data frame to store welfare effects
  welf_name = paste(
    "welf",
    params[param],
    sep = "_"
  )
  welf = as.data.frame(
    matrix(,
           nrow = nrow(filenames),
           ncol = length(policies) * length(inc_groups)
    )
  )
  
  # name columns of data frame according to policy and income group
  for(policy in 1:length(policies)) {
    for(inc_group in 1:length(inc_groups)) {
      col_num = inc_group +
        (as.numeric(policies[policy] == "cbam") * length(inc_groups))
      colnames(welf)[col_num] = paste(
        policies[policy],
        inc_groups[inc_group],
        sep = "_"
      )
    }
  }
  
  # get welfare effects from output files
  for(f in 1:nrow(filenames)) {
    filename = filenames$files[f]
    if(file.exists(filename)) {
      # filter welfare effects for Germany and policy scenarios
      welfare = read.xlsx(filename, sheet = "welfp") %>%
        filter(
         TOC == "DEU" &
         str_detect(
           X3,
           paste(
            paste0(
              "\\b",
              policies
            ),
            collapse = "|"
           )
         )
        )
      welfare = welfare %>%
        arrange(desc(X3))
      
    }
    welf[f, ] = t(as.numeric(welfare$X4))
  }

  # if parameter is CO2factor or esub_cons: name rows according to parameter value
  if(params[param] == "CO2factor" |
     params[param] == "esub_cons") {
    # extract parameter value from file path
    rownames(welf) = str_split(
      str_split(
        filenames$files,
        "=",
        simplify = T
        )[, 2],
      "/",
      simplify = T
    )[, 1]
    # add parameter value to data frame and convert to numeric
    welf = welf %>%
      tibble::rownames_to_column(var = params[param]) %>%
      mutate_if(is.character, as.numeric)
  }
  
  # name welfare effects data frame
  assign(x = welf_name, value = welf)
  
  # save welfare effects data frame as RDS file
  saveRDS(
    get(welf_name),
    paste0("prepared/", welf_name, ".rds")
  )
  return(get(welf_name))
}
stopCluster(cl)

# unlist data frames
for(param in 1:length(params)) {
  assign(
    x = paste("welf", params[param], sep = "_"),
    value = welf_output[[param]]
  )
}
rm(param, welf_output)
