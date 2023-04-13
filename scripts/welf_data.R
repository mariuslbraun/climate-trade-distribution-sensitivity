# This R code extracts welfare effects from the output files of the
# distributional sensitivity analysis of the main results of Hübler
# et al. (2022). Welfare effects are then stored in a separate data
# frame for esubd(i), esubm(i) and esubva(j) and saved in RDS files
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

# clear workspace
rm(list = ls())

# this is where you put the output files from the sensitivity analysis
# (use separate subdirectories for esubd(i), esubm(i), and esubva(j))
dir = "C:/Users/Marius Braun/output_sensitivity"

# string vectors for elasticity, policy and income group names
elasticities = c("esubd", "esubm", "esubva")
policies = c("policy", "cbam")
inc_groups = c("lo", "mi", "hi")

for(elasticity in 1:length(elasticities)) {
  # load file paths of output files
  filenames = as.data.frame(
    list.files(
      paste(
        dir,
        elasticities[elasticity],
        sep = "/"
        ),
      pattern = "output.xlsx",
      full.names = T,
      recursive = T)
  )
  colnames(filenames) = "files"
  
  # create empty data frame to store welfare effects
  welf_name = paste(
    "welf",
    elasticities[elasticity],
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
      welfare = read.xlsx(filename, sheet = "welfp") %>%
        filter(TOC == "DEU" &
                 str_detect(X3,
                            paste(
                              paste0("\\b",
                                     policies),
                              collapse = "|")
                 )
        )
      welfare = welfare %>%
          arrange(desc(X3))

    }
    welf[f, ] = t(as.numeric(welfare$X4))
  }
  
  # name welfare effects data frame
  assign(
    x = welf_name,
    value = welf
  )
  
  # save welfare effects data frame as RDS file
  saveRDS(get(welf_name),
          paste0(
            "prepared/",
            welf_name,
            ".rds")
          )
}
rm(elasticity, policy, inc_group, filename, f,
   col_num, welf_name, welf, filenames,
   welfare, dir, elasticities, inc_groups, policies)