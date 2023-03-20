# This R code generates random draws from a +-10 % interval around the 
# elasticities of substitution esubd(i) for each sector in the Pothen and
# Huebler model (2018). It then writes these draws to a CSV file to be 
# used as the parameter space for sensitivity analysis in Snakemake.
# 
# Marius Braun, May 2022

library(readr)
library(dplyr)

numDraws = 1000  # number of draws from interval
set.seed(127)   # set random seed

# import CSV file with intervals
intervals_10p = read.csv("C:\\Users\\Marius Braun\\Sensitivity analyses\\esubd(i)2\\intervals_esubd(i)_10_percent.csv", header = TRUE)
n = nrow(intervals_10p)

numbers = data.frame(matrix(nrow = numDraws, ncol = 0)) # generate empty data frame to store random draws in

for(i in 1:n){
  # draws from a uniform distribution of +- 10 % around each estimate
  numbers = cbind(numbers, round(runif(numDraws, min = intervals_10p[i, 2], max = intervals_10p[i, 3]), 3))
}
remove(i)

colnames(numbers) = c("dAGR", "dCOA", "dCRU", "dNGA", "dPET", "dFOO", "dMIN", "dPAP", "dCHE", "dNMM", "dIRS", "dNFM", "dMAN", "dELE", "dTRN", "dCON", "dSER")

# write random draws to CSV file as parameter space
write.csv(numbers, "C:\\Users\\Marius Braun\\Sensitivity analyses\\esubd(i)2\\parameter_space_esubd(i)_interval_10p.csv", row.names = FALSE)
  