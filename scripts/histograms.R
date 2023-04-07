# This R code generates histograms showing the distribution of welfare
# effects resulting from the distributional sensitivity analysis of the
# main results of Hübler et al. (2022). Separate histograms are generated
# for each policy scenario and income group. Bootstrap 95% confidence
# intervals are computed using the percentile method.

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
font_import()
loadfonts(device = "win")

# clear workspace
rm(list = ls())

# set random seed
set.seed(127)

# create elasticity, policy and income group names
elasticities = c("esubd", "esubm", "esubva")
policies = c("policy", "cbam")
inc_groups = c("lo", "mi", "hi")

for(i in 1:length(elasticities)) {
  # load welfare effects data frame
  welf_name = paste("welf", elasticities[i], sep = "_")
  
  assign(
    x = welf_name,
    value = readRDS(
         paste0("prepared/", welf_name, ".rds")
    )
  )
  
  # compute bootstrap 95% confidence intervals
  # and create histograms
  for(j in 1:length(policies)) {
    for(k in 1:length(inc_groups)) {
      welf_effect_name = paste(
        policies[j], inc_groups[k], sep = "_"
      )
      welf_effect = (get(welf_name))[welf_effect_name]
      
      # compute bootstrap 95% confidence intervals
      ci_value = ci_mean(
        welf_effect[, ],
        type = "bootstrap",
        boot_type = "perc",
        R = 1000
      )
      
      # create histogram with mean and 95% confidence intervals
      plot_name = paste("hist",
                        elasticities[i],
                        welf_effect_name,
                        sep = "_"
                        )
      plot_value = ggplot(
        data = get(welf_name),
        aes(x = welf_effect[, ])) +
        # histogram
        geom_histogram(
          aes(y = after_stat(density)),
          color = "black",
          fill = "#AFC4DE",
          bins = 15
          ) +
        # mean
        geom_vline(
          xintercept = mean(welf_effect[, ]),
          color = "#36638B",
          linetype = "dashed",
          size = 1
          ) +
        # lower bound of 95% confidence intervals
        geom_vline(
          xintercept = ci_value$interval[1],
          color = "black",
          linetype = "dashed",
          size = 1
          ) +
        # lower bound of 95% confidence intervals
        geom_vline(
          xintercept = ci_value$interval[2],
          color = "black",
          linetype = "dashed",
          size = 1
          ) +
        # label axes
        labs(
          x = "Welfare change / %",
          y = "Density"
          ) +
        # Kernel density estimation
        geom_density(
          alpha = 0.2,
          color = "#36638B",
          size = 1
          ) +
        theme_minimal(base_size = 18.5)
      
      # save histogram as PDF
      ggsave(
        paste(
          "figures",
          elasticities[i],
          paste0(plot_name, ".pdf"),
          sep = "/")
      )
    }
  }
}
rm(i, j, k, welf_effect_name, welf_effect, welf_name,
   ci_value, plot_name, plot_value, elasticities,
   inc_groups, policies)