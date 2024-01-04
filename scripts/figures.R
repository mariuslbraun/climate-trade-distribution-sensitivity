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
library(tibble)
font_import()
loadfonts(device = "win")

# clear workspace
rm(list = ls())

# set random seed
set.seed(127)

# create parameter, policy and income group name vectors
params = readRDS("interim/params.rds")
policies = c("policy", "cbam")
inc_groups = c("lo", "mi", "hi")

# specify file types that figures should be saved as
filetypes = c(".pdf", ".png")

# minimum CO2 target
min_CO2factor = 0.87

# load data frames and create plots
for(i in 1:length(params)) {
  # load welfare effects data frame
  welf_name = paste("welf", params[i], sep = "_")
  assign(
    x = welf_name,
    value = readRDS(
      paste0("prepared/", welf_name, ".rds")
    )
  )

  # create subdirectory for figures
  dir.create(
    file.path("figures", params[i]),
    showWarnings = F
  )
  
  # create plots
  for(j in 1:length(policies)) {
    for(k in 1:length(inc_groups)) {
      # get welfare effect from dataframe
      welf_effect_name = paste(
        policies[j], inc_groups[k], sep = "_"
      )
      welf_effect = (get(welf_name))[welf_effect_name]
      
      # if parameter is CO2factor: line plot, else histogram
      plot_type = ifelse(
        params[i] == "CO2factor" |
        params[i] == "esub_cons",
        yes = "plot",
        no = "hist"
      )
      
      # create plot name
      plot_name = paste(
        plot_type,
        params[i],
        welf_effect_name,
        sep = "_"
      )

      # if parameter is CO2factor: create line plot
      if(params[i] == "CO2factor" |
         params[i] == "esub_cons") {
        plot_value = ggplot(
          data = get(welf_name)
        ) +
          # line plot
          geom_line(
            aes(
              x = (get(welf_name))[params[i]][, ],
              y = welf_effect[, ],
              group = 1
            ),
            color = "#36638B",
            linewidth = 1
          )
          # if parameter is CO2factor: cut x-axis off at 0.87 (model does not solve for lower values)
          if(params[i] == "CO2factor") {
            plot_value = plot_value +
              scale_x_reverse(
                limits = c(
                  NA,
                  min_CO2factor
                )
              )
          }
          # label axes
          plot_value = plot_value +
            labs(
              x = ifelse(
                params[i] == "CO2factor",
                yes = "CO2 target",
                no = "Elasiticity of substitution in consumption"
              ),
              y = "Welfare change / %"
            ) +
            theme_minimal(base_size = 18.5)
      } else { # else: create histogram with mean and 95% confidence intervals
        # compute bootstrap 95% confidence intervals
        ci_value = ci_mean(
          welf_effect[, ],
          type = "bootstrap",
          boot_type = "perc",
          R = 1000
        )

        plot_value = ggplot(
          data = get(welf_name),
          aes(x = welf_effect[, ])
        ) +
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
            linewidth = 1
          ) +
          # lower bound of 95% confidence intervals
          geom_vline(
            xintercept = ci_value$interval[1],
            color = "black",
            linetype = "dashed",
            linewidth = 1
          ) +
          # lower bound of 95% confidence intervals
          geom_vline(
            xintercept = ci_value$interval[2],
            color = "black",
            linetype = "dashed",
            linewidth = 1
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
            linewidth = 1
          ) +
          theme_minimal(base_size = 18.5)
          rm(ci_value)
      }

      # save histogram as PDF and PNG
      for(l in 1:length(filetypes)) {
        ggsave(
          file.path(
            "figures",
            params[i],
            paste0(plot_name, filetypes[l])
          )
        )
      }
    }
  }
}
rm(i, j, k, welf_effect_name, welf_effect, welf_name,
   plot_name, plot_value, params, inc_groups, policies)
