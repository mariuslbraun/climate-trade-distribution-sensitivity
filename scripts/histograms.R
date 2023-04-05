# This R code generates histograms showing the distribution of welfare
# effects resulting from the distributional sensitivity analysis of the
# main results of Hübler et al. (2022). Separate histograms are generated
# for each policy scenario and income group. Bootstrap 95% confidence
# intervals are computed using the percentile method.
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
font_import()
loadfonts(device = "win")

# clear workspace
rm(list = ls())

# set random seed
set.seed(127)



#### 1. esubm(i) ####
# consumer split: plot welfare effects of EU climate policy and CBAM separately for income groups
# EU climate policy

# low-income group
# calculate 95 % confidence intervals via bootstrapping
ci_esubm_policy_lo = ci_mean(welf_esubm_10p$policy_lo, type = "bootstrap", boot_type = "perc", R = 1000)
policy_lo_mean_esubm_minus_error_margin = ci_esubm_policy_lo$interval[1]
policy_lo_mean_esubm_plus_error_margin = ci_esubm_policy_lo$interval[2]

# histogram for policy_lo
p.esubm.10p.policy_lo = ggplot(welf_esubm_10p, aes(x = policy_lo)) +
    geom_histogram(aes(y = ..density..),
                   color = "black", fill = "#AFC4DE", bins = 15) +
    geom_vline(xintercept = mean(welf_esubm_10p$policy_lo),
               color = "#36638B", linetype = "dashed", size = 1) +
    geom_vline(xintercept = policy_lo_mean_esubm_minus_error_margin,
               color = "black", linetype = "dashed", size = 1) +
    geom_vline(xintercept = policy_lo_mean_esubm_plus_error_margin,
               color = "black", linetype = "dashed", size = 1) + 
    labs(x = "Welfare change / %", y = "Density") +
    geom_density(alpha = 0.2, color = "#36638B", size = 1) +
    theme_minimal(base_size = 18.5)
p.esubm.10p.policy_lo
ggsave("hist_esubm(i)_10p_policy_lo.pdf")
remove(ci_esubm_policy_lo, policy_lo_mean_esubm_minus_error_margin, policy_lo_mean_esubm_plus_error_margin)

# middle-income group
# calculate 95 % confidence intervals via bootstrapping
ci_esubm_policy_mi = ci_mean(welf_esubm_10p$policy_mi, type = "bootstrap", boot_type = "perc", R = 1000)
policy_mi_mean_esubm_minus_error_margin = ci_esubm_policy_mi$interval[1]
policy_mi_mean_esubm_plus_error_margin = ci_esubm_policy_mi$interval[2]

# histogram for policy_mi
p.esubm.10p.policy_mi = ggplot(welf_esubm_10p, aes(x = policy_mi)) +
    geom_histogram(aes(y = ..density..),
                   color = "black", fill = "#AFC4DE", bins = 15) +
    geom_vline(xintercept = mean(welf_esubm_10p$policy_mi),
               color = "#36638B", linetype = "dashed", size = 1) +
    geom_vline(xintercept = policy_mi_mean_esubm_minus_error_margin,
               color = "black", linetype = "dashed", size = 1) +
    geom_vline(xintercept = policy_mi_mean_esubm_plus_error_margin,
               color = "black", linetype = "dashed", size = 1) + 
    labs(x = "Welfare change / %", y = "Density") +
    geom_density(alpha = 0.2, color = "#36638B", size = 1) +
    theme_minimal(base_size = 18.5)
p.esubm.10p.policy_mi
ggsave("hist_esubm(i)_10p_policy_mi.pdf")
remove(ci_esubm_policy_mi, policy_mi_mean_esubm_minus_error_margin, policy_mi_mean_esubm_plus_error_margin)

# high-income group
# calculate 95 % confidence intervals via bootstrapping
ci_esubm_policy_hi = ci_mean(welf_esubm_10p$policy_hi, type = "bootstrap", boot_type = "perc", R = 1000)
policy_hi_mean_esubm_minus_error_margin = ci_esubm_policy_hi$interval[1]
policy_hi_mean_esubm_plus_error_margin = ci_esubm_policy_hi$interval[2]


# histogram for policy_hi
p.esubm.10p.policy_hi = ggplot(welf_esubm_10p, aes(x = policy_hi)) +
    geom_histogram(aes(y = ..density..),
                   color = "black", fill = "#AFC4DE", bins = 15) +
    geom_vline(xintercept = mean(welf_esubm_10p$policy_hi),
               color = "#36638B", linetype = "dashed", size = 1) +
    geom_vline(xintercept = policy_hi_mean_esubm_minus_error_margin,
               color = "black", linetype = "dashed", size = 1) +
    geom_vline(xintercept = policy_hi_mean_esubm_plus_error_margin,
               color = "black", linetype = "dashed", size = 1) + 
    labs(x = "Welfare change / %", y = "Density") +
    geom_density(alpha = 0.2, color = "#36638B", size = 1) +
    theme_minimal(base_size = 18.5)
p.esubm.10p.policy_hi
ggsave("hist_esubm(i)_10p_policy_hi.pdf")
remove(ci_esubm_policy_hi, policy_hi_mean_esubm_minus_error_margin, policy_hi_mean_esubm_plus_error_margin)

# CBAM

# low-income group
# calculate 95 % confidence intervals via bootstrapping
ci_esubm_cbam_lo = ci_mean(welf_esubm_10p$cbam_lo, type = "bootstrap", boot_type = "perc", R = 1000)
cbam_lo_mean_esubm_minus_error_margin = ci_esubm_cbam_lo$interval[1]
cbam_lo_mean_esubm_plus_error_margin = ci_esubm_cbam_lo$interval[2]


# histogram for cbam_lo
p.esubm.10p.cbam_lo = ggplot(welf_esubm_10p, aes(x = cbam_lo)) +
    geom_histogram(aes(y = ..density..),
                   color = "black", fill = "#AFC4DE", bins = 15) +
    geom_vline(xintercept = mean(welf_esubm_10p$cbam_lo),
               color = "#36638B", linetype = "dashed", size = 1) +
    geom_vline(xintercept = cbam_lo_mean_esubm_minus_error_margin,
               color = "black", linetype = "dashed", size = 1) +
    geom_vline(xintercept = cbam_lo_mean_esubm_plus_error_margin,
               color = "black", linetype = "dashed", size = 1) + 
    labs(x = "Welfare change / %", y = "Density") +
    geom_density(alpha = 0.2, color = "#36638B", size = 1) +
    theme_minimal(base_size = 18.5)
p.esubm.10p.cbam_lo
ggsave("hist_esubm(i)_10p_cbam_lo.pdf")
remove(ci_esubm_cbam_lo, cbam_lo_mean_esubm_minus_error_margin, cbam_lo_mean_esubm_plus_error_margin)

# middle-income group
# calculate 95 % confidence intervals via bootstrapping
ci_esubm_cbam_mi = ci_mean(welf_esubm_10p$cbam_mi, type = "bootstrap", boot_type = "perc", R = 1000)
cbam_mi_mean_esubm_minus_error_margin = ci_esubm_cbam_mi$interval[1]
cbam_mi_mean_esubm_plus_error_margin = ci_esubm_cbam_mi$interval[2]

# histogram for cbam_mi
p.esubm.10p.cbam_mi = ggplot(welf_esubm_10p, aes(x = cbam_mi)) +
    geom_histogram(aes(y = ..density..),
                   color = "black", fill = "#AFC4DE", bins = 15) +
    geom_vline(xintercept = mean(welf_esubm_10p$cbam_mi),
               color = "#36638B", linetype = "dashed", size = 1) +
    geom_vline(xintercept = cbam_mi_mean_esubm_minus_error_margin,
               color = "black", linetype = "dashed", size = 1) +
    geom_vline(xintercept = cbam_mi_mean_esubm_plus_error_margin,
               color = "black", linetype = "dashed", size = 1) + 
    labs(x = "Welfare change / %", y = "Density") +
    geom_density(alpha = 0.2, color = "#36638B", size = 1) +
    theme_minimal(base_size = 18.5)
p.esubm.10p.cbam_mi
ggsave("hist_esubm(i)_10p_cbam_mi.pdf")
remove(ci_esubm_cbam_mi, cbam_mi_mean_esubm_minus_error_margin, cbam_mi_mean_esubm_plus_error_margin)

# high-income group
# calculate 95 % confidence intervals via bootstrapping
ci_esubm_cbam_hi = ci_mean(welf_esubm_10p$cbam_hi, type = "bootstrap", boot_type = "perc", R = 1000)
cbam_hi_mean_esubm_minus_error_margin = ci_esubm_cbam_hi$interval[1]
cbam_hi_mean_esubm_plus_error_margin = ci_esubm_cbam_hi$interval[2]


# histogram for cbam_hi
p.esubm.10p.cbam_hi = ggplot(welf_esubm_10p, aes(x = cbam_hi)) +
    geom_histogram(aes(y = ..density..),
                   color = "black", fill = "#AFC4DE", bins = 15) +
    geom_vline(xintercept = mean(welf_esubm_10p$cbam_hi),
               color = "#36638B", linetype = "dashed", size = 1) +
    geom_vline(xintercept = cbam_hi_mean_esubm_minus_error_margin,
               color = "black", linetype = "dashed", size = 1) +
    geom_vline(xintercept = cbam_hi_mean_esubm_plus_error_margin,
               color = "black", linetype = "dashed", size = 1) + 
    labs(x = "Welfare change / %", y = "Density") +
    geom_density(alpha = 0.2, color = "#36638B", size = 1) +
    theme_minimal(base_size = 18.5)
p.esubm.10p.cbam_hi
ggsave("hist_esubm(i)_10p_cbam_hi.pdf")
remove(ci_esubm_cbam_hi, cbam_hi_mean_esubm_minus_error_margin, cbam_hi_mean_esubm_plus_error_margin)



#### 2. esubd(i) ####
# consumer split: plot welfare effects of EU climate policy and CBAM separately for income groups
# EU climate policy

# low-income group
# calculate 95 % confidence intervals via bootstrapping
ci_esubd_policy_lo = ci_mean(welf_esubd_10p$policy_lo, type = "bootstrap", boot_type = "perc", R = 1000)
policy_lo_mean_esubd_minus_error_margin = ci_esubd_policy_lo$interval[1]
policy_lo_mean_esubd_plus_error_margin = ci_esubd_policy_lo$interval[2]

# histogram for policy_lo
p.esubd.10p.policy_lo = ggplot(welf_esubd_10p, aes(x = policy_lo)) +
  geom_histogram(aes(y = ..density..),
                 color = "black", fill = "#AFC4DE", bins = 15) +
  geom_vline(xintercept = mean(welf_esubd_10p$policy_lo),
             color = "#36638B", linetype = "dashed", size = 1) +
  geom_vline(xintercept = policy_lo_mean_esubd_minus_error_margin,
             color = "black", linetype = "dashed", size = 1) +
  geom_vline(xintercept = policy_lo_mean_esubd_plus_error_margin,
             color = "black", linetype = "dashed", size = 1) + 
  labs(x = "Welfare change / %", y = "Density") +
  geom_density(alpha = 0.2, color = "#36638B", size = 1) +
  theme_minimal(base_size = 18.5)
p.esubd.10p.policy_lo
ggsave("hist_esubd(i)_10p_policy_lo.pdf")
remove(ci_esubd_policy_lo, policy_lo_mean_esubd_minus_error_margin, policy_lo_mean_esubd_plus_error_margin)

# middle-income group
# calculate 95 % confidence intervals via bootstrapping
ci_esubd_policy_mi = ci_mean(welf_esubd_10p$policy_mi, type = "bootstrap", boot_type = "perc", R = 1000)
policy_mi_mean_esubd_minus_error_margin = ci_esubd_policy_mi$interval[1]
policy_mi_mean_esubd_plus_error_margin = ci_esubd_policy_mi$interval[2]

# histogram for policy_mi
p.esubd.10p.policy_mi = ggplot(welf_esubd_10p, aes(x = policy_mi)) +
  geom_histogram(aes(y = ..density..),
                 color = "black", fill = "#AFC4DE", bins = 15) +
  geom_vline(xintercept = mean(welf_esubd_10p$policy_mi),
             color = "#36638B", linetype = "dashed", size = 1) +
  geom_vline(xintercept = policy_mi_mean_esubd_minus_error_margin,
             color = "black", linetype = "dashed", size = 1) +
  geom_vline(xintercept = policy_mi_mean_esubd_plus_error_margin,
             color = "black", linetype = "dashed", size = 1) + 
  labs(x = "Welfare change / %", y = "Density") +
  geom_density(alpha = 0.2, color = "#36638B", size = 1) +
  theme_minimal(base_size = 18.5)
p.esubd.10p.policy_mi
ggsave("hist_esubd(i)_10p_policy_mi.pdf")
remove(ci_esubd_policy_mi, policy_mi_mean_esubd_minus_error_margin, policy_mi_mean_esubd_plus_error_margin)

# high-income group
# calculate 95 % confidence intervals via bootstrapping
ci_esubd_policy_hi = ci_mean(welf_esubd_10p$policy_hi, type = "bootstrap", boot_type = "perc", R = 1000)
policy_hi_mean_esubd_minus_error_margin = ci_esubd_policy_hi$interval[1]
policy_hi_mean_esubd_plus_error_margin = ci_esubd_policy_hi$interval[2]


# histogram for policy_hi
p.esubd.10p.policy_hi = ggplot(welf_esubd_10p, aes(x = policy_hi)) +
  geom_histogram(aes(y = ..density..),
                 color = "black", fill = "#AFC4DE", bins = 15) +
  geom_vline(xintercept = mean(welf_esubd_10p$policy_hi),
             color = "#36638B", linetype = "dashed", size = 1) +
  geom_vline(xintercept = policy_hi_mean_esubd_minus_error_margin,
             color = "black", linetype = "dashed", size = 1) +
  geom_vline(xintercept = policy_hi_mean_esubd_plus_error_margin,
             color = "black", linetype = "dashed", size = 1) + 
  labs(x = "Welfare change / %", y = "Density") +
  geom_density(alpha = 0.2, color = "#36638B", size = 1) +
  theme_minimal(base_size = 18.5)
p.esubd.10p.policy_hi
ggsave("hist_esubd(i)_10p_policy_hi.pdf")
remove(ci_esubd_policy_hi, policy_hi_mean_esubd_minus_error_margin, policy_hi_mean_esubd_plus_error_margin)

# CBAM

# low-income group
# calculate 95 % confidence intervals via bootstrapping
ci_esubd_cbam_lo = ci_mean(welf_esubd_10p$cbam_lo, type = "bootstrap", boot_type = "perc", R = 1000)
cbam_lo_mean_esubd_minus_error_margin = ci_esubd_cbam_lo$interval[1]
cbam_lo_mean_esubd_plus_error_margin = ci_esubd_cbam_lo$interval[2]


# histogram for cbam_lo
p.esubd.10p.cbam_lo = ggplot(welf_esubd_10p, aes(x = cbam_lo)) +
  geom_histogram(aes(y = ..density..),
                 color = "black", fill = "#AFC4DE", bins = 15) +
  geom_vline(xintercept = mean(welf_esubd_10p$cbam_lo),
             color = "#36638B", linetype = "dashed", size = 1) +
  geom_vline(xintercept = cbam_lo_mean_esubd_minus_error_margin,
             color = "black", linetype = "dashed", size = 1) +
  geom_vline(xintercept = cbam_lo_mean_esubd_plus_error_margin,
             color = "black", linetype = "dashed", size = 1) + 
  labs(x = "Welfare change / %", y = "Density") +
  geom_density(alpha = 0.2, color = "#36638B", size = 1) +
  theme_minimal(base_size = 18.5)
p.esubd.10p.cbam_lo
ggsave("hist_esubd(i)_10p_cbam_lo.pdf")
remove(ci_esubd_cbam_lo, cbam_lo_mean_esubd_minus_error_margin, cbam_lo_mean_esubd_plus_error_margin)

# middle-income group
# calculate 95 % confidence intervals via bootstrapping
ci_esubd_cbam_mi = ci_mean(welf_esubd_10p$cbam_mi, type = "bootstrap", boot_type = "perc", R = 1000)
cbam_mi_mean_esubd_minus_error_margin = ci_esubd_cbam_mi$interval[1]
cbam_mi_mean_esubd_plus_error_margin = ci_esubd_cbam_mi$interval[2]

# histogram for cbam_mi
p.esubd.10p.cbam_mi = ggplot(welf_esubd_10p, aes(x = cbam_mi)) +
  geom_histogram(aes(y = ..density..),
                 color = "black", fill = "#AFC4DE", bins = 15) +
  geom_vline(xintercept = mean(welf_esubd_10p$cbam_mi),
             color = "#36638B", linetype = "dashed", size = 1) +
  geom_vline(xintercept = cbam_mi_mean_esubd_minus_error_margin,
             color = "black", linetype = "dashed", size = 1) +
  geom_vline(xintercept = cbam_mi_mean_esubd_plus_error_margin,
             color = "black", linetype = "dashed", size = 1) + 
  labs(x = "Welfare change / %", y = "Density") +
  geom_density(alpha = 0.2, color = "#36638B", size = 1) +
  theme_minimal(base_size = 18.5)
p.esubd.10p.cbam_mi
ggsave("hist_esubd(i)_10p_cbam_mi.pdf")
remove(ci_esubd_cbam_mi, cbam_mi_mean_esubd_minus_error_margin, cbam_mi_mean_esubd_plus_error_margin)

# high-income group
# calculate 95 % confidence intervals via bootstrapping
ci_esubd_cbam_hi = ci_mean(welf_esubd_10p$cbam_hi, type = "bootstrap", boot_type = "perc", R = 1000)
cbam_hi_mean_esubd_minus_error_margin = ci_esubd_cbam_hi$interval[1]
cbam_hi_mean_esubd_plus_error_margin = ci_esubd_cbam_hi$interval[2]


# histogram for cbam_hi
p.esubd.10p.cbam_hi = ggplot(welf_esubd_10p, aes(x = cbam_hi)) +
  geom_histogram(aes(y = ..density..),
                 color = "black", fill = "#AFC4DE", bins = 15) +
  geom_vline(xintercept = mean(welf_esubd_10p$cbam_hi),
             color = "#36638B", linetype = "dashed", size = 1) +
  geom_vline(xintercept = cbam_hi_mean_esubd_minus_error_margin,
             color = "black", linetype = "dashed", size = 1) +
  geom_vline(xintercept = cbam_hi_mean_esubd_plus_error_margin,
             color = "black", linetype = "dashed", size = 1) + 
  labs(x = "Welfare change / %", y = "Density") +
  geom_density(alpha = 0.2, color = "#36638B", size = 1) +
  theme_minimal(base_size = 18.5)
p.esubd.10p.cbam_hi
ggsave("hist_esubd(i)_10p_cbam_hi.pdf")
remove(ci_esubd_cbam_hi, cbam_hi_mean_esubd_minus_error_margin, cbam_hi_mean_esubd_plus_error_margin)




#### 3. esubva(j) ####
# consumer split: plot welfare effects of EU climate policy and CBAM separately for income groups
# EU climate policy

# low-income group
# calculate 95 % confidence intervals via bootstrapping
ci_esubva_policy_lo = ci_mean(welf_esubva_10p$policy_lo, type = "bootstrap", boot_type = "perc", R = 1000)
policy_lo_mean_esubva_minus_error_margin = ci_esubva_policy_lo$interval[1]
policy_lo_mean_esubva_plus_error_margin = ci_esubva_policy_lo$interval[2]

# histogram for policy_lo
p.esubva.10p.policy_lo = ggplot(welf_esubva_10p, aes(x = policy_lo)) +
  geom_histogram(aes(y = ..density..),
                 color = "black", fill = "#AFC4DE", bins = 15) +
  geom_vline(xintercept = mean(welf_esubva_10p$policy_lo),
             color = "#36638B", linetype = "dashed", size = 1) +
  geom_vline(xintercept = policy_lo_mean_esubva_minus_error_margin,
             color = "black", linetype = "dashed", size = 1) +
  geom_vline(xintercept = policy_lo_mean_esubva_plus_error_margin,
             color = "black", linetype = "dashed", size = 1) + 
  labs(x = "Welfare change / %", y = "Density") +
  geom_density(alpha = 0.2, color = "#36638B", size = 1) +
  theme_minimal(base_size = 18.5)
p.esubva.10p.policy_lo
ggsave("hist_esubva(j)_10p_policy_lo.pdf")
remove(ci_esubva_policy_lo, policy_lo_mean_esubva_minus_error_margin, policy_lo_mean_esubva_plus_error_margin)

# middle-income group
# calculate 95 % confidence intervals via bootstrapping
ci_esubva_policy_mi = ci_mean(welf_esubva_10p$policy_mi, type = "bootstrap", boot_type = "perc", R = 1000)
policy_mi_mean_esubva_minus_error_margin = ci_esubva_policy_mi$interval[1]
policy_mi_mean_esubva_plus_error_margin = ci_esubva_policy_mi$interval[2]

# histogram for policy_mi
p.esubva.10p.policy_mi = ggplot(welf_esubva_10p, aes(x = policy_mi)) +
  geom_histogram(aes(y = ..density..),
                 color = "black", fill = "#AFC4DE", bins = 15) +
  geom_vline(xintercept = mean(welf_esubva_10p$policy_mi),
             color = "#36638B", linetype = "dashed", size = 1) +
  geom_vline(xintercept = policy_mi_mean_esubva_minus_error_margin,
             color = "black", linetype = "dashed", size = 1) +
  geom_vline(xintercept = policy_mi_mean_esubva_plus_error_margin,
             color = "black", linetype = "dashed", size = 1) + 
  labs(x = "Welfare change / %", y = "Density") +
  geom_density(alpha = 0.2, color = "#36638B", size = 1) +
  theme_minimal(base_size = 18.5)
p.esubva.10p.policy_mi
ggsave("hist_esubva(j)_10p_policy_mi.pdf")
remove(ci_esubva_policy_mi, policy_mi_mean_esubva_minus_error_margin, policy_mi_mean_esubva_plus_error_margin)

# high-income group
# calculate 95 % confidence intervals via bootstrapping
ci_esubva_policy_hi = ci_mean(welf_esubva_10p$policy_hi, type = "bootstrap", boot_type = "perc", R = 1000)
policy_hi_mean_esubva_minus_error_margin = ci_esubva_policy_hi$interval[1]
policy_hi_mean_esubva_plus_error_margin = ci_esubva_policy_hi$interval[2]


# histogram for policy_hi
p.esubva.10p.policy_hi = ggplot(welf_esubva_10p, aes(x = policy_hi)) +
  geom_histogram(aes(y = ..density..),
                 color = "black", fill = "#AFC4DE", bins = 15) +
  geom_vline(xintercept = mean(welf_esubva_10p$policy_hi),
             color = "#36638B", linetype = "dashed", size = 1) +
  geom_vline(xintercept = policy_hi_mean_esubva_minus_error_margin,
             color = "black", linetype = "dashed", size = 1) +
  geom_vline(xintercept = policy_hi_mean_esubva_plus_error_margin,
             color = "black", linetype = "dashed", size = 1) + 
  labs(x = "Welfare change / %", y = "Density") +
  geom_density(alpha = 0.2, color = "#36638B", size = 1) +
  theme_minimal(base_size = 18.5)
p.esubva.10p.policy_hi
ggsave("hist_esubva(j)_10p_policy_hi.pdf")
remove(ci_esubva_policy_hi, policy_hi_mean_esubva_minus_error_margin, policy_hi_mean_esubva_plus_error_margin)

# CBAM

# low-income group
# calculate 95 % confidence intervals via bootstrapping
ci_esubva_cbam_lo = ci_mean(welf_esubva_10p$cbam_lo, type = "bootstrap", boot_type = "perc", R = 1000)
cbam_lo_mean_esubva_minus_error_margin = ci_esubva_cbam_lo$interval[1]
cbam_lo_mean_esubva_plus_error_margin = ci_esubva_cbam_lo$interval[2]


# histogram for cbam_lo
p.esubva.10p.cbam_lo = ggplot(welf_esubva_10p, aes(x = cbam_lo)) +
  geom_histogram(aes(y = ..density..),
                 color = "black", fill = "#AFC4DE", bins = 15) +
  geom_vline(xintercept = mean(welf_esubva_10p$cbam_lo),
             color = "#36638B", linetype = "dashed", size = 1) +
  geom_vline(xintercept = cbam_lo_mean_esubva_minus_error_margin,
             color = "black", linetype = "dashed", size = 1) +
  geom_vline(xintercept = cbam_lo_mean_esubva_plus_error_margin,
             color = "black", linetype = "dashed", size = 1) + 
  labs(x = "Welfare change / %", y = "Density") +
  geom_density(alpha = 0.2, color = "#36638B", size = 1) +
  theme_minimal(base_size = 18.5)
p.esubva.10p.cbam_lo
ggsave("hist_esubva(j)_10p_cbam_lo.pdf")
remove(ci_esubva_cbam_lo, cbam_lo_mean_esubva_minus_error_margin, cbam_lo_mean_esubva_plus_error_margin)

# middle-income group
# calculate 95 % confidence intervals via bootstrapping
ci_esubva_cbam_mi = ci_mean(welf_esubva_10p$cbam_mi, type = "bootstrap", boot_type = "perc", R = 1000)
cbam_mi_mean_esubva_minus_error_margin = ci_esubva_cbam_mi$interval[1]
cbam_mi_mean_esubva_plus_error_margin = ci_esubva_cbam_mi$interval[2]

# histogram for cbam_mi
p.esubva.10p.cbam_mi = ggplot(welf_esubva_10p, aes(x = cbam_mi)) +
  geom_histogram(aes(y = ..density..),
                 color = "black", fill = "#AFC4DE", bins = 15) +
  geom_vline(xintercept = mean(welf_esubva_10p$cbam_mi),
             color = "#36638B", linetype = "dashed", size = 1) +
  geom_vline(xintercept = cbam_mi_mean_esubva_minus_error_margin,
             color = "black", linetype = "dashed", size = 1) +
  geom_vline(xintercept = cbam_mi_mean_esubva_plus_error_margin,
             color = "black", linetype = "dashed", size = 1) + 
  labs(x = "Welfare change / %", y = "Density") +
  geom_density(alpha = 0.2, color = "#36638B", size = 1) +
  theme_minimal(base_size = 18.5)
p.esubva.10p.cbam_mi
ggsave("hist_esubva(j)_10p_cbam_mi.pdf")
remove(ci_esubva_cbam_mi, cbam_mi_mean_esubva_minus_error_margin, cbam_mi_mean_esubva_plus_error_margin)

# high-income group
# calculate 95 % confidence intervals via bootstrapping
ci_esubva_cbam_hi = ci_mean(welf_esubva_10p$cbam_hi, type = "bootstrap", boot_type = "perc", R = 1000)
cbam_hi_mean_esubva_minus_error_margin = ci_esubva_cbam_hi$interval[1]
cbam_hi_mean_esubva_plus_error_margin = ci_esubva_cbam_hi$interval[2]


# histogram for cbam_hi
p.esubva.10p.cbam_hi = ggplot(welf_esubva_10p, aes(x = cbam_hi)) +
  geom_histogram(aes(y = ..density..),
                 color = "black", fill = "#AFC4DE", bins = 15) +
  geom_vline(xintercept = mean(welf_esubva_10p$cbam_hi),
             color = "#36638B", linetype = "dashed", size = 1) +
  geom_vline(xintercept = cbam_hi_mean_esubva_minus_error_margin,
             color = "black", linetype = "dashed", size = 1) +
  geom_vline(xintercept = cbam_hi_mean_esubva_plus_error_margin,
             color = "black", linetype = "dashed", size = 1) + 
  labs(x = "Welfare change / %", y = "Density") +
  geom_density(alpha = 0.2, color = "#36638B", size = 1) +
  theme_minimal(base_size = 18.5)
p.esubva.10p.cbam_hi
ggsave("hist_esubva(j)_10p_cbam_hi.pdf")
remove(ci_esubva_cbam_hi, cbam_hi_mean_esubva_minus_error_margin, cbam_hi_mean_esubva_plus_error_margin)