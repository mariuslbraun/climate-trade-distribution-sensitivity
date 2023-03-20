# This R code generates histograms of welfare effects based on sensitivity
# analyses of the Huebler et al. (2022) consumer split CGE model.
# WelWelfare effects are evaluated for 1000 random draws from +- 10 % intervals
#  araround sector-specific estimates of the following parameters:
# - esubm(i): Armington elasticities
# - esubd(i): domestic import elasticities
# - esubva(j): input elasticities between production factors
#
# Marius Braun, September 2022

install.packages("extrafont")
install.packages("Rcpp")
install.packages("openxlsx")
install.packages("moments")
install.packages("confintr")

library(ggplot2)
library(readr)
library(extrafont)
library(openxlsx)
library(Rcpp)
library(tictoc)
library(moments)
library(confintr)
font_import()
loadfonts(device = "win")

# set random seed (Mersenne prime number because why not)
set.seed(127)

# esubm(i): Armington elasticities
# load parameter space
paramspace_esubm_10p = read.csv("C:\\Users\\Marius Braun\\Sensitivity analyses\\esubm(i)\\parameter_space_esubm(i)_interval_10p.csv")

welf_esubm_10p = as.data.frame(matrix(, nrow = 1000, ncol = 6))
colnames(welf_esubm_10p) = c("policy_lo", "policy_mi", "policy_hi", "cbam_lo", "cbam_mi", "cbam_hi")

tic()
for(i in 1:nrow(paramspace_esubm_10p)){
    # load output file for specific set of parameter values
    # workaround to deal with trailing zeros and create correct filepaths
    mAGR = toString(ifelse(paramspace_esubm_10p[i, 1] %% 1 == 0, format(round(paramspace_esubm_10p[i, 1], 1), nsmall = 1), ifelse(paramspace_esubm_10p[i, 1] %% 0.1 == 0, format(round(paramspace_esubm_10p[i, 1], 1), nsmall = 1), ifelse(paramspace_esubm_10p[i, 1] %% 0.01 == 0, format(round(paramspace_esubm_10p[i, 1], 2), nsmall = 2), paramspace_esubm_10p[i, 1]))))
    mCOA = toString(ifelse(paramspace_esubm_10p[i, 2] %% 1 == 0, format(round(paramspace_esubm_10p[i, 2], 1), nsmall = 1), ifelse(paramspace_esubm_10p[i, 2] %% 0.1 == 0, format(round(paramspace_esubm_10p[i, 2], 1), nsmall = 1), ifelse(paramspace_esubm_10p[i, 2] %% 0.01 == 0, format(round(paramspace_esubm_10p[i, 2], 2), nsmall = 2), paramspace_esubm_10p[i, 2]))))
    mCRU = toString(ifelse(paramspace_esubm_10p[i, 3] %% 1 == 0, format(round(paramspace_esubm_10p[i, 3], 1), nsmall = 1), ifelse(paramspace_esubm_10p[i, 3] %% 0.1 == 0, format(round(paramspace_esubm_10p[i, 3], 1), nsmall = 1), ifelse(paramspace_esubm_10p[i, 3] %% 0.01 == 0, format(round(paramspace_esubm_10p[i, 3], 2), nsmall = 2), paramspace_esubm_10p[i, 3]))))
    mNGA = toString(ifelse(paramspace_esubm_10p[i, 4] %% 1 == 0, format(round(paramspace_esubm_10p[i, 4], 1), nsmall = 1), ifelse(paramspace_esubm_10p[i, 4] %% 0.1 == 0, format(round(paramspace_esubm_10p[i, 4], 1), nsmall = 1), ifelse(paramspace_esubm_10p[i, 4] %% 0.01 == 0, format(round(paramspace_esubm_10p[i, 4], 2), nsmall = 2), paramspace_esubm_10p[i, 4]))))
    mPET = toString(ifelse(paramspace_esubm_10p[i, 5] %% 1 == 0, format(round(paramspace_esubm_10p[i, 5], 1), nsmall = 1), ifelse(paramspace_esubm_10p[i, 5] %% 0.1 == 0, format(round(paramspace_esubm_10p[i, 5], 1), nsmall = 1), ifelse(paramspace_esubm_10p[i, 5] %% 0.01 == 0, format(round(paramspace_esubm_10p[i, 5], 2), nsmall = 2), paramspace_esubm_10p[i, 5]))))
    mFOO = toString(ifelse(paramspace_esubm_10p[i, 6] %% 1 == 0, format(round(paramspace_esubm_10p[i, 6], 1), nsmall = 1), ifelse(paramspace_esubm_10p[i, 6] %% 0.1 == 0, format(round(paramspace_esubm_10p[i, 6], 1), nsmall = 1), ifelse(paramspace_esubm_10p[i, 6] %% 0.01 == 0, format(round(paramspace_esubm_10p[i, 6], 2), nsmall = 2), paramspace_esubm_10p[i, 6]))))
    mMIN = toString(ifelse(paramspace_esubm_10p[i, 7] %% 1 == 0, format(round(paramspace_esubm_10p[i, 7], 1), nsmall = 1), ifelse(paramspace_esubm_10p[i, 7] %% 0.1 == 0, format(round(paramspace_esubm_10p[i, 7], 1), nsmall = 1), ifelse(paramspace_esubm_10p[i, 7] %% 0.01 == 0, format(round(paramspace_esubm_10p[i, 7], 2), nsmall = 2), paramspace_esubm_10p[i, 7]))))
    mPAP = toString(ifelse(paramspace_esubm_10p[i, 8] %% 1 == 0, format(round(paramspace_esubm_10p[i, 8], 1), nsmall = 1), ifelse(paramspace_esubm_10p[i, 8] %% 0.1 == 0, format(round(paramspace_esubm_10p[i, 8], 1), nsmall = 1), ifelse(paramspace_esubm_10p[i, 8] %% 0.01 == 0, format(round(paramspace_esubm_10p[i, 8], 2), nsmall = 2), paramspace_esubm_10p[i, 8]))))
    mCHE = toString(ifelse(paramspace_esubm_10p[i, 9] %% 1 == 0, format(round(paramspace_esubm_10p[i, 9], 1), nsmall = 1), ifelse(paramspace_esubm_10p[i, 9] %% 0.1 == 0, format(round(paramspace_esubm_10p[i, 9], 1), nsmall = 1), ifelse(paramspace_esubm_10p[i, 9] %% 0.01 == 0, format(round(paramspace_esubm_10p[i, 9], 2), nsmall = 2), paramspace_esubm_10p[i, 9]))))
    mNMM = toString(ifelse(paramspace_esubm_10p[i, 10] %% 1 == 0, format(round(paramspace_esubm_10p[i, 10], 1), nsmall = 1), ifelse(paramspace_esubm_10p[i, 10] %% 0.1 == 0, format(round(paramspace_esubm_10p[i, 10], 1), nsmall = 1), ifelse(paramspace_esubm_10p[i, 10] %% 0.01 == 0, format(round(paramspace_esubm_10p[i, 10], 2), nsmall = 2), paramspace_esubm_10p[i, 10]))))
    mIRS = toString(ifelse(paramspace_esubm_10p[i, 11] %% 1 == 0, format(round(paramspace_esubm_10p[i, 11], 1), nsmall = 1), ifelse(paramspace_esubm_10p[i, 11] %% 0.1 == 0, format(round(paramspace_esubm_10p[i, 11], 1), nsmall = 1), ifelse(paramspace_esubm_10p[i, 11] %% 0.01 == 0, format(round(paramspace_esubm_10p[i, 11], 2), nsmall = 2), paramspace_esubm_10p[i, 11]))))
    mNFM = toString(ifelse(paramspace_esubm_10p[i, 12] %% 1 == 0, format(round(paramspace_esubm_10p[i, 12], 1), nsmall = 1), ifelse(paramspace_esubm_10p[i, 12] %% 0.1 == 0, format(round(paramspace_esubm_10p[i, 12], 1), nsmall = 1), ifelse(paramspace_esubm_10p[i, 12] %% 0.01 == 0, format(round(paramspace_esubm_10p[i, 12], 2), nsmall = 2), paramspace_esubm_10p[i, 12]))))
    mMAN = toString(ifelse(paramspace_esubm_10p[i, 13] %% 1 == 0, format(round(paramspace_esubm_10p[i, 13], 1), nsmall = 1), ifelse(paramspace_esubm_10p[i, 13] %% 0.1 == 0, format(round(paramspace_esubm_10p[i, 13], 1), nsmall = 1), ifelse(paramspace_esubm_10p[i, 13] %% 0.01 == 0, format(round(paramspace_esubm_10p[i, 13], 2), nsmall = 2), paramspace_esubm_10p[i, 13]))))
    mELE = toString(ifelse(paramspace_esubm_10p[i, 14] %% 1 == 0, format(round(paramspace_esubm_10p[i, 14], 1), nsmall = 1), ifelse(paramspace_esubm_10p[i, 14] %% 0.1 == 0, format(round(paramspace_esubm_10p[i, 14], 1), nsmall = 1), ifelse(paramspace_esubm_10p[i, 14] %% 0.01 == 0, format(round(paramspace_esubm_10p[i, 14], 2), nsmall = 2), paramspace_esubm_10p[i, 14]))))
    mTRN = toString(ifelse(paramspace_esubm_10p[i, 15] %% 1 == 0, format(round(paramspace_esubm_10p[i, 15], 1), nsmall = 1), ifelse(paramspace_esubm_10p[i, 15] %% 0.1 == 0, format(round(paramspace_esubm_10p[i, 15], 1), nsmall = 1), ifelse(paramspace_esubm_10p[i, 15] %% 0.01 == 0, format(round(paramspace_esubm_10p[i, 15], 2), nsmall = 2), paramspace_esubm_10p[i, 15]))))
    mCON = toString(ifelse(paramspace_esubm_10p[i, 16] %% 1 == 0, format(round(paramspace_esubm_10p[i, 16], 1), nsmall = 1), ifelse(paramspace_esubm_10p[i, 16] %% 0.1 == 0, format(round(paramspace_esubm_10p[i, 16], 1), nsmall = 1), ifelse(paramspace_esubm_10p[i, 16] %% 0.01 == 0, format(round(paramspace_esubm_10p[i, 16], 2), nsmall = 2), paramspace_esubm_10p[i, 16]))))
    mSER = toString(ifelse(paramspace_esubm_10p[i, 17] %% 1 == 0, format(round(paramspace_esubm_10p[i, 17], 1), nsmall = 1), ifelse(paramspace_esubm_10p[i, 17] %% 0.1 == 0, format(round(paramspace_esubm_10p[i, 17], 1), nsmall = 1), ifelse(paramspace_esubm_10p[i, 17] %% 0.01 == 0, format(round(paramspace_esubm_10p[i, 17], 2), nsmall = 2), paramspace_esubm_10p[i, 17]))))
    f = paste0("C:\\Users\\Marius Braun\\Sensitivity analyses\\esubm(i)\\mAGR=", mAGR, "\\", "mCOA=", mCOA, "\\", "mCRU=", mCRU, "\\", "mNGA=", mNGA, "\\", "mPET=", mPET, "\\", "mFOO=", mFOO, "\\", "mMIN=", mMIN, "\\", "mPAP=", mPAP, "\\", "mCHE=", mCHE, "\\", "mNMM=", mNMM, "\\", "mIRS=", mIRS, "\\", "mNFM=", mNFM, "\\", "mMAN=", mMAN, "\\", "mELE=", mELE, "\\", "mTRN=", mTRN, "\\", "mCON=", mCON, "\\", "mSER=", mSER, "\\output.xlsx")
    if(file.exists(f)){
        welfare = read.xlsx(paste0("C:\\Users\\Marius Braun\\Sensitivity analyses\\esubm(i)\\mAGR=", mAGR, "\\", "mCOA=", mCOA, "\\", "mCRU=", mCRU, "\\", "mNGA=", mNGA, "\\", "mPET=", mPET, "\\", "mFOO=", mFOO, "\\", "mMIN=", mMIN, "\\", "mPAP=", mPAP, "\\", "mCHE=", mCHE, "\\", "mNMM=", mNMM, "\\", "mIRS=", mIRS, "\\", "mNFM=", mNFM, "\\", "mMAN=", mMAN, "\\", "mELE=", mELE, "\\", "mTRN=", mTRN, "\\", "mCON=", mCON, "\\", "mSER=", mSER, "\\output.xlsx"), 135)
        # get welfare effects
        welf_esubm_10p[i, 1] = welfare[10, 4]
        welf_esubm_10p[i, 2] = welfare[19, 4]
        welf_esubm_10p[i, 3] = welfare[28, 4]
        welf_esubm_10p[i, 4] = welfare[11, 4]
        welf_esubm_10p[i, 5] = welfare[20, 4]
        welf_esubm_10p[i, 6] = welfare[29, 4]
    }
    
    # remove variables to keep workspace clean
    remove(mAGR, mCOA, mCRU, mNGA, mPET, mFOO, mMIN, mPAP, mCHE, mNMM, mIRS, mNFM, mMAN, mELE, mTRN, mCON, mSER, welfare, f)
}
remove(i)
toc()

# welfare effects for random draws from +- 10 percent interval
# welf_esubm_10p = read.csv("C:\\Users\\Marius Braun\\Sensitivity analyses\\Plots\\welf_esubm(i)_10p.csv")
welf_esubm_10p$policy_lo = as.numeric(welf_esubm_10p$policy_lo) * 100
welf_esubm_10p$policy_mi = as.numeric(welf_esubm_10p$policy_mi) * 100
welf_esubm_10p$policy_hi = as.numeric(welf_esubm_10p$policy_hi) * 100
welf_esubm_10p$cbam_lo = as.numeric(welf_esubm_10p$cbam_lo) * 100
welf_esubm_10p$cbam_mi = as.numeric(welf_esubm_10p$cbam_mi) * 100
welf_esubm_10p$cbam_hi = as.numeric(welf_esubm_10p$cbam_hi) * 100



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



# esubd(i): domestic import elasticities
# load parameter space
paramspace_esubd_10p = read.csv("C:\\Users\\Marius Braun\\Sensitivity analyses\\esubd(i)\\parameter_space_esubd(i)_interval_10p.csv")

welf_esubd_10p = as.data.frame(matrix(, nrow = 1000, ncol = 6))
colnames(welf_esubd_10p) = c("policy_lo", "policy_mi", "policy_hi", "cbam_lo", "cbam_mi", "cbam_hi")

tic()
for(i in 1:nrow(paramspace_esubd_10p)){
    # load output file for specific set of parameter values
    # workaround to deal with trailing zeros and create correct filepaths
    dAGR = toString(ifelse(paramspace_esubd_10p[i, 1] %% 1 == 0, format(round(paramspace_esubd_10p[i, 1], 1), nsmall = 1), ifelse(paramspace_esubd_10p[i, 1] %% 0.1 == 0, format(round(paramspace_esubd_10p[i, 1], 1), nsmall = 1), ifelse(paramspace_esubd_10p[i, 1] %% 0.01 == 0, format(round(paramspace_esubd_10p[i, 1], 2), nsmall = 2), paramspace_esubd_10p[i, 1]))))
    dCOA = toString(ifelse(paramspace_esubd_10p[i, 2] %% 1 == 0, format(round(paramspace_esubd_10p[i, 2], 1), nsmall = 1), ifelse(paramspace_esubd_10p[i, 2] %% 0.1 == 0, format(round(paramspace_esubd_10p[i, 2], 1), nsmall = 1), ifelse(paramspace_esubd_10p[i, 2] %% 0.01 == 0, format(round(paramspace_esubd_10p[i, 2], 2), nsmall = 2), paramspace_esubd_10p[i, 2]))))
    dCRU = toString(ifelse(paramspace_esubd_10p[i, 3] %% 1 == 0, format(round(paramspace_esubd_10p[i, 3], 1), nsmall = 1), ifelse(paramspace_esubd_10p[i, 3] %% 0.1 == 0, format(round(paramspace_esubd_10p[i, 3], 1), nsmall = 1), ifelse(paramspace_esubd_10p[i, 3] %% 0.01 == 0, format(round(paramspace_esubd_10p[i, 3], 2), nsmall = 2), paramspace_esubd_10p[i, 3]))))
    dNGA = toString(ifelse(paramspace_esubd_10p[i, 4] %% 1 == 0, format(round(paramspace_esubd_10p[i, 4], 1), nsmall = 1), ifelse(paramspace_esubd_10p[i, 4] %% 0.1 == 0, format(round(paramspace_esubd_10p[i, 4], 1), nsmall = 1), ifelse(paramspace_esubd_10p[i, 4] %% 0.01 == 0, format(round(paramspace_esubd_10p[i, 4], 2), nsmall = 2), paramspace_esubd_10p[i, 4]))))
    dPET = toString(ifelse(paramspace_esubd_10p[i, 5] %% 1 == 0, format(round(paramspace_esubd_10p[i, 5], 1), nsmall = 1), ifelse(paramspace_esubd_10p[i, 5] %% 0.1 == 0, format(round(paramspace_esubd_10p[i, 5], 1), nsmall = 1), ifelse(paramspace_esubd_10p[i, 5] %% 0.01 == 0, format(round(paramspace_esubd_10p[i, 5], 2), nsmall = 2), paramspace_esubd_10p[i, 5]))))
    dFOO = toString(ifelse(paramspace_esubd_10p[i, 6] %% 1 == 0, format(round(paramspace_esubd_10p[i, 6], 1), nsmall = 1), ifelse(paramspace_esubd_10p[i, 6] %% 0.1 == 0, format(round(paramspace_esubd_10p[i, 6], 1), nsmall = 1), ifelse(paramspace_esubd_10p[i, 6] %% 0.01 == 0, format(round(paramspace_esubd_10p[i, 6], 2), nsmall = 2), paramspace_esubd_10p[i, 6]))))
    dMIN = toString(ifelse(paramspace_esubd_10p[i, 7] %% 1 == 0, format(round(paramspace_esubd_10p[i, 7], 1), nsmall = 1), ifelse(paramspace_esubd_10p[i, 7] %% 0.1 == 0, format(round(paramspace_esubd_10p[i, 7], 1), nsmall = 1), ifelse(paramspace_esubd_10p[i, 7] %% 0.01 == 0, format(round(paramspace_esubd_10p[i, 7], 2), nsmall = 2), paramspace_esubd_10p[i, 7]))))
    dPAP = toString(ifelse(paramspace_esubd_10p[i, 8] %% 1 == 0, format(round(paramspace_esubd_10p[i, 8], 1), nsmall = 1), ifelse(paramspace_esubd_10p[i, 8] %% 0.1 == 0, format(round(paramspace_esubd_10p[i, 8], 1), nsmall = 1), ifelse(paramspace_esubd_10p[i, 8] %% 0.01 == 0, format(round(paramspace_esubd_10p[i, 8], 2), nsmall = 2), paramspace_esubd_10p[i, 8]))))
    dCHE = toString(ifelse(paramspace_esubd_10p[i, 9] %% 1 == 0, format(round(paramspace_esubd_10p[i, 9], 1), nsmall = 1), ifelse(paramspace_esubd_10p[i, 9] %% 0.1 == 0, format(round(paramspace_esubd_10p[i, 9], 1), nsmall = 1), ifelse(paramspace_esubd_10p[i, 9] %% 0.01 == 0, format(round(paramspace_esubd_10p[i, 9], 2), nsmall = 2), paramspace_esubd_10p[i, 9]))))
    dNMM = toString(ifelse(paramspace_esubd_10p[i, 10] %% 1 == 0, format(round(paramspace_esubd_10p[i, 10], 1), nsmall = 1), ifelse(paramspace_esubd_10p[i, 10] %% 0.1 == 0, format(round(paramspace_esubd_10p[i, 10], 1), nsmall = 1), ifelse(paramspace_esubd_10p[i, 10] %% 0.01 == 0, format(round(paramspace_esubd_10p[i, 10], 2), nsmall = 2), paramspace_esubd_10p[i, 10]))))
    dIRS = toString(ifelse(paramspace_esubd_10p[i, 11] %% 1 == 0, format(round(paramspace_esubd_10p[i, 11], 1), nsmall = 1), ifelse(paramspace_esubd_10p[i, 11] %% 0.1 == 0, format(round(paramspace_esubd_10p[i, 11], 1), nsmall = 1), ifelse(paramspace_esubd_10p[i, 11] %% 0.01 == 0, format(round(paramspace_esubd_10p[i, 11], 2), nsmall = 2), paramspace_esubd_10p[i, 11]))))
    dNFM = toString(ifelse(paramspace_esubd_10p[i, 12] %% 1 == 0, format(round(paramspace_esubd_10p[i, 12], 1), nsmall = 1), ifelse(paramspace_esubd_10p[i, 12] %% 0.1 == 0, format(round(paramspace_esubd_10p[i, 12], 1), nsmall = 1), ifelse(paramspace_esubd_10p[i, 12] %% 0.01 == 0, format(round(paramspace_esubd_10p[i, 12], 2), nsmall = 2), paramspace_esubd_10p[i, 12]))))
    dMAN = toString(ifelse(paramspace_esubd_10p[i, 13] %% 1 == 0, format(round(paramspace_esubd_10p[i, 13], 1), nsmall = 1), ifelse(paramspace_esubd_10p[i, 13] %% 0.1 == 0, format(round(paramspace_esubd_10p[i, 13], 1), nsmall = 1), ifelse(paramspace_esubd_10p[i, 13] %% 0.01 == 0, format(round(paramspace_esubd_10p[i, 13], 2), nsmall = 2), paramspace_esubd_10p[i, 13]))))
    dELE = toString(ifelse(paramspace_esubd_10p[i, 14] %% 1 == 0, format(round(paramspace_esubd_10p[i, 14], 1), nsmall = 1), ifelse(paramspace_esubd_10p[i, 14] %% 0.1 == 0, format(round(paramspace_esubd_10p[i, 14], 1), nsmall = 1), ifelse(paramspace_esubd_10p[i, 14] %% 0.01 == 0, format(round(paramspace_esubd_10p[i, 14], 2), nsmall = 2), paramspace_esubd_10p[i, 14]))))
    dTRN = toString(ifelse(paramspace_esubd_10p[i, 15] %% 1 == 0, format(round(paramspace_esubd_10p[i, 15], 1), nsmall = 1), ifelse(paramspace_esubd_10p[i, 15] %% 0.1 == 0, format(round(paramspace_esubd_10p[i, 15], 1), nsmall = 1), ifelse(paramspace_esubd_10p[i, 15] %% 0.01 == 0, format(round(paramspace_esubd_10p[i, 15], 2), nsmall = 2), paramspace_esubd_10p[i, 15]))))
    dCON = toString(ifelse(paramspace_esubd_10p[i, 16] %% 1 == 0, format(round(paramspace_esubd_10p[i, 16], 1), nsmall = 1), ifelse(paramspace_esubd_10p[i, 16] %% 0.1 == 0, format(round(paramspace_esubd_10p[i, 16], 1), nsmall = 1), ifelse(paramspace_esubd_10p[i, 16] %% 0.01 == 0, format(round(paramspace_esubd_10p[i, 16], 2), nsmall = 2), paramspace_esubd_10p[i, 16]))))
    dSER = toString(ifelse(paramspace_esubd_10p[i, 17] %% 1 == 0, format(round(paramspace_esubd_10p[i, 17], 1), nsmall = 1), ifelse(paramspace_esubd_10p[i, 17] %% 0.1 == 0, format(round(paramspace_esubd_10p[i, 17], 1), nsmall = 1), ifelse(paramspace_esubd_10p[i, 17] %% 0.01 == 0, format(round(paramspace_esubd_10p[i, 17], 2), nsmall = 2), paramspace_esubd_10p[i, 17]))))
    f = paste0("C:\\Users\\Marius Braun\\Sensitivity analyses\\esubd(i)\\dAGR=", dAGR, "\\", "dCOA=", dCOA, "\\", "dCRU=", dCRU, "\\", "dNGA=", dNGA, "\\", "dPET=", dPET, "\\", "dFOO=", dFOO, "\\", "dMIN=", dMIN, "\\", "dPAP=", dPAP, "\\", "dCHE=", dCHE, "\\", "dNMM=", dNMM, "\\", "dIRS=", dIRS, "\\", "dNFM=", dNFM, "\\", "dMAN=", dMAN, "\\", "dELE=", dELE, "\\", "dTRN=", dTRN, "\\", "dCON=", dCON, "\\", "dSER=", dSER, "\\output.xlsx")
    if(file.exists(f)){
        welfare = read.xlsx(paste0("C:\\Users\\Marius Braun\\Sensitivity analyses\\esubd(i)\\dAGR=", dAGR, "\\", "dCOA=", dCOA, "\\", "dCRU=", dCRU, "\\", "dNGA=", dNGA, "\\", "dPET=", dPET, "\\", "dFOO=", dFOO, "\\", "dMIN=", dMIN, "\\", "dPAP=", dPAP, "\\", "dCHE=", dCHE, "\\", "dNMM=", dNMM, "\\", "dIRS=", dIRS, "\\", "dNFM=", dNFM, "\\", "dMAN=", dMAN, "\\", "dELE=", dELE, "\\", "dTRN=", dTRN, "\\", "dCON=", dCON, "\\", "dSER=", dSER, "\\output.xlsx"), 135)
        # get welfare effects
        welf_esubd_10p[i, 1] = welfare[10, 4]
        welf_esubd_10p[i, 2] = welfare[19, 4]
        welf_esubd_10p[i, 3] = welfare[28, 4]
        welf_esubd_10p[i, 4] = welfare[11, 4]
        welf_esubd_10p[i, 5] = welfare[20, 4]
        welf_esubd_10p[i, 6] = welfare[29, 4]
    }
    
    # remove variables to keep workspace clean
    remove(dAGR, dCOA, dCRU, dNGA, dPET, dFOO, dMIN, dPAP, dCHE, dNMM, dIRS, dNFM, dMAN, dELE, dTRN, dCON, dSER, welfare, f)
}
remove(i)
toc()

# welfare effects for random draws from +- 10 percent interval
# welf_esubd_10p = read.csv("C:\\Users\\Marius Braun\\Sensitivity analyses\\Plots\\welf_esubd(i)_10p.csv")
welf_esubd_10p$policy_lo = as.numeric(welf_esubd_10p$policy_lo) * 100
welf_esubd_10p$policy_mi = as.numeric(welf_esubd_10p$policy_mi) * 100
welf_esubd_10p$policy_hi = as.numeric(welf_esubd_10p$policy_hi) * 100
welf_esubd_10p$cbam_lo = as.numeric(welf_esubd_10p$cbam_lo) * 100
welf_esubd_10p$cbam_mi = as.numeric(welf_esubd_10p$cbam_mi) * 100
welf_esubd_10p$cbam_hi = as.numeric(welf_esubd_10p$cbam_hi) * 100



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




# esubva(j): input elasticities between production factors
# load parameter space
paramspace_esubva_10p = read.csv("C:\\Users\\Marius Braun\\Sensitivity analyses\\esubva(j)\\parameter_space_esubva(j)_interval_10p.csv")

welf_esubva_10p = as.data.frame(matrix(, nrow = 100, ncol = 6))
colnames(welf_esubva_10p) = c("policy_lo", "policy_mi", "policy_hi", "cbam_lo", "cbam_mi", "cbam_hi")

tic()
for(i in 1:nrow(paramspace_esubva_10p)){
    # load output file for specific set of parameter values
    # workaround to deal with trailing zeros and create correct filepaths
    vAGR = toString(ifelse(paramspace_esubva_10p[i, 1] %% 1 == 0, format(round(paramspace_esubva_10p[i, 1], 1), nsmall = 1), ifelse(paramspace_esubva_10p[i, 1] %% 0.1 == 0, format(round(paramspace_esubva_10p[i, 1], 1), nsmall = 1), ifelse(paramspace_esubva_10p[i, 1] %% 0.01 == 0, format(round(paramspace_esubva_10p[i, 1], 2), nsmall = 2), paramspace_esubva_10p[i, 1]))))
    vCOA = toString(ifelse(paramspace_esubva_10p[i, 2] %% 1 == 0, format(round(paramspace_esubva_10p[i, 2], 1), nsmall = 1), ifelse(paramspace_esubva_10p[i, 2] %% 0.1 == 0, format(round(paramspace_esubva_10p[i, 2], 1), nsmall = 1), ifelse(paramspace_esubva_10p[i, 2] %% 0.01 == 0, format(round(paramspace_esubva_10p[i, 2], 2), nsmall = 2), paramspace_esubva_10p[i, 2]))))
    vCRU = toString(ifelse(paramspace_esubva_10p[i, 3] %% 1 == 0, format(round(paramspace_esubva_10p[i, 3], 1), nsmall = 1), ifelse(paramspace_esubva_10p[i, 3] %% 0.1 == 0, format(round(paramspace_esubva_10p[i, 3], 1), nsmall = 1), ifelse(paramspace_esubva_10p[i, 3] %% 0.01 == 0, format(round(paramspace_esubva_10p[i, 3], 2), nsmall = 2), paramspace_esubva_10p[i, 3]))))
    vNGA = toString(ifelse(paramspace_esubva_10p[i, 4] %% 1 == 0, format(round(paramspace_esubva_10p[i, 4], 1), nsmall = 1), ifelse(paramspace_esubva_10p[i, 4] %% 0.1 == 0, format(round(paramspace_esubva_10p[i, 4], 1), nsmall = 1), ifelse(paramspace_esubva_10p[i, 4] %% 0.01 == 0, format(round(paramspace_esubva_10p[i, 4], 2), nsmall = 2), paramspace_esubva_10p[i, 4]))))
    vPET = toString(ifelse(paramspace_esubva_10p[i, 5] %% 1 == 0, format(round(paramspace_esubva_10p[i, 5], 1), nsmall = 1), ifelse(paramspace_esubva_10p[i, 5] %% 0.1 == 0, format(round(paramspace_esubva_10p[i, 5], 1), nsmall = 1), ifelse(paramspace_esubva_10p[i, 5] %% 0.01 == 0, format(round(paramspace_esubva_10p[i, 5], 2), nsmall = 2), paramspace_esubva_10p[i, 5]))))
    vFOO = toString(ifelse(paramspace_esubva_10p[i, 6] %% 1 == 0, format(round(paramspace_esubva_10p[i, 6], 1), nsmall = 1), ifelse(paramspace_esubva_10p[i, 6] %% 0.1 == 0, format(round(paramspace_esubva_10p[i, 6], 1), nsmall = 1), ifelse(paramspace_esubva_10p[i, 6] %% 0.01 == 0, format(round(paramspace_esubva_10p[i, 6], 2), nsmall = 2), paramspace_esubva_10p[i, 6]))))
    vMIN = toString(ifelse(paramspace_esubva_10p[i, 7] %% 1 == 0, format(round(paramspace_esubva_10p[i, 7], 1), nsmall = 1), ifelse(paramspace_esubva_10p[i, 7] %% 0.1 == 0, format(round(paramspace_esubva_10p[i, 7], 1), nsmall = 1), ifelse(paramspace_esubva_10p[i, 7] %% 0.01 == 0, format(round(paramspace_esubva_10p[i, 7], 2), nsmall = 2), paramspace_esubva_10p[i, 7]))))
    vPAP = toString(ifelse(paramspace_esubva_10p[i, 8] %% 1 == 0, format(round(paramspace_esubva_10p[i, 8], 1), nsmall = 1), ifelse(paramspace_esubva_10p[i, 8] %% 0.1 == 0, format(round(paramspace_esubva_10p[i, 8], 1), nsmall = 1), ifelse(paramspace_esubva_10p[i, 8] %% 0.01 == 0, format(round(paramspace_esubva_10p[i, 8], 2), nsmall = 2), paramspace_esubva_10p[i, 8]))))
    vCHE = toString(ifelse(paramspace_esubva_10p[i, 9] %% 1 == 0, format(round(paramspace_esubva_10p[i, 9], 1), nsmall = 1), ifelse(paramspace_esubva_10p[i, 9] %% 0.1 == 0, format(round(paramspace_esubva_10p[i, 9], 1), nsmall = 1), ifelse(paramspace_esubva_10p[i, 9] %% 0.01 == 0, format(round(paramspace_esubva_10p[i, 9], 2), nsmall = 2), paramspace_esubva_10p[i, 9]))))
    vNMM = toString(ifelse(paramspace_esubva_10p[i, 10] %% 1 == 0, format(round(paramspace_esubva_10p[i, 10], 1), nsmall = 1), ifelse(paramspace_esubva_10p[i, 10] %% 0.1 == 0, format(round(paramspace_esubva_10p[i, 10], 1), nsmall = 1), ifelse(paramspace_esubva_10p[i, 10] %% 0.01 == 0, format(round(paramspace_esubva_10p[i, 10], 2), nsmall = 2), paramspace_esubva_10p[i, 10]))))
    vIRS = toString(ifelse(paramspace_esubva_10p[i, 11] %% 1 == 0, format(round(paramspace_esubva_10p[i, 11], 1), nsmall = 1), ifelse(paramspace_esubva_10p[i, 11] %% 0.1 == 0, format(round(paramspace_esubva_10p[i, 11], 1), nsmall = 1), ifelse(paramspace_esubva_10p[i, 11] %% 0.01 == 0, format(round(paramspace_esubva_10p[i, 11], 2), nsmall = 2), paramspace_esubva_10p[i, 11]))))
    vNFM = toString(ifelse(paramspace_esubva_10p[i, 12] %% 1 == 0, format(round(paramspace_esubva_10p[i, 12], 1), nsmall = 1), ifelse(paramspace_esubva_10p[i, 12] %% 0.1 == 0, format(round(paramspace_esubva_10p[i, 12], 1), nsmall = 1), ifelse(paramspace_esubva_10p[i, 12] %% 0.01 == 0, format(round(paramspace_esubva_10p[i, 12], 2), nsmall = 2), paramspace_esubva_10p[i, 12]))))
    vMAN = toString(ifelse(paramspace_esubva_10p[i, 13] %% 1 == 0, format(round(paramspace_esubva_10p[i, 13], 1), nsmall = 1), ifelse(paramspace_esubva_10p[i, 13] %% 0.1 == 0, format(round(paramspace_esubva_10p[i, 13], 1), nsmall = 1), ifelse(paramspace_esubva_10p[i, 13] %% 0.01 == 0, format(round(paramspace_esubva_10p[i, 13], 2), nsmall = 2), paramspace_esubva_10p[i, 13]))))
    vELE = toString(ifelse(paramspace_esubva_10p[i, 14] %% 1 == 0, format(round(paramspace_esubva_10p[i, 14], 1), nsmall = 1), ifelse(paramspace_esubva_10p[i, 14] %% 0.1 == 0, format(round(paramspace_esubva_10p[i, 14], 1), nsmall = 1), ifelse(paramspace_esubva_10p[i, 14] %% 0.01 == 0, format(round(paramspace_esubva_10p[i, 14], 2), nsmall = 2), paramspace_esubva_10p[i, 14]))))
    vTRN = toString(ifelse(paramspace_esubva_10p[i, 15] %% 1 == 0, format(round(paramspace_esubva_10p[i, 15], 1), nsmall = 1), ifelse(paramspace_esubva_10p[i, 15] %% 0.1 == 0, format(round(paramspace_esubva_10p[i, 15], 1), nsmall = 1), ifelse(paramspace_esubva_10p[i, 15] %% 0.01 == 0, format(round(paramspace_esubva_10p[i, 15], 2), nsmall = 2), paramspace_esubva_10p[i, 15]))))
    vCON = toString(ifelse(paramspace_esubva_10p[i, 16] %% 1 == 0, format(round(paramspace_esubva_10p[i, 16], 1), nsmall = 1), ifelse(paramspace_esubva_10p[i, 16] %% 0.1 == 0, format(round(paramspace_esubva_10p[i, 16], 1), nsmall = 1), ifelse(paramspace_esubva_10p[i, 16] %% 0.01 == 0, format(round(paramspace_esubva_10p[i, 16], 2), nsmall = 2), paramspace_esubva_10p[i, 16]))))
    vSER = toString(ifelse(paramspace_esubva_10p[i, 17] %% 1 == 0, format(round(paramspace_esubva_10p[i, 17], 1), nsmall = 1), ifelse(paramspace_esubva_10p[i, 17] %% 0.1 == 0, format(round(paramspace_esubva_10p[i, 17], 1), nsmall = 1), ifelse(paramspace_esubva_10p[i, 17] %% 0.01 == 0, format(round(paramspace_esubva_10p[i, 17], 2), nsmall = 2), paramspace_esubva_10p[i, 17]))))
    f = paste0("C:\\Users\\Marius Braun\\Sensitivity analyses\\esubva(j)\\vAGR=", vAGR, "\\", "vCOA=", vCOA, "\\", "vCRU=", vCRU, "\\", "vNGA=", vNGA, "\\", "vPET=", vPET, "\\", "vFOO=", vFOO, "\\", "vMIN=", vMIN, "\\", "vPAP=", vPAP, "\\", "vCHE=", vCHE, "\\", "vNMM=", vNMM, "\\", "vIRS=", vIRS, "\\", "vNFM=", vNFM, "\\", "vMAN=", vMAN, "\\", "vELE=", vELE, "\\", "vTRN=", vTRN, "\\", "vCON=", vCON, "\\", "vSER=", vSER, "\\output.xlsx")
    if(file.exists(f)){
        welfare = read.xlsx(paste0("C:\\Users\\Marius Braun\\Sensitivity analyses\\esubva(j)\\vAGR=", vAGR, "\\", "vCOA=", vCOA, "\\", "vCRU=", vCRU, "\\", "vNGA=", vNGA, "\\", "vPET=", vPET, "\\", "vFOO=", vFOO, "\\", "vMIN=", vMIN, "\\", "vPAP=", vPAP, "\\", "vCHE=", vCHE, "\\", "vNMM=", vNMM, "\\", "vIRS=", vIRS, "\\", "vNFM=", vNFM, "\\", "vMAN=", vMAN, "\\", "vELE=", vELE, "\\", "vTRN=", vTRN, "\\", "vCON=", vCON, "\\", "vSER=", vSER, "\\output.xlsx"), 135)
        # get welfare effects
        welf_esubva_10p[i, 1] = welfare[10, 4]
        welf_esubva_10p[i, 2] = welfare[19, 4]
        welf_esubva_10p[i, 3] = welfare[28, 4]
        welf_esubva_10p[i, 4] = welfare[11, 4]
        welf_esubva_10p[i, 5] = welfare[20, 4]
        welf_esubva_10p[i, 6] = welfare[28, 4]
    }
    
    # remove variables to keep workspace clean
    remove(vAGR, vCOA, vCRU, vNGA, vPET, vFOO, vMIN, vPAP, vCHE, vNMM, vIRS, vNFM, vMAN, vELE, vTRN, vCON, vSER, welfare, f)
}
remove(i)
toc()

# welfare effects for random draws from +- 10 percent interval
welf_esubva_10p$policy_lo = as.numeric(welf_esubva_10p$policy_lo) * 100
welf_esubva_10p$policy_mi = as.numeric(welf_esubva_10p$policy_mi) * 100
welf_esubva_10p$policy_hi = as.numeric(welf_esubva_10p$policy_hi) * 100
welf_esubva_10p$cbam_lo = as.numeric(welf_esubva_10p$cbam_lo) * 100
welf_esubva_10p$cbam_mi = as.numeric(welf_esubva_10p$cbam_mi) * 100
welf_esubva_10p$cbam_hi = as.numeric(welf_esubva_10p$cbam_hi) * 100

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