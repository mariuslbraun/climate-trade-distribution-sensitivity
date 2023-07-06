This repo contains R code for the sensitivity analysis of the CGE model developed in Hübler et al. (2022). Using household data from the German Income and Expenditure Survey, we analyze distributional effects of climate policy 
in Germany in a CGE model calibrated to GTAP 10 data. We find that the negative consumption effect of CO2 pricing is highest for the low-income group, whereas the negative income effect is highest for the high-income group and exceeds
the consumption effect. The low-income group benefits most from (per capita-based redistribution of) carbon pricing revenues and receives social transfers such that poor households can be better off with such climate policies than without them.

To check the robustness of our findings, we conduct a distributional sensitivity analysis of relevant sets of elasticity parameter values within our CGE model. In this repo, we provide R code for the generating parameter spaces for the sensitivity analysis,
processing the output files from the model runs and generating histograms of the resulting welfare effect distributions.

## 1. Setting up the R environment

First, open the R project file [climate-trade-distribution-sensitivity.Rproj](climate-trade-distribution-sensitivity.Rproj) in RStudio. It is important to work in the R project, as it is associated with the directory that the project is located in and will set the working directory accordingly.
After opening the R project file, open [setup.R](setup.R). Running the file will set up an R environment using the `renv` package, which manages project-local R dependencies to ensure that existing data analysis workflows work as they did before;
for more information see https://rstudio.github.io/renv/articles/renv.html

Proceed as follows:

1. Call `options(repos = c(REPO_NAME = "https://packagemanager.rstudio.com/cran/2023-03-17"))`
to load the CRAN snapshot from RStudio Package Manager. This will freeze to the R packages as they were available at the time that the analysis was conducted.

2. Install `renv` by calling `install.packages("renv")`.

3. Call `renv::init()` to initialize a new project-local environment with a private R library.
`bare = TRUE`: instead of installing dependencies automatically, we install packages manually.

4. Install the required packages.

5. Call `renv::snapshot()` to save the state of the project library.

The state of the library is saved in the [renv.lock](renv.lock) file.

## 2. Generating parameter spaces

[paramspaces.R](paramspaces.R) generates parameter spaces for the model runs of the sensitivity analysis. In particular, three sets of sector-level elasticity of substition parameters are considered, for which baseline values are
taken from Pothen and Hübler (2018):

- esubd(i): elasticities between domestically produced versus imported goods
- esubm(i): Armington elasticities
- esubva(j): elasticities between production factors

For each set of parameters, we generate 1000 random draws from a +-10 % interval around each of the sector-specific elasticities, resulting in 1000 sets of sectoral parameter values.
The parameter spaces as well as the baseline values with +-10 % intervals are stored as CSV files in [paramspaces](paramspaces).
