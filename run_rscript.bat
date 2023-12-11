Rscript -e 'renv::activate("climate-trade-distribution-sensitivity.Rproj")'

Rscript %batdir%scripts\welf_data.R

Rscript %batdir%scripts\figures.R