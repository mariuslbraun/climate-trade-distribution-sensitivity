system.time({
    # activate renv
    renv::activate()

    # generate parameter spaces
    source("scripts/paramspaces.R")

    # pre-process welfare data
    source("scripts/welf_data.R")

    # generate figures
    source("scripts/figures.R")
})