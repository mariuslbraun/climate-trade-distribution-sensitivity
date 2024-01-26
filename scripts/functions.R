# function to load file paths from sensitivity analysis output
load_filepaths = function(dir, param, filename_pattern) {
  filepaths = as.data.frame(
    list.files(
      file.path(
        dir,
        param
      ),
      pattern = filename_pattern,
      full.names = T,
      recursive = T
    )
  )
  colnames(filepaths) = "files"
  return(filepaths)
}

# function to load welfare effects from output files
load_welf_data = function(
  param,
  filenames,
  policies,
  inc_groups
) {
  # create empty dataframe to store welfare effects
  welf = as.data.frame(
    matrix(,
           nrow = nrow(filenames),
           ncol = length(policies) * length(inc_groups)
    )
  )

  # name columns of dataframe according to policy and income group
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
  if(param == "CO2factor" |
     param == "esub_cons") {
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
      tibble::rownames_to_column(var = param) %>%
      mutate_if(is.character, as.numeric)
  }
  return(welf)
}

# function to create welfare effects dataframe
make_welf_df = function(
  dir,
  param,
  policies,
  inc_groups
) {
  # load file paths of output files
  filenames = load_filepaths(
    dir,
    param,
    filename_pattern = "output.xlsx"
  )
  
  # load welfare effects from output files
  welf = load_welf_data(
    param,
    filenames,
    policies,
    inc_groups
  )
  
  # name welfare effects data frame
  welf_name = paste(
      "welf",
      param,
      sep = "_"
    )
  assign(
    x = welf_name,
    value = welf
  )
  
  # save welfare effects data frame as RDS file
  saveRDS(
    get(welf_name),
    paste0("prepared/", welf_name, ".rds")
  )
  return(get(welf_name))
}