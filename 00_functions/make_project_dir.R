make_project_dir <- function() {
  require(fs)

  dir_names <- c(
    "00_data",
    "00_scripts",
    "00_functions",
    "01_business_understanding",
    "02_data_understanding",
    "03_data_preperation",
    "04_modelling",
    "05_evaluation",
    "06_deployment"
  )

  dir_create(dir_names)

  dir_ls()
}
