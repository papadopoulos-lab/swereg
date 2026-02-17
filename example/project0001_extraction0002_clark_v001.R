project <- "0001"
extraction <- "0002"
person <- "clark"
tag <- "v001"

# ONLY FOR KRISTEN TO RUN
# devtools::install_github("epi-ai/swereg", upgrade = "never")

# ONLY FOR RICHARD TO RUN
# devtools::load_all("~/r-packages/swereg")

library(data.table)
library(magrittr)

# Loading in data and assigning output locations --------------------------
org::initialize_project(
  env = .GlobalEnv,
  folders_to_be_sourced = paste0("R_generic_", tag),
  home = c(
    "~/R Code/structural-gd-registry-data-2000-2023",
    "~/papadopoulos-lab/structural-gd-registry-data-2000-2023/"
  ),
  data_generic_skeleton = c(
    "//argos.rudbeck.uu.se/MyGroups$/Bronze/Postdoc_Kristen/structural-gd-registry-data-2000-2023/data_processed/",
    "/data/argos/Bronze/Postdoc_Kristen/structural-gd-registry-data-2000-2023/data_generic_skeleton/"
  ),
  data_project_specific = c(
    "//argos.rudbeck.uu.se/MyGroups$/Bronze/Postdoc_Kristen/structural-gd-registry-data-2000-2023/data_project_specific/",
    "/data/argos/Bronze/Postdoc_Kristen/structural-gd-registry-data-2000-2023/data_project_specific/"
  )
)

data_generic_skeleton <- fs::path(org::project$data_generic_skeleton, tag)


# identify all the skeleton2 batch files
files <- fs::dir_ls(data_generic_skeleton) %>%
  stringr::str_subset("skeleton2_clean_")

retval <- vector("list", length = length(files))
for(i in seq_along(retval)){
  cat(i, " - ", length(retval), "\n")
  skeleton <- qs::qread(files[i], nthreads = 4)

  skeleton <- skeleton[
    ,
    .(
      death_categorical = swereg::first_non_na(death_categorical),
      death_binary = swereg::first_non_na(death_binary),
      age = swereg::first_non_na(age),
      is_dead = swereg::first_non_na(is_dead)
  ),
  keyby=.(
    id,
    isoyear,
    rowind_register_tag_control2,
    rowind_register_tag_control1,
    rowind_shared_isoyear_first_gd,
    rowind_shared_age_first_gd,
    rowind_isoyear_death,
    rowind_shared_saab_of_case
  )]

  retval[[i]] <- skeleton
}

retval <- rbindlist(retval)

qs::qsave(
  retval,
  fs::path(
    org::project$data_project_specific,
    paste0("project", project, "_extraction", extraction, "_", person, "_", tag, ".qs")
  )
)


