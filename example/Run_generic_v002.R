tag <- "v002"

# ONLY FOR KRISTEN TO RUN
# devtools::install_github("epi-ai/swereg", upgrade = "never")

# ONLY FOR RICHARD TO RUN
# devtools::load_all("~/r-packages/swereg")

# Loading in data and assigning output locations --------------------------
org::initialize_project(
  env = .GlobalEnv,
  folders_to_be_sourced = paste0("R_generic_", tag),
  home = c(
    "~/R Code/structural-gd-registry-data-2000-2023",
    "~/papadopoulos-lab/structural-gd-registry-data-2000-2023/"
  ),
  data_structural = c(
    "C:/Users/kricl384/Documents/R Code/structural-gd-registry-data-2000-2023/data_structural",
    "~/papadopoulos-lab/structural-gd-registry-data-2000-2023/data_structural"
  ),
  data_raw = c(
    "//argos.rudbeck.uu.se/MyGroups$/Bronze/Postdoc_Kristen/structural-gd-registry-data-2000-2023/data_raw",
    "/data/argos/Bronze/Postdoc_Kristen/structural-gd-registry-data-2000-2023/data_raw"
  ),
  data_generic_skeleton = c(
    "//argos.rudbeck.uu.se/MyGroups$/Bronze/Postdoc_Kristen/structural-gd-registry-data-2000-2023/data_generic_skeleton/",
    "/data/argos/Bronze/Postdoc_Kristen/structural-gd-registry-data-2000-2023/data_generic_skeleton/"
  )
)

data_generic_skeleton <- fs::path(org::project$data_generic_skeleton, tag)
dir.create(data_generic_skeleton, showWarnings = FALSE, recursive = TRUE)

# load every single function (commands) into what is currently available in these named libraries
library(data.table)
library(ggplot2)
library(magrittr)

# KRISTEN STOP HERE IF DEBUGGING skeleton2_clean

large_files <- read_large_files()

# get the correct ids/lopnr
id_master_same <- large_files[["SCB/fp_lev_fall_och_kontroller_1.sas7bdat"]]
id_master_opposite <- large_files[["SCB/fp_lev_fall_och_kontroller_2.sas7bdat"]]
id_master_sibling <- large_files[["SCB/fp_lev_helsyskon.sas7bdat"]]
# We only allow CIS-sibling controls
id_master_sibling <- id_master_sibling[!lopnr_syskon %in% id_master_sibling$lopnr]
setnames(id_master_sibling, c("lopnr_fall", "lopnr_kontroll"))

id_master <- rbind(
  id_master_same[,.(lopnr_fall, lopnr_kontroll)],
  id_master_opposite[,.(lopnr_fall, lopnr_kontroll)],
  id_master_sibling[,.(lopnr_fall, lopnr_kontroll)]
)
id_master[,N := .N, by=.(lopnr_kontroll)]
id_master[, unique_id_for_control := lopnr_kontroll]
id_master[N>1, unique_id_for_control := as.numeric(paste0(lopnr_kontroll,"000",lopnr_fall))]

# cleaning of parents
id_parent <- large_files[["SCB/fp_lev_bioforaldrar.sas7bdat"]]
# remove parents who are cases
id_parent[id_parent$lopnr_far %in% id_master$lopnr_fall,]$lopnr_far <- NA
id_parent[id_parent$lopnr_mor %in% id_master$lopnr_fall,]$lopnr_mor <- NA

ids_gd_unsplit <- unique(c(id_master_same$lopnr_fall, id_master_opposite$lopnr_fall))
# ids_gd_unsplit <- unique(c(155509, 243, 192514, ids_gd_unsplit))
# ids_gd_unsplit <- c(12288, 51519, 13778, 503941)

ids_gd_split <- csutil::easy_split(ids_gd_unsplit, size_of_each_group = 50)
ids_gd <- vector("list", length = length(ids_gd_split))

number_of_batches_to_run <- length(ids_gd_split)
# if you just want to do a quick run of only 200 cases
# number_of_batches_to_run <- 2
 # 35297
# adding in the controls and the parents
for(i in seq_len(number_of_batches_to_run)){
  ids_gd[[i]] <- list()
  ids_gd[[i]]$case <- ids_gd_split[[i]]

  ids_gd[[i]]$control_same <- id_master_same[lopnr_fall %in% ids_gd[[i]]$case]$lopnr_kontroll %>%
    unique() %>%
    na.omit()
  ids_gd[[i]]$control_opposite <- id_master_opposite[lopnr_fall %in% ids_gd[[i]]$case]$lopnr_kontroll %>%
    unique() %>%
    na.omit()
  ids_gd[[i]]$control_sibling <- id_master_sibling[lopnr_fall %in% ids_gd[[i]]$case]$lopnr_kontroll %>%
    unique() %>%
    na.omit()

  ids_gd[[i]]$master_list_of_what_i_expect <- id_master[lopnr_fall %in% ids_gd[[i]]$case]

  # get parent data
  d_parent <- id_parent %>%
    dplyr::filter(lopnr %in% c(
      ids_gd[[i]]$case,
      ids_gd[[i]]$control_same,
      ids_gd[[i]]$control_opposite,
      ids_gd[[i]]$control_sibling
    ))
  # remove parents who are cases
  d_parent[d_parent$lopnr_far %in% ids_gd_unsplit,]$lopnr_far <- NA
  d_parent[d_parent$lopnr_mor %in% ids_gd_unsplit,]$lopnr_mor <- NA


  ids_gd[[i]]$mother <- d_parent$lopnr_mor %>%
    unique() %>%
    na.omit()

  ids_gd[[i]]$mother_matched_to_child <- d_parent[,.(lopnr, lopnr_mor)] %>%
    unique() %>%
    na.omit()
  # make sure controls are using unique_id_for_control
  ids_gd[[i]]$mother_matched_to_child[
    ids_gd[[i]]$master_list_of_what_i_expect,
    on = c("lopnr==lopnr_fall"),
    id_child := lopnr
  ]
  ids_gd[[i]]$mother_matched_to_child[!is.na(id_child)]
  ids_gd[[i]]$mother_matched_to_child[
    ids_gd[[i]]$master_list_of_what_i_expect,
    on = c("lopnr==lopnr_kontroll"),
    id_child := unique_id_for_control
  ]
  ids_gd[[i]]$mother_matched_to_child[is.na(id_child)]
  ids_gd[[i]]$mother_matched_to_child[, lopnr := NULL]
  ids_gd[[i]]$mother_matched_to_child[id_child==1816850001700]

  ids_gd[[i]]$father <- d_parent$lopnr_far %>%
    unique() %>%
    na.omit()

  ids_gd[[i]]$father_matched_to_child <- d_parent[,.(lopnr, lopnr_far)] %>%
    unique() %>%
    na.omit()
  # make sure controls are using unique_id_for_control
  ids_gd[[i]]$father_matched_to_child[
    ids_gd[[i]]$master_list_of_what_i_expect,
    on = c("lopnr==lopnr_fall"),
    id_child := lopnr
  ]
  ids_gd[[i]]$father_matched_to_child[!is.na(id_child)]
  ids_gd[[i]]$father_matched_to_child[
    ids_gd[[i]]$master_list_of_what_i_expect,
    on = c("lopnr==lopnr_kontroll"),
    id_child := unique_id_for_control
  ]
  ids_gd[[i]]$father_matched_to_child[is.na(id_child)]
  ids_gd[[i]]$father_matched_to_child[, lopnr := NULL]
  ids_gd[[i]]$father_matched_to_child[id_child==1816850001700]

  ids_gd[[i]]$all <- c(
    ids_gd[[i]]$case,
    ids_gd[[i]]$control_same,
    ids_gd[[i]]$control_opposite,
    ids_gd[[i]]$control_sibling,
    ids_gd[[i]]$mother,
    ids_gd[[i]]$father
  )

  # we have this many controls:
  n1 <- uniqueN(c(ids_gd[[i]]$control_same, ids_gd[[i]]$control_opposite, ids_gd[[i]]$control_sibling))

  # we want this many controls:
  n2 <- nrow(ids_gd[[i]]$master_list_of_what_i_expect)

  if(n2 > n1) print(i)
}

# KRISTEN STOP HERE IF DEBUGGING skeleton1_create

# create the skeleton <- generic
# for(i in seq_along(ids_gd)){
for(i in seq_len(number_of_batches_to_run)){
  cat(as.character(lubridate::now()), " ---- skeleton1 ----- ", i,"/",number_of_batches_to_run, "\n")
  skeleton1_create(file_number = i, ids_batch = ids_gd[[i]], id_master = id_master, large_files = large_files)
}
rm("large_files")

# clean the skeleton (row independent variables) <- generic
for(i in seq_len(number_of_batches_to_run)){
  cat(as.character(lubridate::now()), " ---- skeleton2 ----- ", i,"/",number_of_batches_to_run, "\n")
  skeleton2_clean(file_number = i)
}

