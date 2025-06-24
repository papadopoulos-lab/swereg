org::initialize_project(
  env = .GlobalEnv,
  home = c(
    "~/GitHub/2022-mht/2023-bai-paper-1",
    "~/epi-ai/2022-mht/2023-bai-paper-1"
  ),
  # results = c(
  #   "C:/Users/kricl384/Box/2023-gd-register-clark-gat-descriptives/results/",
  #   "~/epi-ai/2023-gd-register-clark-gat-descriptives/results"
  # ),
  data_raw = c(
    "//argos.rudbeck.uu.se/MyGroups$/Bronze/Embla_data/_MHT/mht_raw/",
    "/data/argos/Bronze/Embla_data/_MHT/mht_raw/"
  ),
  data_processed = c(
    "//argos.rudbeck.uu.se/MyGroups$/Bronze/Embla_data/_MHT/mht_processed",
    "/data/argos/Bronze/Embla_data/_MHT/mht_processed/"
  )
)


# load every single function (commands) into what is currently available in these named libraries
library(data.table)
library(ggplot2)
library(magrittr)

ids_restricted = 1:1000

# get the correct ids/lopnr
print("Get the correct IDS/lopnr")
id <- read.csv(
  fs::path(org::project$data_raw, "scb/Individ_2007.csv"),
  fileEncoding = "UTF-16"
) %>%
  setDT()
ids <- unique(id$P1163_LopNr_PersonNr)
# taking the first X id numbers for women
if(!is.null(ids_restricted)) ids <- ids[ids_restricted]
rm("id")

skeleton <- swereg::create_skeleton(
  ids = ids,
  date_min = "2005-05-01",
  date_max = "2021-12-31"
)

folder <- org::project$data_raw
