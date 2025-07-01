library(data.table)
library(magrittr)

# mht
folder <- "/data/argos/Bronze/Embla_data/_MHT/mht_raw/scb/"
idx <- read.csv(
  fs::path(folder, "Individ_2007.csv"),
  fileEncoding = "UTF-16"
) %>%
  setDT()
idx

# gd
folder <- "/data/argos/Bronze/Postdoc_Kristen/2023-gd-register-clark-gat-descriptives/data_raw/"
id <- haven::read_sas(
  fs::path(folder, "SCB/demografi.sas7bdat")
) %>%
  setDT()
id

ids <- id$lopnr[1:2000]
date_min <- as.Date("2001-01-01")
date_max <- as.Date("2015-12-31")

isoyearweeks <- seq.Date(
  date_min,
  date_max,
  1
) %>%
  cstime::date_to_isoyearweek_c() %>%
  unique()

skeleton <- expand.grid(
  id = ids,
  isoyearweek = isoyearweeks,
  stringsAsFactors = FALSE
) %>% setDT()

skeleton[, isoyear := cstime::isoyearweek_to_isoyear_n(skeleton$isoyearweek)]
setcolorder(skeleton, c("id", "isoyear", "isoyearweek"))
setorder(skeleton, id, isoyearweek)

skeleton

# add one-time information
id_name <- "lopnr"

d <- haven::read_sas(
  fs::path(folder, "SCB/demografi.sas7bdat")
) %>%
  setDT()
nam_left <- names(d)[!names(d) %in% c(id_name)]
nam_right <- nam_left

for(i in seq_along(nam_left)){
  if(nam_left[i] %in% names(skeleton)) nam_right[i] <- paste0("i.",nam_left[i])
}

nam_left <- paste0(nam_left,collapse='","')
nam_left <- paste0('"',nam_left, '"')
nam_right <- paste0(nam_right,collapse=',')
txt <- paste0('skeleton[d,on = c("id==',id_name,'"),c(',nam_left,'):=.(',nam_right,')]')
eval(parse(text = txt))

# add annual information
isoyear <- 2001
id_name <- "LopNr"

d <- haven::read_sas(
  fs::path(folder, "SCB/fp_lev_famtyp2001.sas7bdat")
) %>%
  setDT()
d[, isoyear := isoyear]
nam_left <- names(d)[!names(d) %in% c(id_name, "isoyear")]
nam_right <- nam_left

for(i in seq_along(nam_left)){
  if(nam_left[i] %in% names(skeleton)) nam_right[i] <- paste0("i.",nam_left[i])
}

nam_left <- paste0(nam_left,collapse='","')
nam_left <- paste0('"',nam_left, '"')
nam_right <- paste0(nam_right,collapse=',')
txt <- paste0('skeleton[d,on = c("id==',id_name,'","isoyear"),c(',nam_left,'):=.(',nam_right,')]')
eval(parse(text = txt))

# add ICD-10 information
# diagnoses and surgeries
# outpatient
ov <- haven::read_sas(file.path(folder, "Sos", "ov.sas7bdat"))
setDT(ov) # convert to data.table
# inpatient
sv <- haven::read_sas(file.path(folder, "Sos", "sv.sas7bdat"))
setDT(sv)
#use.names means matching column name even if in different order, fill means that if a column is
#exists in one and not another it will set the non existing to missing
diag_and_surgeries = rbind(ov, sv, use.names=T, fill=T) # put ov and sv on top of each other
rm("ov", "sv") # remove ov and sv from the RAM of the computer

diag_and_surgeries <- diag_and_surgeries[LopNr %in% skeleton$id]

id_name <- "LopNr"

diags <- list(
  "icd10_F64_0" = c("^F640"),
  "icd10_F64_89" = c("^F6489"),
  "icd10_F64_089" = c("^F640", "^F648", "^F649")
)

variables_containing_icd_codes <- c(
  "HDIA",
  stringr::str_subset(names(diag_and_surgeries), "^DIA"),
  stringr::str_subset(names(diag_and_surgeries), "^EKOD")
)

for(i in seq_along(diags)){
  nam <- names(diags)[i]
  diag_and_surgeries[, (nam) := FALSE]

  for(ii in variables_containing_icd_codes) for(iii in diags[[i]]){
    diag_and_surgeries[stringr::str_detect(get(ii), iii), (nam):=TRUE]
  }
}

diag_and_surgeries[, isoyearweek := cstime::date_to_isoyearweek_c(INDATUM)]

nam <- names(diags)
txt <- paste0("reduced <- diag_and_surgeries[, .(", paste0(names(diags),"=as.logical(max(",names(diags),"))", collapse=", "),"), keyby=.(",id_name,", isoyearweek)]")
eval(parse(text = txt))

nam_left <- paste0(nam,collapse='","')
nam_left <- paste0('"',nam_left, '"')
nam_right <- paste0(nam,collapse=',')
txt <- paste0('skeleton[reduced,on = c("id==',id_name,'","isoyearweek"),c(',nam_left,'):=.(',nam_right,')]')
eval(parse(text = txt))

for(i in nam){
  skeleton[is.na(get(i)), (i) := FALSE]
}
xtabs(~skeleton$icd10_F64_0)




