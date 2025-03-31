#' COVID-19 data for total age/sex in Norway (2020 border).
#'
#' @export
add_rx <- function(
    skeleton,
    lmed,
    id_name = "lopnr",
    rxs = list(
      "rx_hormones_pubblock"= c(
        "L02AE",
        "H01CA"
      )
    ),
    source = "atc"
){
  stopifnot(source %in% c("atc", "produkt"))
  if(!"start_date" %in% names(lmed)) lmed[, start_date := edatum]
  if(!"stop_date" %in% names(lmed)) lmed[, stop_date := edatum + round(fddd)]
  if(!"start_isoyearweek" %in% names(lmed)) lmed[, start_isoyearweek := cstime::date_to_isoyearweek_c(start_date)]
  if(!"stop_isoyearweek" %in% names(lmed)) lmed[, stop_isoyearweek :=  cstime::date_to_isoyearweek_c(stop_date)]

  setkey(lmed, start_isoyearweek, stop_isoyearweek, atc)
  setkey(skeleton, id, isoyearweek)
  for(rx in names(rxs)){
    skeleton[,(rx) := FALSE]

    message(Sys.time()," ", rx)
    atcs_for_rx <- rxs[[rx]]
    for(atc_for_rx in atcs_for_rx){
      if(source=="atc"){
        lmed_atc <- lmed[stringr::str_detect(atc, paste0("^",atc_for_rx))]
      } else if(source == "produkt"){
        lmed_atc <- lmed[stringr::str_detect(produkt, paste0("^",atc_for_rx))]
      }
      for(x_isoyearweek in sort(unique(skeleton$isoyearweek))){
        # identify all the women who received A1 in 2021-M01
        women_in_category_and_isoyearweek <- lmed_atc[
          (start_isoyearweek <= x_isoyearweek & x_isoyearweek <= stop_isoyearweek)
        ]

        # assign A1:=TRUE for all the women we found above, in 2021-M01
        if(nrow(women_in_category_and_isoyearweek) > 0){
          women_in_category_and_isoyearweek <- women_in_category_and_isoyearweek[[id_name]] %>% unique()
          skeleton[
            .(women_in_category_and_isoyearweek, x_isoyearweek),
            (rx) := TRUE
          ]
        }
      }
    }
  }
}
