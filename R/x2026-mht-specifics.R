x2026_mht_lmed_categorize_product_names <- function(x) {
  # Declare variables for data.table non-standard evaluation
  produkt_clean <- product_category <- produkt <- NULL

  x[, produkt_clean := stringr::str_remove_all(produkt, "-")]
  x[, produkt_clean := stringr::str_remove_all(produkt, " ")]
  x[, product_category := fcase(
    stringr::str_detect(produkt_clean, "Oestring"), "A3",
    stringr::str_detect(produkt_clean, "Vagidonna"), "A3",
    stringr::str_detect(produkt_clean, "Vagifem"), "A3",
    stringr::str_detect(produkt_clean, "Vagirux"), "A3",
    stringr::str_detect(produkt_clean, "EstradiolSUN"), "A3",
    stringr::str_detect(produkt_clean, "Menovag"), "A3",



    stringr::str_detect(produkt_clean, "Blissel"), "A4",
    stringr::str_detect(produkt_clean, "Estrokad"), "A4",
    stringr::str_detect(produkt_clean, "Ovesterin"), "A4",
    stringr::str_detect(produkt_clean, "Gelistrol"), "A4",

    stringr::str_detect(produkt_clean, "Divigel"), "A1",
    stringr::str_detect(produkt_clean, "Estradot"), "A1",
    stringr::str_detect(produkt_clean, "Estrogel"), "A1",
    stringr::str_detect(produkt_clean, "Lenzetto"), "A1",
    stringr::str_detect(produkt_clean, "Dermestril"), "A1",

    stringr::str_detect(produkt_clean, "Evorel"), "A1",
    stringr::str_detect(produkt_clean, "Oesclim"), "A1",
    stringr::str_detect(produkt_clean, "Climara"), "A1",
    stringr::str_detect(produkt_clean, "Evopad"), "A1",
    stringr::str_detect(produkt_clean, "Femseven"), "A1",

    stringr::str_detect(produkt_clean, "Progynon"), "A2",
    stringr::str_detect(produkt_clean, "Femanest"), "A2",

    stringr::str_detect(produkt_clean, "Oestriolaspen"), "A5",
    stringr::str_detect(produkt_clean, "Premarina"), "A6",
    stringr::str_detect(produkt_clean, "Presomen"), "A6",

    stringr::str_detect(produkt_clean, "Delestrogen"), "A7",
    stringr::str_detect(produkt_clean, "Neofollin"), "A7",

    stringr::str_detect(produkt_clean, "Estalis"), "B1",
    stringr::str_detect(produkt_clean, "EstalisSekvens"), "B1",

    stringr::str_detect(produkt_clean, "Activelle"), "B2",
    stringr::str_detect(produkt_clean, "Cliovelle"), "B2",
    stringr::str_detect(produkt_clean, "Eviana"), "B2",
    stringr::str_detect(produkt_clean, "Femanor"), "B2",
    stringr::str_detect(produkt_clean, "Noresmea"), "B2",
    stringr::str_detect(produkt_clean, "Kliogest"), "B2",



    stringr::str_detect(produkt_clean, "Indivina"), "B3",
    stringr::str_detect(produkt_clean, "Duova"), "B3",
    stringr::str_detect(produkt_clean, "Premelle"), "B3",
    stringr::str_detect(produkt_clean, "Premellesekvens"), "B3",


    stringr::str_detect(produkt_clean, "Femostonconti"), "B4",

    stringr::str_detect(produkt_clean, "Climodien"), "B5",

    stringr::str_detect(produkt_clean, "Angemin"), "B6",

    stringr::str_detect(produkt_clean, "Sequidot"), "B7",
    stringr::str_detect(produkt_clean, "Femasekvens"), "B8",
    stringr::str_detect(produkt_clean, "Trisekvens"), "B8",
    stringr::str_detect(produkt_clean, "Novofem"), "B8",

    stringr::str_detect(produkt_clean, "DivinaPlus"), "B9",
    stringr::str_detect(produkt_clean, "Trivina"), "B9",

    stringr::str_detect(produkt_clean, "Presomen"), "B10",


    stringr::str_detect(produkt_clean, "Femoston"), "B11",


    stringr::str_detect(produkt_clean, "Cyclabil"), "B11",

    stringr::str_detect(produkt_clean, "Crinone"), "C1",
    stringr::str_detect(produkt_clean, "Cyclogest"), "C1",
    stringr::str_detect(produkt_clean, "Lugesteron"), "C1",
    stringr::str_detect(produkt_clean, "Lutinus"), "C1",
    stringr::str_detect(produkt_clean, "Utrogest"), "C1",
    stringr::str_detect(produkt_clean, "Utrogestan"), "C1",
    stringr::str_detect(produkt_clean, "Progesteron"), "C1",
    stringr::str_detect(produkt_clean, "Extemporeprogesteron"), "C1",
    stringr::str_detect(produkt_clean, "ProgesteronMICAPL"), "C1",

    stringr::str_detect(produkt_clean, "Prolutex"), "C1",


    stringr::str_detect(produkt_clean, "Visanne"), "C3",
    stringr::str_detect(produkt_clean, "Desogestrel"), "C3",
    stringr::str_detect(produkt_clean, "Cerazette"), "C3",
    stringr::str_detect(produkt_clean, "Azalia"), "C3",
    stringr::str_detect(produkt_clean, "Gestrina"), "C3",
    stringr::str_detect(produkt_clean, "Velavel"), "C3",
    stringr::str_detect(produkt_clean, "Vinelle"), "C3",
    stringr::str_detect(produkt_clean, "Zarelle"), "C3",
    stringr::str_detect(produkt_clean, "Slinda"), "C3",

    stringr::str_detect(produkt_clean, "PrimolutNor"), "C4",
    stringr::str_detect(produkt_clean, "Provera"), "C4",
    stringr::str_detect(produkt_clean, "Duphaston"), "C4",
    stringr::str_detect(produkt_clean, "Orgametril"), "C4",
    stringr::str_detect(produkt_clean, "Gestapuran"), "C4",
    stringr::str_detect(produkt_clean, "Duphaston"), "C5",

    stringr::str_detect(produkt_clean, "DepoProvera"), "D1",
    stringr::str_detect(produkt_clean, "Nexplanon"), "D2",
    stringr::str_detect(produkt_clean, "Implanon"), "D2",
    stringr::str_detect(produkt_clean, "Folistrel"), "D2",
    stringr::str_detect(produkt_clean, "Jadelle"), "D3",

    stringr::str_detect(produkt_clean, "Jaydess"), "E1",
    stringr::str_detect(produkt_clean, "Kyleena"), "E1",
    stringr::str_detect(produkt_clean, "Levosert"), "E1",
    stringr::str_detect(produkt_clean, "Levosertone"), "E1",
    stringr::str_detect(produkt_clean, "Mirena"), "E1",

    stringr::str_detect(produkt_clean, "Livial"), "F1",
    stringr::str_detect(produkt_clean, "Tibelia"), "F1",
    stringr::str_detect(produkt_clean, "Tibocina"), "F1",
    stringr::str_detect(produkt_clean, "TibolonAristo"), "F1",
    stringr::str_detect(produkt_clean, "TibolonMylan"), "F1",

    stringr::str_detect(produkt_clean, "TibolonOrifarm"), "F1",
    stringr::str_detect(produkt_clean, "Boltin"), "F1",
    stringr::str_detect(produkt_clean, "Duavive"), "G1",

    stringr::str_detect(produkt_clean, "Nebido"), "H1",
    stringr::str_detect(produkt_clean, "Testogel"), "H1",
    stringr::str_detect(produkt_clean, "Undestor"), "H1",
    stringr::str_detect(produkt_clean, "Undestortestocaps"), "H1",
    stringr::str_detect(produkt_clean, "Testovirondepot"), "H1",
    stringr::str_detect(produkt_clean, "Intrinsa"), "H1",
    stringr::str_detect(produkt_clean, "Testavan"), "H1",
    stringr::str_detect(produkt_clean, "Testim"), "H1",
    stringr::str_detect(produkt_clean, "Testovirondepot"), "H1",
    stringr::str_detect(produkt_clean, "Tostran"), "H1",
    stringr::str_detect(produkt_clean, "Tostrex"), "H1",

    stringr::str_detect(produkt_clean, "MiniPe"), "I1",
    stringr::str_detect(produkt_clean, "Exlutena"), "I2"

  )
  ]
}

x2026_mht_apply_lmed_categories_to_skeleton <- function(skeleton, LMED, verbose = TRUE){
  # Declare variables for data.table non-standard evaluation
  . <- NULL
  start_isoyearweek <- stop_isoyearweek <- isoyearweek <- product_category <- id <- NULL

  product_categories <- c(
    "A1", "A2", "A3", "A4", "A5", "A6", "A7",
    "B1", "B2", "B3", "B4", "B5", "B6", "B7", "B8", "B9", "B10", "B11", "B12",
    "C1", "C2", "C3", "C4", "C5",
    "D1", "D2", "D3", "D4",
    "E1",
    "F1",
    "H1",
    "I1", "I2"
  )
  setkey(LMED, start_isoyearweek, stop_isoyearweek, product_category)
  setkey(skeleton, id, isoyearweek)
  for(product in product_categories){
    skeleton[,(product) := FALSE]
  }
  for(product in product_categories){
    if (verbose) message(Sys.time(), " ", product)
    LMED_product <- LMED[product_category == product]
    for (x_isoyearweek in sort(unique(skeleton$isoyearweek))){
      # identify all the women who received A1 in 2021-M01
      women_in_category_and_isoyearweek <- LMED_product[
        (start_isoyearweek <= x_isoyearweek & x_isoyearweek <= stop_isoyearweek)
      ]$lopnr |> unique()

      if(length(women_in_category_and_isoyearweek) == 0) next()
      # assign A1:=TRUE for all the women we found above, in 2021-M01
      skeleton[
        .(women_in_category_and_isoyearweek, x_isoyearweek),
        (product) := TRUE
      ]
    }
  }
  setorder(skeleton, id, isoyearweek)
}

x2026_mht_replace_false_runs <- function(x) {
  runs <- rle(x)
  runs$values[runs$values == FALSE & runs$lengths <= 4] <- TRUE
  inverse.rle(runs)
}

x2026_mht_cumulative_reset <- function(x) {
  grp_id <- rleid(!x)
  cumsum_reset <- ave(x, grp_id, FUN = cumsum)
  return(cumsum_reset)
}


x2026_mht_apply_lmed_approaches_to_skeleton <- function(skeleton){
  # Declare variables for data.table non-standard evaluation
  . <- NULL
  approach <- id <- row_min <- num_of_approaches_at_row_min <- NULL

  # approaches
  data_approach <- readxl::read_excel(
    system.file("2023-mht", "dataDictionary20241105.xlsx", package = "swereg"),
    sheet = "post_grouping"
  )
  setDT(data_approach)
  data_approach <- data_approach[!is.na(approach)]

  for (i in unique(data_approach$approach)){
    app <- data_approach[approach == i]
    for (j in unique(app$variable)) skeleton[, (j) := FALSE]

    for (j in 1:nrow(app)){
      x_approach <- app[j, ]
      formula <- glue::glue("{x_approach$includes1}==T")
      if(!is.na(x_approach$includes2)) formula <- glue::glue("{formula} & {x_approach$includes2}==T")

      for (k in 1:30){
        dontinclude <- paste0("doesnotinclude", k)
        if(!is.na(x_approach[[dontinclude]])) formula <- glue::glue("{formula} & {x_approach[[dontinclude]]}==F")
      }
      formula <- glue::glue(
        'skeleton[{formula}, {x_approach$variable} := TRUE]'
      )
      eval(parse(text = formula))
    }

    # fill in the missing gaps (up to four weeks)
    for (j in unique(app$variable)){
      if (j == "local_or_none_mht") next()
      skeleton[, (j) := x2026_mht_replace_false_runs(get(j)), by = .(id)]
    }

    # how long they've been taking the drug for
    run_vars <- c()
    for (j in unique(app$variable)){
      if (j == "local_or_none_mht") next()
      var <- paste0("run_", j)
      run_vars <- c(run_vars, var)
      skeleton[, (var) := x2026_mht_cumulative_reset(get(j)), by = .(id)]
      skeleton[get(var) == 0, (var) := 999999999]
    }

    skeleton[, row_min := do.call(pmin, c(.SD, na.rm = TRUE)), .SDcols = run_vars]

    # combine them into the 'final' approach conclusion
    approach_name <- paste0("approach", i)
    skeleton[, (approach_name) := "local_or_none_mht"]
    skeleton[, num_of_approaches_at_row_min := 0]
    for (j in unique(app$variable)){
      if (j == "local_or_none_mht") next()
      var <- paste0("run_", j)
      skeleton[get(var) == row_min & row_min != 999999999, (approach_name) := j]
      skeleton[get(var) == row_min & row_min != 999999999, num_of_approaches_at_row_min := num_of_approaches_at_row_min + 1]
    }
    skeleton[num_of_approaches_at_row_min > 1, (approach_name) := "clashingprescriptions"]
    skeleton[, num_of_approaches_at_row_min := NULL]

    # tag all subsequent weeks as clashingprescriptions
    skeleton[
      ,
      (approach_name):= fifelse(
        any(get(approach_name) == "clashingprescriptions") & seq_len(.N) >= which(get(approach_name) == "clashingprescriptions")[1],
        "clashingprescriptions",
        get(approach_name)
      ),
      by = id
    ]

    skeleton[, row_min := NULL]
    for(j in run_vars) skeleton[, (j) := NULL]
    for(j in unique(app$variable)) skeleton[, (j) := NULL]
  }
}

#' Add 2023 MHT-specific prescription data to skeleton
#'
#' Processes Swedish prescription registry data (LMED) specifically for the 2023
#' Menopausal Hormone Therapy (MHT) study. Categorizes medications into predefined
#' groups, applies duration fixes, and creates treatment approach variables.
#'
#' @param skeleton A data.table containing the main skeleton structure
#' @param lmed A data.table containing prescription registry data with columns:
#'   lopnr (ID), produkt (product name), edatum (dispensing date),
#'   fddd (duration in days), atc (ATC code)
#' @return The skeleton data.table is modified by reference with MHT treatment
#'   variables and approach categorizations added. Returns the modified skeleton invisibly.
#' @details
#' This function performs several steps:
#' \itemize{
#'   \item Restricts LMED data to individuals in the skeleton
#'   \item Categorizes products into MHT groups (A1-I2) based on product names
#'   \item Applies duration fixes for specific products (IUDs, minimum doses)
#'   \item Creates treatment approach variables based on predefined logic
#'   \item Handles treatment gaps and overlapping prescriptions
#' }
#' @note This function is specific to the 2023 MHT study and uses study-specific
#'   categorizations and approaches defined in the package data dictionary.
#' @export
x2026_mht_add_lmed <- function(skeleton, lmed, verbose = TRUE) {
  # Declare variables for data.table non-standard evaluation
  lopnr <- start_isoyearweek <- stop_isoyearweek <- start_date <- stop_date <- NULL
  product_category <- fddd <- produkt <- edatum <- NULL

  if (verbose) message(Sys.time(), " LMED loading")
  if (verbose) message(Sys.time(), " LMED restricting")
  lmed <- lmed[lopnr %in% unique(skeleton$id)]
  if (verbose) message(Sys.time(), " LMED categorizing product names ")
  x2026_mht_lmed_categorize_product_names(lmed)

  # fixing IUDS
  lmed[product_category == "D3", fddd := 1680] # IUDs
  lmed[product_category == "E1", fddd := 1680] # IUDs
  lmed[
    stringr::str_detect(produkt, "Jaydess"),
    fddd := 1008
  ]

  # fixing FDDDs
  fixes <- if (verbose) {
    readxl::read_excel(
      system.file("2023-mht", "dataDictionary20241105.xlsx", package = "swereg"),
      sheet = "MHT_groups"
    )
  } else {
    suppressWarnings(readxl::read_excel(
      system.file("2023-mht", "dataDictionary20241105.xlsx", package = "swereg"),
      sheet = "MHT_groups"
    ))
  }
  setDT(fixes)
  fixes <- fixes[!is.na(minimum_monthly_dose)]
  for (i in 1:nrow(fixes)){
    x_produkt <- fixes$Preparatnamn[i]
    minimum_monthly_dose <- fixes$minimum_monthly_dose[i]
    minimum_months <- fixes$minimum_months[i]

    lmed[
      stringr::str_detect(produkt, x_produkt),
      fddd := fifelse(
        floor(fddd/minimum_monthly_dose) < minimum_months, 0, floor(fddd/minimum_monthly_dose)*28
      )
    ]
  }

  if (verbose) message(Sys.time(), " LMED reducing size ")
  lmed <- lmed[!is.na(product_category)]
  lmed[, start_date := edatum]
  lmed[, stop_date := edatum + round(fddd)]
  if (verbose) message(Sys.time(), " LMED start/stop ")
  lmed[, start_isoyearweek := cstime::date_to_isoyearweek_c(start_date)]
  lmed[, stop_isoyearweek :=  cstime::date_to_isoyearweek_c(stop_date)]

  if (verbose) message(Sys.time(), " LMED apply categories to skeleton ")
  x2026_mht_apply_lmed_categories_to_skeleton(skeleton, lmed, verbose = verbose)
  if (verbose) message(Sys.time(), " LMED apply approaches ")
  x2026_mht_apply_lmed_approaches_to_skeleton(skeleton)
  if (verbose) message(Sys.time(), " LMED finished ")
  data.table::shouldPrint(skeleton)
}