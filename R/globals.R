# Declare global variables used by data.table to avoid R CMD check warnings

utils::globalVariables(c(
  # Core skeleton variables
  "id", "isoyear", "isoyearweek", "is_isoyear",
  
  # Date and time variables  
  "indatum", "dodsdat", "edatum", "start_date", "stop_date",
  "start_isoyearweek", "stop_isoyearweek",
  
  # Medical coding variables
  "atc", "produkt", "produkt_clean", "fddd",
  
  # Data processing variables
  "d", "XXX_EXCLUDE",
  
  # MHT study specific variables
  "p1163_lopnr_personnr", "product_category", "approach", 
  "row_min", "num_of_approaches_at_row_min",
  
  # data.table's .() function
  "."
))