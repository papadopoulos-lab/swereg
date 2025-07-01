identify_fourth_internal <- function(x){
  retval <- which(x)[4]
  rep(retval, length(x))
}

identify_fourth <- Vectorize(identify_fourth_internal)
