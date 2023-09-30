
# dataframe-to-dataframe merge function like Stata
stata.merge <- function(x, y, by) {
  x$new1 <- 1L
  y$new2 <- 2L
  df <- full_join(x, y, by)
  df$merge <- as.integer(rowSums(df[, c("new1", "new2")], na.rm = TRUE))
  df$new1 <- NULL
  df$new2 <- NULL
  return(df)
}

check_df_unique_by <- function(df, ..., stop = T) {
  # inputs:
  # df: a dataframe
  #       identify the dataframe. Set to False if just want to print, not stop
  # ...: sequence of variables in the dataframe (list the vars without quotes separated by commas)
  # stop: True (default) if want to execute stop function when the variables do not uniquely
  #
  vars <- dplyr::quos(...)
  n <- df %>%
    dplyr::distinct(!!!vars) %>%
    nrow()
  
  varnames <- rlang::quo_text(vars)
  varnames <- gsub("~", "", regmatches(varnames, gregexpr("~\\w+", varnames))[[1]])
  varnames <- paste(varnames, collapse = ", ")
  if (n != nrow(df)) {
    message <- paste0("The data are NOT uniquely identified by: ", varnames)
    if (stop == T) {
      stop(message)
    } else {
      print(message)
    }
  } else {
    message <- paste0("The data are uniquely identified by: ", varnames)
    print(message)
  }
}