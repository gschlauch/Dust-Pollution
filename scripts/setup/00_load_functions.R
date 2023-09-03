# R functions
library(dplyr)
library(sf)
library(ggplot2)
library(stringr)
library(sp)
library(raster)
library(ncdf4)
library(data.table)
library(readr)
library(tidyr)
library(haven)
library(exactextractr)

# User written functions
stata.merge <- function(x, y, by) {
  x$new1 <- 1L
  y$new2 <- 2L
  df <- full_join(x, y, by)
  df$merge <- as.integer(rowSums(df[, c("new1", "new2")], na.rm = TRUE))
  df$new1 <- NULL
  df$new2 <- NULL
  return(df)
}

stata.merge.print <- function(df) {
  counts_df <- count(df, merge, name = "count")
  
  all_counts_df <- data.frame(
    merge = 1L:3L,
    merge_status = c("master only", "using only", "matched")
  )
  merge(all_counts_df, counts_df, by = "merge", all = TRUE) %>%
    mutate(count = ifelse(is.na(count), 0, count))
  
}
