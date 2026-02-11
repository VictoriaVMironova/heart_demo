#compute mortality rate
compute_mortality <- function(data) {
  if (nrow(data) == 0) return("N/A")
  paste0(round(100 * sum(data$DIED == "Died") / nrow(data), 1), "%")
}
