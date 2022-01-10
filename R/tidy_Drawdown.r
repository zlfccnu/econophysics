#' the drawdown of a vector accompany to tidyverse
#' @param Ra the time series vector
#' @param geometric whether to use geometric or not
#' @return 
#' @export
tidy_draw_down <- function(Ra, geometric) {
  if (geometric) 
    Return.cumulative = cumprod(1 + {{Ra}})
  else Return.cumulative = 1 + cumsum({{Ra}})
  maxCumulativeReturn = cummax(c(1, Return.cumulative))[-1]
  column.drawdown = Return.cumulative/maxCumulativeReturn - 1
}