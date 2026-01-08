#' Asian Lung Cancer Absolute Risk Models (ALARM)
#'
#' @description
#' These functions return ALARM model objects that can be used to make
#'   predictions of the *t*-year absolute risk of lung cancer mortality. These
#'   functions are memoised when the package is loaded and they are called
#'   internally by [`predictALARM()`], if/when necessary.
#'
#' @return ALARM model objects (i.e., a `list` with the class `fmsm`).
#'
#' @md
#'
#' @export
ALARM_NS <- function() {
  readRDS(system.file('models/ALARM_NS.rds', package = 'ALARM'))
}

#' @rdname ALARM_NS
#' @export
ALARM_ES <- function() {
  readRDS(system.file('models/ALARM_ES.rds', package = 'ALARM'))
}
