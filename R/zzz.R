.onLoad <- function(libname, pkgname) {
  ALARM_NS <<- memoise::memoise(ALARM_NS)
  ALARM_ES <<- memoise::memoise(ALARM_ES)
}
