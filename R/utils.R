pkg_resource <- function(...) {
  system.file("resources", ..., package = "rmsdown", mustWork = TRUE)
}
