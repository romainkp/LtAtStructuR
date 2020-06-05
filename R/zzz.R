.onAttach <- function(...) {
  pkgName <- "LtAtStructuR"
  packageStartupMessage(paste0(
    pkgName,
    " v",
    utils::packageDescription(pkgName)$Version,
    ": ",
    utils::packageDescription(pkgName)$Title
  ))
}
