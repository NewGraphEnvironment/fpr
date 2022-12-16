.onAttach <- function(libname, pkgname) {
  message <- c("\n Machin3 readable Winters.",
               "\n Happy coding bud!")
  packageStartupMessage(message)
}
