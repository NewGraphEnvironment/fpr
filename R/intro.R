.onAttach <- function(libname, pkgname) {
  message <- c("\n Machin3 readable bubs.",
               "\n Happy coding!")
  packageStartupMessage(message)
}
