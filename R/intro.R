.onAttach <- function(libname, pkgname) {
  message <- c("\n Machin3a RReadable.",
               "\n Happy coding!")
  packageStartupMessage(message)
}
