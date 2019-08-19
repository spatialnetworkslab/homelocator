.onAttach <- function(libname, pkgname){
  packageStartupMessage("Welcome to homelocator package!")
}

.onLoad <- function(libname, pkgname){
  op <- options()
  op.devtools <- list(
    devtools.path = "~/R-dev",
    devtools.install.args = "",
    devtools.name = "Chen Qingqing",
    devtools.desc.author = 'person("Chen","Qingqing", "qingqing_chen@sutd.edu.sg", role = c("aut","cre"))',
    devtools.desc.license = "MIT + file LICENSE",
    devtools.desc.suggests = NULL,
    devtools.desc = list()
  )
  toset <- !(names(op.devtools) %in% names(op))
  if(any(toset)) options(op.devtools[toset])
  invisible()
}














