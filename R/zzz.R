.onAttach <- function (libname, pkgname) {
  
  status <- dbpedia_spotlight_status()
  
  if (status[["docker"]]){
    packageStartupMessage("* DBpedia Spotlight Docker container running")
  } else {
    packageStartupMessage(
      "* Using DBpedia Spotlight online API (no local Docker container running)"
    )
  }
  packageStartupMessage(sprintf("* endpoint/api: %s", status[["endpoint"]]))
  packageStartupMessage(sprintf("* language: %s", status[["lang"]]))
}

