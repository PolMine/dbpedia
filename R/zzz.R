.onAttach <- function (libname, pkgname) {
  stdout <- system2(command = "docker", args = c("container", "ls"), stdout = TRUE)
  if (grepl("dbpedia/dbpedia-spotlight", stdout[2])){
    packageStartupMessage("* Docker container 'dbpedia/dbpedia-spotlight' running")
    lang <- gsub('^.*"spotlight.sh\\s+(\\w{2})".*$', "\\1", stdout[2])
    packageStartupMessage(sprintf("* language: %s", lang))
    options("dbpedia.lang" = lang)
  } else {
    packageStartupMessage("* Docker container 'dbpedia/dbpedia-spotlight' not found")
  }
}

