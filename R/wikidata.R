#' Get Wikipedia IDs for DBpedia IDS.
#' 
#' @param x A character vector with DBpedia URIs.
#' @param optional Optional information to retrieve (passed as length-one
#'   character vector, e.g. 'municipalityCode').
#' @param endpoint Endpoint to query (a `character` vector).
#' @param limit Single numeric value with maximum size of chunks to process at
#'   a time.
#' @param wait A numeric value passed into `Sys.sleep()` to slow down sequence
#'   of requests (and avoid denial of service). Defaults to 100.
#' @param progress Whether to show progress bar (`logical` value).
#' @examples
#' \donttest{
#' dbpedia_ids <- c(
#'   "http://de.dbpedia.org/resource/Killesberg",
#'   "http://de.dbpedia.org/resource/Ljubljana",
#'   "http://de.dbpedia.org/resource/Velbert"
#' )
#' dbpedia_get_wikidata_uris(
#'   dbpedia_ids,
#'   optional = "municipalityCode",
#'   endpoint = "http://de.dbpedia.org/sparql",
#'   wait = 0,
#'   limit = 2,
#'   progress = TRUE
#' )
#' }
#' @importFrom cli cli_progress_bar cli_progress_done cli_progress_update
dbpedia_get_wikidata_uris <- function(x, optional, endpoint, limit = 100, wait = 1, progress = FALSE){
  
  if (!requireNamespace("SPARQL", quietly = TRUE)){
    stop(
      "R package SPARQL required but not available. ",
      "SPARQL is currently not at CRAN, but can be installed from the archive"
    )
  }

  stopifnot(
    is.character(x),
    is.numeric(limit), length(limit) == 1L,
    is.logical(progress), length(progress) == 1L,
    is.numeric(wait), length(wait) == 1L, wait >= 0
  )
  
  if (!missing(optional)){
    stopifnot(is.character(optional), length(optional) == 1L)
    optional <- sprintf('OPTIONAL { ?item dbo:%s ?key . }', optional)
  } else {
    optional <- ""
  }
  
  template <- 'SELECT distinct ?item ?wikidata_uri ?key
      WHERE {
      VALUES ?item {%s}
      ?item owl:sameAs ?wikidata_uri
      %s
      FILTER(regex(str(?wikidata_uri), "www.wikidata.org" ) )}
      LIMIT %d'
  

  chunks <- as_chunks(x = x, size = limit)
  retval_li <- list()
  
  if (progress) cli_progress_bar("Tasks", total = length(chunks), type = "tasks")
  for (i in 1L:length(chunks)){
    cli_progress_update()
    query <- sprintf(
      template,
      paste(sprintf("<%s>", chunks[[i]]), collapse = " "),
      optional,
      limit
    )
    
    Sys.sleep(wait)
    
    retval_li[[i]] <- SPARQL::SPARQL(url = endpoint, query = query)[["results"]]
    retval_li[[i]][["wikidata_id"]] <- gsub(
      "^.*\\/(Q\\d+)>$", "\\1",
      retval_li[[i]][["wikidata_uri"]]
    )
    colnames(retval_li[[i]])[1] <- "dbpedia_uri"
  }
  
  if (progress) cli_progress_done()
  
  do.call(rbind, retval_li)
}