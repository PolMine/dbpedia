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
#' @export
#' @examples
#' \donttest{
#' dbpedia_uris <- c(
#'   "http://de.dbpedia.org/resource/Killesberg",
#'   "http://de.dbpedia.org/resource/Ljubljana",
#'   "http://de.dbpedia.org/resource/Velbert"
#' )
#' dbpedia_get_wikidata_uris(
#'   dbpedia_uris,
#'   optional = "municipalityCode",
#'   endpoint = "http://de.dbpedia.org/sparql",
#'   wait = 0,
#'   limit = 2,
#'   progress = TRUE
#' )
#' }
#' @importFrom cli cli_progress_bar cli_progress_done cli_progress_update
dbpedia_get_wikidata_uris <- function(x, optional, endpoint, limit = 100, wait = 1, verbose = TRUE, progress = FALSE){
  
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
  
  x <- na_drop(x, verbose = verbose)
  
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
  }
  
  if (progress) cli_progress_done()

  y <- as_tibble(do.call(rbind, retval_li))
  colnames(y)[1] <- "dbpedia_uri"
  y[["dbpedia_uri"]] <- gsub("^<(.*?)>$", "\\1", y[["dbpedia_uri"]])
  y[["wikidata_uri"]] <- gsub("^<(.*?)>$", "\\1", y[["wikidata_uri"]])
  y[["wikidata_id"]] <- gsub("^.*/(Q\\d+)$", "\\1", y[["wikidata_uri"]])
  y[["key"]] <- NULL
  y
}


#' Query Wikidata endpoint for additional information.
#' 
#' This is a wrapper for `WikidataQueryServiceR::query_wikidata()` to get
#' additional information for known wikidata IDs.
#' 
#' @return A `tibble`.
#' @param x A vector of wikidata ids.
#' @param id Wikidata ID for information to retrieve (`character` vector).
#' @param limit Maximum number of wikidata IDs to be sent to endpoint at a time.
#' @param progress Whether to show progress information (`logical` value).
#' @param wait A numeric value - slow down requests to avoid denial of service.
#' @export
#' @examples
#' \donttest{
#' wikidata_ids <- c("Q1741365", "Q3840", "Q437")
#' wikidata_resolve_dbpedia_uri(
#'   wikidata_ids,
#'   id = "P439", # German municipality key
#'   wait = 0,
#'   limit = 2,
#'   progress = TRUE
#' )
#' }
wikidata_query <- function(x, id, limit = 100L, wait = 1, verbose = TRUE, progress = FALSE){
  
  if (!requireNamespace("WikidataQueryServiceR", quietly = TRUE)){
    stop("R package WikidataQueryServiceR required but not available. ")
  }
  
  stopifnot(
    is.vector(x), is.character(x),
    is.character(id), length(id) == 1L,
    is.numeric(limit), limit > 0,
    is.numeric(wait), wait > 0, length(wait) == 1L,
    is.logical(progress), length(progress) == 1L
  )
  
  x <- na_drop(x, verbose = verbose)
  
  template <- 'SELECT ?item ?label ?key ?keyLabel
        WHERE {
        VALUES ?item { %s }
        OPTIONAL { ?item wdt:%s ?key . }
        ?item rdfs:label ?label
          filter(lang(?label) = "de")
        SERVICE wikibase:label { bd:serviceParam wikibase:language "de". }
      }'
  
  chunks <- as_chunks(x = x, size = limit)
  retval_li <- list()
  
  if (progress) cli_progress_bar("Tasks", total = length(chunks), type = "tasks")
  for (i in 1L:length(chunks)){
    if (progress) cli_progress_update()
    query <- sprintf(
      template,
      paste0("wd:", chunks[[i]], collapse = " "),
      id
    )
    
    Sys.sleep(wait)
    
    suppressWarnings({
      retval_li[[i]] <- WikidataQueryServiceR::query_wikidata(
        sparql_query = query,
        format = "simple"
      )
    })
    
    colnames(retval_li[[i]])[1] <- "wikidata_uri"
  }
  
  if (progress) cli_progress_done()
  
  as_tibble(do.call(rbind, retval_li))
}