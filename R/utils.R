#' Query SPARQL endpoint
#' 
#' @param endpoint URL of a SPARQL endpoint.
#' @param query A (single) SPARL query. 
#' @importFrom httr GET add_headers content
#' @importFrom xml2 read_xml xml_find_all xml_attr xml_text
#' @importFrom utils URLencode
#' @export
#' @examples
#' sparql_endpoint <- "http://de.dbpedia.org/sparql"
#' query <- 'SELECT distinct ?item ?wikidata_uri ?key
#'   WHERE {
#'     VALUES ?item {<http://de.dbpedia.org/resource/Bayernpartei> <http://de.dbpedia.org/resource/Deutsches_Kaiserreich>}
#'     ?item owl:sameAs ?wikidata_uri
#'     FILTER(regex(str(?wikidata_uri), "www.wikidata.org" ) )
#'   }
#'   LIMIT 100
#' '
#' sparql_query(endpoint = sparql_endpoint, query = query)
sparql_query <- function(endpoint, query){
  
  stopifnot(
    is.character(endpoint), length(endpoint) == 1L,
    is.character(query), length(query) == 1L
  )
  
  url <- paste(
    endpoint,
    "?query=",
    gsub("\\+", "%2B", URLencode(query, reserved = TRUE)),
    sep = ""
  )
  
  results <- GET(
    url = url,
    add_headers(Accept = "application/sparql-results+xml")
  )
  
  content <- content(results, as = "text")
  
  dom <- read_xml(x = content)
  results <- xml_find_all(x = dom, xpath = "//d1:result")
  
  if (length(results) == 0L) return(data.frame(c()))
  
  vars <- xml_find_all(dom, xpath = "//d1:head/d1:variable")
  attrs <- xml_attr(vars, attr = "name")
  
  data <- lapply(
    attrs,
    function(attr){
      nodes <- xml_find_all(
        results,
        xpath = sprintf("//d1:binding[@name='%s']", attr)
      )
      if (length(nodes) == 0L){
        rep(NA, times = length(results))
      } else {
        xml_text(nodes)
      }
    }
  )
  names(data) <- attrs
  data.frame(data)
}

#' Split vector into equally sized chunks
#' 
#' Split input vector `x` into equally sized chunks. The maximum size is taken
#' from argument `size`.
#' 
#' @return A list of vectors.
#' @param x A vector to be split into equally sized chunks.
#' @param size Numeric value, maximum size of chunks.
as_chunks <- function(x, size){
  if (isFALSE(is.vector(x))) stop("as_chunks() requires x to be vector")
  if (isFALSE(is.numeric(size))) stop("as_chunks() requires size to be numeric")
  
  if (length(x) <= size){
    li <- list(x)
  } else {
    n_chunks <- length(x) / size
    is_divisable <-  if ((n_chunks - floor(n_chunks)) == 0) TRUE else FALSE
    if (is_divisable){
      b <- unique(c(1L, 1L:floor(n_chunks) * size, length(x) - 1L, length(x)))
    } else {
      b <- unique(c(1L, 1L:floor(n_chunks) * size, length(x)))
    }
    
    f <- cut(x = seq_along(x), breaks = b, include.lowest = TRUE, right = FALSE)
    li <- split(x = x, f = f)
  }
  
  if (any(sapply(li, length) > size))
    warning("as_chunks() yields at least chunk exceeding size")
  
  li
}

#' Transform table with DBpedia URIs to subcorpus.
#' 
#' @param x A `data.table` with DBpedia URIs.
#' @importFrom fs path
#' @export
as_subcorpus <- function(x){
  new(
    "subcorpus",
    template = path(NA_character_),
    cpos = as.matrix(x[, c("cpos_left", "cpos_right")]),
    annotations = list(
      highlight = sapply(
        x[["ne_type"]],
        switch,
        PERSON = "yellow",
        LOCATION = "lightgreen",
        ORGANIZATION = "lightskyblue",
        MISC = "lightgrey"
      ),
      href = x[["dbpedia_uri"]],
      tooltips = ifelse(is.na(x[["dbpedia_uri"]]), "[no uri]", x[["dbpedia_uri"]])
    )
  )
}

na_drop <- function(x, verbose = TRUE){
  if (any(is.na(x))){
    nas <- is.na(x)
    if (verbose)
      cli_alert_info(
        "drop {.val {length(which(nas))}} NA values (old length {.val {length(x)}})"
      )
    x <- x[-which(nas)]
  }
  x
}

unique_msg <- function(x, verbose = TRUE){
  y <- unique(x)
  if (length(y) < length(x) && verbose)
    cli_alert_info("{.val {length(y)}} unique values to process")
  y
}

