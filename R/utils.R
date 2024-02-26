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
#'     VALUES ?item {
#'       <http://de.dbpedia.org/resource/Bayernpartei>
#'       <http://de.dbpedia.org/resource/Deutsches_Kaiserreich>
#'     }
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
  
  if (nchar(content) == 0L) return(data.frame(c()))
  
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
#' @param highlight_by A `character vector` of the column in which entity names
#'   are annotated. Defaults to NULL.
#' @details If a `character vector` is supplied to `highlight_by`, selected
#'   entity types  (PERSON, LOCATION, ORGANIZATION, MISC) are assigned specific
#'   color codes. Other entities in the column are assigned a single color.
#' @importFrom fs path
#' @export
as_subcorpus <- function(x, highlight_by = NULL){

  if (is.null(highlight_by)) {

    highlights <- rep(x = c(OTHER = "lavender"), times = nrow(x))

  } else {

    if (!highlight_by %in% colnames(x)) {
      stop(format_error(
        c(
          "{.var highlight_by} must exist in {.var x}",
          "x" = "The character vector you supplied was not found in data.table {.var x}"
        )
      ))
    }

    highlights <- sapply(
      x[[highlight_by]],
      switch,
      PERSON = "yellow",
      LOCATION = "lightgreen",
      ORGANIZATION = "lightskyblue",
      MISC = "lightgrey",
      "lavender"
    )
  }

  new(
    "subcorpus",
    template = path(NA_character_),
    cpos = as.matrix(x[, c("cpos_left", "cpos_right")]),
    annotations = list(
      highlight = highlights,
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


#' Map types returned by DBpedia Spotlight to a limited set of classes
#'
#' This function takes the output of `get_dbpedia_uris()` and compares values in
#' the `types` column with a named character vector. The main purpose of this
#' function is to reduce the number of types to a limited set of classes.
#'
#' @param x A `data.table` with DBpedia URIs.
#' @param mapping_vector A `named character vector` with desired class names (as
#'   names) and types from the DBpedia ontology as values. For example:
#'   c("PERSON" = "DBpedia:Person"). Can contain more than one pair of class and
#'   type.
#' @param other a `character vector` with the name of the class of all types not
#'   matched by the `mapping_vector`.
#' @param verbose A `logical` value - whether to display messages.
#' @importFrom data.table is.data.table
#' @importFrom cli format_error cli_alert_info
#' @details If there is more than one match between the retrieved types and the
#'   `mapping vector`, unique classes are sorted alphabetically and collapsed.
#' @return Function adds classes to input data.table by reference.
#' @export
map_types_to_class <- function(x, mapping_vector, other = "MISC", verbose = TRUE) {

  if (!is.data.table(x)) 
    stop(format_error("input {.var x} is no data.table."))

  if (!is.character(mapping_vector)) {
    stop(format_error(c(
        "{.var mapping_vector} is no character vector.",
        "i" = "The {.var mapping_vector} must be a named character vector."
      )))
  }

  if (!is.character(other) | length(other) > 1) {

    stop(format_error("{.var other} not character vector of length {.val 1}."))
  }

  if (!"types" %in% colnames(x)) {

    stop(format_error(c(
        "There is no {.var types} column in the input data.table.",
        "i" = "Types are returned by {.fn get_dbpedia_uri} only if the argument `types` is set to TRUE."
      )))
  }

  types_to_class_fun <- function(types) {

    # types is a list of lists. Transform to single character vector.
    type_list <- unlist(types, recursive = FALSE)

    types_with_class_raw <- lapply(seq_along(type_list), function(i) {
      list_name <- names(type_list)[[i]]
      list_elements <- type_list[[i]]
      paste0(list_name, ":", list_elements)
    })
    types_with_class <- intersect(unlist(types_with_class_raw), mapping_vector)

    if (length(types_with_class) > 0L) {
      match_idx <- which(mapping_vector %in% types_with_class)

      class_name <- paste(
        sort(unique(names(mapping_vector)[match_idx])),
        collapse = "|"
      )

    } else {
      class_name <- other
    }

    return(class_name)
  }

  if (verbose)
    cli_alert_info(
      "mapping values in column {.var types} to new column {.var class}"
    )

  x[, class := types_to_class_fun(types = x[["types"]]), by = 1:nrow(x)]

}
