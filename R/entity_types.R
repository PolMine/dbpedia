#' Map types returned by DBpedia Spotlight to a limited set of categories
#'
#' This function takes the output of `get_dbpedia_uris()` and compares values in
#' the `types` column with a named character vector. The main purpose of this
#' function is to reduce the number of types to a limited set of categories.
#'
#' @param x A `data.table` with DBpedia URIs.
#' @param mapping_vector A `named character vector` with desired category names
#'   (as names) and types from the DBpedia ontology as values. For example:
#'   c("PERSON" = "DBpedia:Person"). Can contain more than one pair of class and
#'   type.
#' @param other A `character vector` with the name of the class of all types not
#'   matched by the `mapping_vector`.
#' @param verbose A `logical` value - whether to display messages.
#' @param ... Further arguments.
#' @importFrom data.table is.data.table
#' @importFrom cli format_error cli_alert_info
#' @details If there is more than one match between the retrieved types and the
#'   `mapping vector`, unique classes are sorted alphabetically and collapsed.
#' @return Function adds classes to input data.table by reference.
#' @exportMethod entity_types_map
#' @rdname entity_types_map
setGeneric(
  "entity_types_map",
  function(x, ...) standardGeneric("entity_types_map")
)


#' @rdname entity_types_map
#' @examples
#' library(quanteda)
#' 
#' inaugural_paragraphs <- data_corpus_inaugural %>%
#'   corpus_subset(Year == 2009) %>% # limit to Barack Obama 2009
#'   corpus_reshape(to = "paragraphs")
#'   
#' uritab_paragraphs <- get_dbpedia_uris(
#'   x = inaugural_paragraphs,
#'   language = "en",
#'   max_len = 5600L,
#'   confidence = 0.5,
#'   api = "http://api.dbpedia-spotlight.org/en/annotate",
#'   verbose = FALSE,
#'   progress = TRUE
#' )
#' 
#' mapping_vector = c(
#'   "PERSON" = "DBpedia:Person",
#'   "ORGANIZATION" = "DBpedia:Organisation",
#'   "LOCATION" = "DBpedia:Place"
#' )
#' 
#' entity_types_map(
#'   uritab_paragraphs[["types"]],
#'   mapping_vector = mapping_vector
#' )
setMethod(
  "entity_types_map", "list",
  function(x, mapping_vector, other = "MISC", verbose = TRUE
  ) {
    
    # there is not a check yet whether the vector is named
    if (!is.character(mapping_vector)) {
      stop(format_error(c(
        "{.var mapping_vector} is no character vector.",
        "i" = "The {.var mapping_vector} must be a named character vector."
      )))
    }
    
    if (!is.character(other) | length(other) > 1)
      stop(format_error("{.var other} not character vector of length {.val 1}."))
    
    sapply(
      x,
      function(el) {
        types_with_category_raw <- lapply(
          seq_along(el),
          function(i) {
            list_name <- names(el)[[i]]
            list_elements <- el[[i]]
            paste0(list_name, ":", list_elements)
          })

        types_with_category <- intersect(unlist(types_with_category_raw), mapping_vector)

        if (length(types_with_category) > 0L) {
          match_idx <- which(mapping_vector %in% types_with_category)

          category <- paste(
            sort(unique(names(mapping_vector)[match_idx])),
            collapse = "|"
          )
        } else {
          category <- other
        }
      }
    )
  }
)
  
#' @rdname entity_types_map
setMethod(
  "entity_types_map", "data.table",
  function(x, mapping_vector, other = "MISC", verbose = TRUE) {
  
  if (!"types" %in% colnames(x)) {
    stop(format_error(c(
      "There is no {.var types} column in the input data.table.",
      "i" = "Types are returned by {.fn get_dbpedia_uri} only if the argument `types` is set to TRUE."
    )))
  }

  if (verbose)
    cli_alert_info(
      "mapping values in column {.var types} to new column {.var category}"
    )
  
  x[, "category" := entity_types_map(x = x[["types"]], mapping_vector = mapping_vector, other = other, verbose = verbose)]
  x
})
