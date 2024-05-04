# Auxillary function to make c("PERSON" = "DBpedia:Person") to a list
# representation like list("DBpedia" = c("PERSON" = "Person")).

entity_types_categorize <- function(x, mapping_vector) {
  
  raw_type_src <- unname(gsub("(.*?)\\:.*$", "\\1", mapping_vector))
  raw_type <- unname(gsub(".*?\\:(.*)$", "\\1", mapping_vector))
  category <- names(mapping_vector)
  
  if (!all(grepl("\\:", mapping_vector)) | !all(paste0(unique(raw_type_src), "_type") %in% colnames(x))) {
    cli_abort(c(
      "When processing `data.table` objects, the `mapping_vector` must include the name of the type source.",
      "x" = "Either no type source was provided or the provided type source does not correspond to the columns in the data.table."
    ))
  }
  
  raw_type_src_unique <- unique(raw_type_src)
  
  mapping_per_source <- lapply(raw_type_src_unique, function(el) {
    el_type_idx <- which(el == raw_type_src)
    raw_type_by_el <- raw_type[el_type_idx]
    names(raw_type_by_el) <- category[el_type_idx]
    return(raw_type_by_el)
  }
  )
  
  names(mapping_per_source) <- paste0(raw_type_src_unique, "_type")
  return(mapping_per_source)
}


#' Map types returned by DBpedia Spotlight to a limited set of categories
#'
#' This function takes the output of `get_dbpedia_uris()` and compares values in
#' the different `types` columns such as "DBpedia_type" with a named character
#' vector. The main purpose of this function is to reduce the number of types to
#' a limited set of categories.
#'
#' @param x Either a `character vector` of types or a `data.table` returned by
#'   `get_dbpedia_uris()`.
#' @param mapping_vector A `named character vector` with desired category names
#'   (as names) and types from the DBpedia ontology as values. If `x` is a
#'   character vector of types, the mapping should correspond to types in the
#'   character vector, such as c("LOCATION" = "Place") in which the type "Place"
#'   is mapped to the category "LOCATION". If `x` is the entire `data.table`
#'   returned by `get_dbpedia_uris()`, then the source of the type (i.e.
#'   "DBpedia" or "Wikidata") should be provided in the format c("LOCATION" =
#'   "DBpedia:Place"). This indicates the column(s) which should be used in the
#'   comparison. This can contain more than one pair of class and type.
#' @param other A `character vector` with the name of the class of all types not
#'   matched by the `mapping_vector`.
#' @param verbose A `logical` value - whether to display messages.
#' @param ... Further arguments.
#' @importFrom cli format_error cli_alert_info cli_abort
#' @details If there is more than one match between the retrieved types and the
#'   `mapping vector`, unique categories are sorted alphabetically and
#'   collapsed.
#' @return If the input is a `data.table` the function adds categories to the
#'   input data.table by reference. Otherwise, a `character vector` is returned.
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
#'   x = inaugural_paragraphs[2], # single paragraph
#'   language = "en",
#'   max_len = 5600L,
#'   confidence = 0.5,
#'   api = "http://api.dbpedia-spotlight.org/en/annotate",
#'   verbose = FALSE,
#'   progress = TRUE
#' )
#'
#' mapping_vector_character = c(
#'   "PERSON" = "Person",
#'   "ORGANIZATION" = "Organisation",
#'   "LOCATION" = "Place"
#' )
#'
#' categories <- entity_types_map(
#'   uritab_paragraphs[["DBpedia_type"]],
#'   mapping_vector = mapping_vector_character
#' )
#'
#' # For the entire data.table and multiple types sources:
#'
#' mapping_vector_dt = c(
#'   "PERSON" = "DBpedia:Person",
#'   "ORGANIZATION" = "DBpedia:Organisation",
#'   "ORGANIZATION" = "Wikidata:7278",
#'   "LOCATION" = "DBpedia:Place"
#' )
#'
#' entity_types_map(
#'   uritab_paragraphs,
#'   mapping_vector = mapping_vector_dt
#' )
setMethod(
  "entity_types_map", "character",
  function(x, mapping_vector, other = "MISC", verbose = TRUE
  ) {

    # there is not a check yet whether the vector is named
    if (!is.character(mapping_vector)) {
      stop(format_error(c(
        "{.var mapping_vector} is no character vector.",
        "i" = "The {.var mapping_vector} must be a named character vector."
      )))
    }

    if (!is.character(other) | length(other) > 1) {
      stop(
        format_error("{.var other} not character vector of length {.val 1}.")
      )
    }

    if (!is.character(x)) {
      stop(format_error("{.var x} not character vector."))
    }

    col_split <- strsplit(x, split = "\\|")

    retval <- sapply(col_split, function(row_vector) {
      match_idx <- which(mapping_vector %in% row_vector)
      if (length(match_idx) > 0) {
        category <- sort(unique(names(mapping_vector)[match_idx]))
        if (length(category) > 1) {
          category <- paste0(category, collapse = "|")
        }
      } else {
        category <- other
      }
      return(category)
    }
    )

    return(retval)
  }
)

#' @rdname entity_types_map
setMethod(
  "entity_types_map", "data.table",
  function(x, mapping_vector, other = "MISC", verbose = TRUE) {
    
    if (!inherits(x, "data.table")) {
      cli_abort("`x` must be a `data.table` object.")
    }
    
    # reorganize mapping vector to match columns
    mapping_per_source <- entity_types_categorize(
      x,
      mapping_vector = mapping_vector
    )
    
    # check whether mapping vector is valid
    for (mapping_idx in seq_along(mapping_per_source)) {
      mult_mapping <- which(table(mapping_per_source[[mapping_idx]]) > 1)
      if (length(mult_mapping) > 0) {
        category_name <- names(mapping_per_source)[[mapping_idx]]
        category_name <- gsub("_type$", "", category_name)
        cli_abort(c(
          "Error in mapping vector for {category_name} types:",
          "x" = "Type{?s} {names(mult_mapping)} {?is/are} mapped onto multiple categories.")
        )
      }
    }
    
    category_per_source <- sapply(seq_along(mapping_per_source), function(i) {
      type_src <- names(mapping_per_source)[[i]]
      categories <- entity_types_map(
        x = x[[type_src]],
        mapping_vector = mapping_per_source[[i]],
        other = other,
        verbose = TRUE
      )
    }
    )
    
    # Depending on the number of type sources, this is either a vector or a matrix.
    # Multiple mappings are uniqued and concatenated.
    
    if (length(mapping_per_source) > 1) {
      categories <- apply(
        X = category_per_source,
        MARGIN = 1,
        function(category) {
          # in case that there is an ambiguous mapping earlier, there already
          # could be concatenated entities here. Split up.
          
          category <- unlist(strsplit(category, split = "\\|"))
          
          # now unique, sort and concatenate again
          paste0(sort(unique(category)), collapse = "|")
        }
      )
    } else {
      categories <- as.vector(category_per_source)
    }
    
    # check
    if (length(categories) != nrow(x)) {
      cli_abort(
        "Mapped categories are not equal to all observations in the input `data.table`."
      )
    }
    
    if (verbose)
      cli_alert_info(
        "mapping values in column{?s} {names(mapping_per_source)} to new column {.var category}"
      )
    
    x[, "category" := categories]
    x
  })

