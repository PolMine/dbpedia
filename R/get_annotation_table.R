#' Turn information on annotations to table.
#' 
#' @param x A `subcorpus` or `subcorpus_bundle` object.
#' @import methods
#' @exportMethod get_annotation_table
#' @rdname get_annotations_table
setGeneric(
  "get_annotation_table",
  function(x) standardGeneric("get_annotation_table") 
)

#' @rdname get_annotations_table
#' @importFrom tibble as_tibble
setMethod(
  "get_annotation_table",
  "subcorpus",
  function(x) {
    df <- as_tibble(slot(x, "annotations"))
    df[["type"]] <- gsub(
      'type=\\"(.*?)\\"$', "\\1",
      names(slot(x, "annotations")[["highlight"]])
    )
    df[["cpos_left"]] <- x@cpos[,1]
    df[["cpos_right"]] <- x@cpos[,2]
    df
  }
)

#' @rdname get_annotations_table
setMethod(
  "get_annotation_table",
  "subcorpus_bundle",
  function(x) do.call(rbind, lapply(x@objects, get_annotation_table))
)