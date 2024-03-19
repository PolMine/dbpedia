#' Detect overlapping entities
#'
#' The function detects overlapping regions of annotations in a `data.table`
#' object and assigns an overlap id to each group of overlapping entities.
#'
#' @param x the input `data.table` containing a start and an end position of
#'   regions to be compared.
#' @param start_col a `character vector` of length 1, referring to the name of
#'   the column containing the start positions of the regions to be compared.
#' @param end_col a `character vector` of length 1, referring to the name of the
#'   column containing the end positions of the regions to be compared.
#' @param verbose a `logical value` of whether to print messages or not.
#' @return The input `data.table` is modified by reference. The column "ovl_id"
#'   (overlap ID) is added. Each group of overlapping entities is annotated with
#'   a unique ID. The functions `classify_overlap()` and `resolve_overlap()` can
#'   be used thereafter.
#' @importFrom data.table foverlaps melt
#' @importFrom cli cli_abort
#' @export
#' @examples
#' doc <- "Der Deutsche Bundestag tagt in Berlin."
#'
#' get_dbpedia_uris(
#'   x = doc,
#'   language = getOption("dbpedia.lang"),
#'   max_len = 5600L,
#'   confidence = 0.35,
#'   api = getOption("dbpedia.endpoint"),
#'   types = character(),
#'   support = 20,
#'   types_src = c("DBpedia", "Wikidata"),
#'   verbose = TRUE
#' ) |>
#'   detect_overlap(start = "start", verbose = TRUE)
detect_overlap <- function(x, start_col, end_col = NULL, verbose = TRUE) {

  # Preparation: Add column "end" if necessary
  if (is.null(end_col)) {
    # check if there is already a column called "end".
    if ("end" %in% colnames(x)) {
      cli_abort(
        c("{.var end_col} is NULL but there is already a column named {.val end}.",
          "x" = "Cannot add new column {.val end} to the input data.table.")
      ) 
    }

    cli_alert_warning(
      "Argument {.var end} is NULL. Setting {.var end} to {.var start_col + nchar}.
      This can be wrong in case of CWB corpora."
    )
    x[, end := get(start_col) + nchar(text)]
    end_col <- "end"
  }

  if ("doc" %in% colnames(x)) {
    x[, ovl_id := detect_overlap_aux(.SD,
                                     group_id = .GRP,
                                     start_col = start_col,
                                     end_col = end_col,
                                     verbose = verbose),
      by = doc]
  } else {
    x[, ovl_id := detect_overlap_aux(x,
                                     group_id = NULL,
                                     start_col = start_col,
                                     end_col = end_col,
                                     verbose = verbose)]
  }
  return(x)
}

detect_overlap_aux <- function(input_dt, group_id, start_col, end_col, verbose = TRUE) {

  # create subset of data.table
  ovl_dt <- input_dt[, which(colnames(input_dt) %in% c("doc", "start", "end",
                                                       "text", start_col, end_col)),
                     with = FALSE]

  # add temporary row idx for later join
  ovl_dt[, row_idx := 1:nrow(ovl_dt)]

  # set keys for the following foverlaps. Should be start and end.
  setkeyv(ovl_dt, c(start_col, end_col))

  # foverlaps returns a data.table with row numbers in x and y (both x). These
  # pairs are redundant in the sense that there is both "1-2" and "2-1" (first
  # row in x overlaps with the second row in y and second row in y overlaps with
  # the first row in x).

  # To get only relevant overlaps (i.e. not the same entity in both
  # data.tables), subset by those in which the row idx in x is smaller than in
  # y.

  overlaps_out <- foverlaps(ovl_dt, ovl_dt, type = "any", which = TRUE)[xid < yid]

  if (nrow(overlaps_out) == 0) {

    if (isTRUE(verbose)) {
      cli_alert_info("No overlaps found. Returning NA", wrap = TRUE) 
    }

    retval <- NA_character_

  } else {

    # add an ID for individual overlaps
    if (!is.null(group_id)) {
      overlaps_out[, overlap_id := sprintf("ovl_%s_%s",
                                           group_id,
                                           1:nrow(overlaps_out))]
    } else {
      overlaps_out[, overlap_id := sprintf("ovl_%s", 1:nrow(overlaps_out))] 
    }

    # and make from wide to long table for join
    overlaps_out_long <- melt(overlaps_out,
                              id.vars = "overlap_id",
                              measure.vars = c("xid", "yid"),
                              value.name = "row_idx")

    # remove variable name
    overlaps_out_long[, variable := NULL]

    # print number of overlapping entities and rows
    if (isTRUE(verbose)) {
      cli_alert_info(
        "Found {.val {nrow(overlaps_out)}} overlaps concerning {.val {nrow(overlaps_out_long)}} entities.",
        wrap = TRUE) 
    }

    # merge to input
    ovl_dt[overlaps_out_long, on = "row_idx", ovl_id := i.overlap_id]

    retval <- ovl_dt[["ovl_id"]]
  }

  return(retval)
}
