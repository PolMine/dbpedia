#' Detect overlapping entities
#'
#' `detect_overlap()` detects overlapping regions of annotations in a `data.table`
#' object and assigns an overlap id to each group of overlapping entities.
#' **This functionality is currently experimental**. The correctness of results
#' is subject to ongoing checks. The syntax is likely to change in future
#' updates.
#'
#' @param x The input `data.table` containing a start and an end position of
#'   regions to be compared.
#' @param start_col A `character vector` of length 1, referring to the name of
#'   the column containing the start positions of the regions to be compared.
#' @param end_col A `character vector` of length 1, referring to the name of the
#'   column containing the end positions of the regions to be compared.
#' @param verbose A `logical value` of whether to print messages or not.
#' @return The input `data.table` is modified by reference. The column "ovl_id"
#'   (overlap ID) is added. Each group of overlapping entities is annotated with
#'   a unique ID. The functions `categorize_overlap()` and `resolve_overlap()`
#'   can be used thereafter.
#' @importFrom data.table foverlaps melt
#' @importFrom cli cli_abort
#' @export
#' @examples
#' doc <- "Der Deutsche Bundestag tagt in Berlin."
#'
#' x <- get_dbpedia_uris(
#'   x = doc,
#'   language = "de",
#'   max_len = 5600L,
#'   confidence = 0.35,
#'   api = "https://api.dbpedia-spotlight.org/de/annotate",
#'   types = character(),
#'   support = 20,
#'   types_src = c("DBpedia", "Wikidata"),
#'   verbose = TRUE
#' ) |>
#'   detect_overlap(start_col = "start", verbose = TRUE)
detect_overlap <- function(x,
                           start_col,
                           end_col = NULL,
                           verbose = TRUE) {

  if (!start_col %in% colnames(x)) {
    cli_abort(
      c("x" = "{.var start_col} is no column in input data.table {.var x}.")
    )
  }

  # Preparation: Add column "end" if necessary
  if (is.null(end_col)) {
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
    x[, "ovl_id" := detect_overlap_aux(.SD,
                                     group_id = .GRP,
                                     start_col = start_col,
                                     end_col = end_col,
                                     verbose = verbose),
      by = doc]

  } else {

    x[, "ovl_id" := detect_overlap_aux(input_dt = x,
                                     group_id = NULL,
                                     start_col = start_col,
                                     end_col = end_col,
                                     verbose = verbose)]
  }

  invisible(x)
}

detect_overlap_aux <- function(input_dt,
                               group_id,
                               start_col,
                               end_col,
                               verbose = TRUE) {

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

    # With nested overlaps, overlaps_out can have multiple rows all referring to
    # the same overlap. See which intersect.

    if (nrow(overlaps_out) == 1) {
      overlaps_out[, overlap_group_idx := 1]
    } else {
      idx_ranges <- lapply(1:nrow(overlaps_out), function(i) {
        overlaps_out[i, ][["xid"]]:overlaps_out[i, ][["yid"]]
      }
      )

      # iterate through every overlap and see if it overlaps with another
      # overlap

      overlap_groups <- sapply(seq_along(idx_ranges), function(i) {
        range_i <- idx_ranges[[i]]
        others_idx <- seq_along(idx_ranges)[-i]
        other_ranges <- idx_ranges[others_idx]
        intersect_idx_i <- which(sapply(other_ranges, function(other_range) ifelse(length(intersect(range_i, other_range)) > 0, TRUE, FALSE)))
        if (length(intersect_idx_i) > 0) {
          grp_idx <- others_idx[intersect_idx_i]
          overlap_group <- sort(c(i, grp_idx))
        } else {
          overlap_group <- i
        }
      }
      )

      # it is possible that more complex overlaps are not fully grouped yet.
      # Consider index vectors x = c(1, 2), y = c(2, 3) and z = c(3, 4). They
      # at least partially overlap but are not all grouped above.

      if (length(overlap_groups) > 1) {
        for (i in seq_along(overlap_groups)) {
          same_group_idx <- which(sapply(overlap_groups[-i], function(other_group) ifelse(length(intersect(overlap_groups[[i]], other_group)) > 0, TRUE, FALSE)))
          if (length(same_group_idx) > 0) {
            group_min <- min(unlist(overlap_groups[same_group_idx]))
            group_max <- max(unlist(overlap_groups[same_group_idx]))
            overlap_groups[[i]] <- group_min:group_max
          }
        }
      }

      # overlapping entities should now all have the same region.
      overlap_groups_unique <- unique(overlap_groups)
      overlaps_out[, overlap_group_idx := which(sapply(overlap_groups_unique, function(x) .I %in% x)), by = .I]
    }

    # add an ID for individual overlaps
    if (!is.null(group_id)) {
      overlaps_out[, overlap_id := sprintf("ovl_%s_%s",
                                           group_id,
                                           overlap_group_idx)]
    } else {
      overlaps_out[, overlap_id := sprintf("ovl_%s", overlap_group_idx)]
    }

    # and make from wide to long table for join
    overlaps_out_long <- melt(overlaps_out,
                              id.vars = "overlap_id",
                              measure.vars = c("xid", "yid"),
                              value.name = "row_idx")

    overlaps_out_long[, variable := NULL]

    if (isTRUE(verbose)) {
      cli_alert_info(
        "Found {.val {nrow(overlaps_out)}} overlaps concerning {.val {nrow(overlaps_out_long)}} entities.",
        wrap = TRUE) 
    }

    # merge to input
    ovl_dt[overlaps_out_long, on = "row_idx", "ovl_id" := i.overlap_id]

    retval <- ovl_dt[["ovl_id"]]
  }
}

#' Categorizing overlapping entities
#'
#' The function categorizes overlapping regions of annotations in a `data.table`
#' object and assigns types to each overlapping entity.
#' **This functionality is currently experimental**. The correctness of results
#' is subject of ongoing checks. The syntax is likely to change in future
#' updates.
#'
#' @param x The input `data.table` containing an overlap ID for each group of
#'   overlapping entities.
#' @param experimental A `logical value` of whether to add "outer" or "inner"
#'   annotations to partially overlapping entities. See discussion in "details".
#' @param corpus A `character vector` with the name of the CWB corpus the
#'   documents are retrieved from. Used to create a token stream when argument
#'   `experimental` is set to TRUE. Defaults to NULL.
#' @return A `data.table` with the input `data.table` plus multiple column
#'   containing `boolean` values. The function `resolve_overlap()` can be used
#'   thereafter to select the most appropriate entity in the overlap.
#' @details `categorize_overlap()` returns the input data.table with additional
#'   columns containing `boolean` values for specific overlap types. These types
#'   are informed by discussions about nested entities in the field of Named
#'   Entity Recognition, see for example Benikova et al. (2014). One entity can
#'   contain multiple overlap types. These columns currently are:
#'   * `ovl_longest`: If entities completely overlap, i.e. all entities are
#'   part of another entity, `ovl_longest` = TRUE describes the longest entity
#'   in the overlap. Example: "Vice President Gore" is annotated both fully
#'   ("Vice President Gore") and as only a part of the span (e.g. "Gore"). If
#'   both entities refer to the same URI, then "Vice President Gore" is assigned
#'   as the "longest" match while "Gore" is assigned as the "shortest" match in
#'   the overlap.
#'   * `ovl_shortest`: If entities completely overlap, i.e all entities are part
#'   of another entity, `ovl_shortest` = TRUE indicates the shortest entity in
#'   the overlap. See "Gore" in the example above.
#'   * `ovl_partial`: If entities overlap only partially, this is indicated by
#'   `ovl_partial` = TRUE. Example: The span "Crude oil prices" is annotated as
#'   both "Crude oil" and "oil prices" with distinct URIs.
#'   * `ovl_inner`: If entities overlap only partially (see above), the
#'   character span included in all entities (i.e. their intersecting
#'   characters) are indicated by `ovl_inner` = TRUE. Example: "Crude oil
#'   prices" is annotated by both "Crude oil" and "oil prices". The inner
#'   overlap of these entities is "oil". Only created when argument
#'   `experimental` = TRUE.
#'   * `ovl_outer`: If entities overlap only partially (see above), the span
#'   describing the maximal extension of all entities is indicated by
#'   `ovl_outer` = TRUE. Example: "Crude oil prices" is annotated by both "Crude
#'   oil" and "oil prices". The outer overlap of these entities is "Crude oil
#'   prices". Only created when argument `experimental` = TRUE.
#'   * `ovl_multiple`: If multiple entities describe the same span, this is
#'   indicated by `ovl_multiple` = TRUE. This can happen when the tokenization
#'   of DBpedia Spotlight is different from the tokenization of the input data
#'   resulting in sub-token annotations. This also potentially occurs with
#'   `ovl_inner` and `ovl_outer` when different entities are concatenated.
#'   Example: "Crude oil prices" is concatenated by the annotation of "Crude
#'   oil" and "oil prices", both described by different URIs. The combined
#'   entity contains then two distinct URIs, which is indicated by
#'   `ovl_multiple` = TRUE in addition to `ovl_outer` = TRUE.
#'   * `ovl_distinct`: If entities overlap only partially (see above) and both
#'   entities describe distinct concepts, then this is indicated by
#'   `ovl_distinct` = TRUE. Example: The span "Crude oil prices" is annotated as
#'   both "Crude oil" and "oil prices" with distinct URIs and thus is both
#'   `ovl_partial` = TRUE and `ovl_distinct` = TRUE.
#'   * `ovl_undetermined`: Overlaps which do not fall under the categories above
#'   are indicated by `ovl_undetermined` = TRUE.
#'
#'   The main purpose of the - currently experimental - types of `ovl_outer` and
#'   `ovl_inner` is to identify instances in which multiple overlapping entities
#'   describe the same concept.
#' @inheritParams detect_overlap
#' @importFrom polmineR get_token_stream
#' @references Darina Benikova, Chris Biemann, and Marc Reznicek. 2014. NoSta-D
#'   Named Entity Annotation for German: Guidelines and Dataset. In Proceedings
#'   of the Ninth International Conference on Language Resources and Evaluation
#'   (LREC'14), pages 2524–2531, Reykjavik, Iceland. European Language Resources
#'   Association (ELRA).
#' @export
#' @examples
#' doc <- "Crude oil prices on the rise."
#'
#' x <- get_dbpedia_uris(
#'   x = doc,
#'   max_len = 5600L,
#'   confidence = 0.35,
#'   api = "https://api.dbpedia-spotlight.org/en/annotate",
#'   language = "en",
#'   types = character(),
#'   support = 20,
#'   types_src = c("DBpedia", "Wikidata"),
#'   verbose = TRUE
#' ) |>
#'   detect_overlap(start_col = "start", verbose = TRUE) |>
#'   categorize_overlap(start_col = "start",
#'                      end_col = "end",
#'                      experimental = TRUE,
#'                      verbose = TRUE)
categorize_overlap <- function(x, start_col, end_col, experimental = FALSE, corpus = NULL, verbose = TRUE) {

  if (!"ovl_id" %in% colnames(x)) {
    cli_abort(c(
      "{.var x} must contain overlap IDs created by {.fn detect_overlaps}",
      "i" = "Did you run {.fn detect_overlaps} before?",
      "x" = "{.var x} does not contain a column named {.var ovl_id}. Abort."
    )
    )
  }

  # Create new column with document type "NA"
  x[, "ovl_type" := ifelse(is.na(x[["ovl_id"]]), NA_character_, "ovl_undetermined")]

  # set key for later foverlaps
  setkeyv(x, c(start_col, end_col))

  if (isTRUE(verbose)) {
    cli_alert_info(text = "Determining full and partial overlaps and spans with multiple URIs.")
  }

  x[, c(start_col, end_col, "dbpedia_uri", "text", "types", "ovl_type") := list(
    .SD[[start_col]],
    .SD[[end_col]],
    .SD[["dbpedia_uri"]],
    .SD[["text"]],
    .SD[["types"]],
    get_outer_inner_ovl_aux(.SD, start_col = start_col, end_col = end_col, verbose = verbose)),
    by = "ovl_id"]

  # For "partial" matches, create an inner and an outer version of the
  # annotation. This is currently experimental as it introduces annotations not
  # provided by DBpedia Spotlight.

  if (isTRUE(experimental)) {

    if (isTRUE(verbose)) {
      cli_alert_info(text = "Finding outer and inner segments for partial matches. This is experimental.")
    }

    if (x[x[["ovl_type"]] %in% c("ovl_partial", "ovl_partial|ovl_distinct"), .N] > 0) {
      overlaps_outer_dt <- x[x[["ovl_type"]] %in% c("ovl_partial", "ovl_partial|ovl_distinct"),
                             list(
                               doc = ifelse("doc" %in% colnames(.SD), .SD[["doc"]], NA),
                               start = min(.SD[[start_col]]),
                               end = max(.SD[[end_col]]),
                               dbpedia_uri = paste(unique(.SD[["dbpedia_uri"]]), collapse = "|"),
                               text = get_combined_text(.SD, start_col = start_col, end_col = end_col, segment = "outer", corpus = corpus),
                               types = ifelse(length(unique(.SD[["dbpedia_uri"]])) == 1, unique(.SD[["types"]]), list(list())),
                               ovl_type = ifelse(length(unique(.SD[["dbpedia_uri"]])) == 1, "ovl_partial|ovl_outer", "ovl_partial|ovl_multiple|ovl_outer")
                             ),
                             by = "ovl_id"]

      overlaps_inner_dt <- x[x[["ovl_type"]] %in% c("ovl_partial", "ovl_partial|ovl_distinct"),
                             list(
                               doc = ifelse("doc" %in% colnames(.SD), .SD[["doc"]], NA),
                               start = min(get_inner_overlap_range(.SD, start_col = start_col, end_col = end_col)),
                               end = max(get_inner_overlap_range(.SD, start_col = start_col, end_col = end_col)),
                               dbpedia_uri = paste(unique(.SD[["dbpedia_uri"]]), collapse = "|"),
                               text = get_combined_text(.SD, start_col = start_col, end_col = end_col, segment = "inner", corpus = corpus),
                               types = ifelse(length(unique(.SD[["dbpedia_uri"]])) == 1,
                                              unique(.SD[["types"]]),
                                              list(list())),
                               ovl_type = ifelse(length(unique(.SD[["dbpedia_uri"]])) == 1, "ovl_partial|ovl_inner", "ovl_partial|ovl_multiple|ovl_inner")
                             ),
                             by = "ovl_id"]

      add_ents_dt <- rbind(overlaps_outer_dt, overlaps_inner_dt)

      if (all(is.na(add_ents_dt[["doc"]]))) {
        add_ents_dt[, doc := NULL]
      }

      if (!start_col %in% colnames(add_ents_dt)) {
        data.table::setnames(add_ents_dt, old = "start", new = start_col)
      }

      if (!end_col %in% colnames(add_ents_dt)) {
        data.table::setnames(add_ents_dt, old = "end", new = end_col)
      }

      x <- data.table::rbindlist(list(x, add_ents_dt), use.names = TRUE, fill = TRUE)
    }
  }

  # make to boolean columns
  cols <- c("ovl_longest", "ovl_shortest", "ovl_inner", "ovl_outer",
            "ovl_partial", "ovl_multiple", "ovl_distinct", "ovl_undetermined")

  x[!is.na(x[["ovl_id"]]), (cols) := lapply(cols, function(x) grepl(pattern = x, x[["ovl_type"]])), by = .I]

  # after this, the ovl_type column is not needed anymore.
  x[, "ovl_type" := NULL]

  if ("doc" %in% colnames(x)) {
    setorderv(x, c("doc", start_col))
  } else {
    setorderv(x, start_col)
  }

  return(x)
}

# auxiliary functions for categorize_overlap()

get_outer_inner_ovl_aux = function(.SD, start_col, end_col, verbose = verbose) {

  if (!any(is.na(.SD[["ovl_type"]]))) {

    overlap_types <- .SD[["ovl_type"]]

    entity_range <- unique(
      apply(.SD, MARGIN = 1, FUN = function(x) x[[start_col]]:x[[end_col]])
    )

    n_entities <- ifelse(is.matrix(entity_range), nrow(entity_range), length(entity_range))

    if (n_entities == 1L & length(unique(.SD[["dbpedia_uri"]])) > 1) {
      return("ovl_multiple")
    }

    # check whether entities are full overlaps (i.e. whether one entity
    # covers all other entities).

    overlap_dt <- data.table::foverlaps(.SD,
                                        .SD,
                                        type = "within",
                                        which = TRUE)

    if (all(overlap_dt[["xid"]] == overlap_dt[["yid"]])) {

      # in this case, the entities only fully overlap with themselves but not
      # with other entities, indicating a partial overlap.

      # check if both entities which overlap partially refer to the same URI.

      if (length(unique(.SD[["dbpedia_uri"]])) == 1) {
        return("ovl_partial")
      } else {
        return("ovl_partial|ovl_distinct")
      }

    }

    # check if there is a row in which the short index is in x and the long
    # index is in y. This suggests that these are totally within each.

    inner_idx <- overlap_dt[xid > yid][["xid"]]

    if (length(inner_idx) > 0) {
      overlap_types[inner_idx] <- "ovl_inner"

      # this should result in rows which are both shortest and inner and might
      # result in rows which are only inner (if there can be more nested
      # entities)
    }

    outer_idx <- overlap_dt[xid > yid][["yid"]]

    if (length(outer_idx) > 0) {
      overlap_types[outer_idx] <- "ovl_outer"

      # this should result in rows which are both longest and outer and might
      # result in rows which are only outer (if there can be more nested
      # entities)
    }

    # find out which is the shortest entity in the overlap
    entity_lengths <- .SD[[end_col]] - .SD[[start_col]]
    short_idx <- which(entity_lengths == min(entity_lengths, na.rm = TRUE))

    if (length(short_idx) == 1) {
      overlap_types[short_idx] <- "ovl_shortest"
    }

    # find out which is the longest entity in the overlap
    long_idx <- which(entity_lengths == max(entity_lengths, na.rm = TRUE))

    if (length(long_idx) == 1) {
      overlap_types[long_idx] <- "ovl_longest"
    }

    return(overlap_types)
  } else {
    return(NA_character_)
  }
}

get_inner_overlap_range = function(.SD, start_col, end_col) {
  ranges <- lapply(1:nrow(.SD), function(i) .SD[i, ][[start_col]]:.SD[i, ][[end_col]])
  retval <- Reduce(intersect, ranges)
}

get_combined_text = function(.SD, start_col, end_col, segment, corpus = NULL) {
  if (nrow(.SD) != 2) {
    cli_alert_warning(
      text = "Retrieving text based on character vectors is not yet implemented for overlaps with more than two entities.
     Skipping and returning NA"
    )
    return(NA)
  }

  ranges <- lapply(1:nrow(.SD), function(i) .SD[i, ][[start_col]]:.SD[i, ][[end_col]])

  if (segment == "inner") {
    inner <- sort(Reduce(intersect, ranges))
    if (is.null(corpus)) {
      text_as_chars <- strsplit(.SD[["text"]], split = "")[[2]]
      trimws(paste(text_as_chars[seq_along(inner)], collapse = ""))
    } else {
      get_token_stream(inner,
                       corpus = corpus,
                       p_attribute = "word",
                       collapse = " ")
    }

  } else if (segment == "outer") {
    if (is.null(corpus)) {
      left <- Reduce(setdiff, ranges)
      text_as_chars <- strsplit(.SD[["text"]], split = "")
      text_left <- paste(text_as_chars[[1]][seq_along(left)], collapse = "")
      text_right <- paste(text_as_chars[[2]], collapse = "")
      paste(c(text_left, text_right), collapse = "")
    } else {
      get_token_stream(sort(Reduce(union, ranges)),
                       corpus = corpus,
                       p_attribute = "word",
                       collapse = " ")
    }
  }
}

#' Resolve overlapping entities
#'
#' The function resolves detected and categorized overlapping regions of
#' annotations in a `data.table` object.
#' **This functionality is currently experimental**. The correctness of results
#' is subject of ongoing checks. The syntax is likely to change in future
#' updates.
#'
#' @param x The input `data.table` containing annotations with classified
#'   overlaps.
#' @param keep A `character vector` of the names of the overlap types to be
#'   kept. These should refer to the types described in `?categorize_overlap`
#'   without the leading `ovl_`, i.e. to `longest`, `shortest`, etc. If the
#'   length of the `character vector` is longer than 1, columns are evaluated in
#'   order, i.e the first column is preferred over the second, etc.
#' @param omit A `character vector` of the names of the overlap types to be
#'   omitted from the final output. These should refer to the types described in
#'   `?categorize_overlap` without the leading `ovl_`, i.e. to `longest`,
#'   `shortest`, etc.
#' @param tiebreak A `character vector` describing a strategy to handle
#'   unresolved overlaps, i.e. groups of overlapping entities in which the
#'   combination of `keep` and `omit` does not indicate a single remaining
#'   entity. Valid values are `first` (for each unresolved overlap, keep the
#'   first entity in the order of the row index), `sample` (for each unresolved
#'   overlap, sample one random entity) or `remove` (for each unresolved
#'   overlap, remove all entities).
#' @inheritParams detect_overlap
#' @return A `data.table` only containing one entity per overlap.
#' @export
resolve_overlap = function(x, keep, omit = NULL, tiebreak, verbose = TRUE) {

  ovl_unique_before <- length(unique(x[!is.na(ovl_id), ][["ovl_id"]])) # number of overlaps

  # first, keep all non-overlapping entities
  x[is.na(x[["ovl_id"]]), ovl_keep := 1L]

  if (isTRUE(verbose)) {
    cli_alert_info("Identifing entities to {.strong keep}.")
  }

  for (i in seq_along(keep)) {
    keep_i <- paste0("ovl_", keep[i])
    x[x[, .I[which(get(keep_i) == TRUE)], by = "ovl_id"]$V1, c("ovl_keep", "ovl_by") := list(i, keep[i])]
  }

  if (!is.null(omit)) {

    if (isTRUE(verbose)) {
      cli_alert_info("Identifing entities to {.strong omit}.")
    }

    for (i in seq_along(omit)) {
      omit_i <- paste0("ovl_", omit[i])
      x[x[, .I[which(get(omit_i) == TRUE)], by = "ovl_id"]$V1, ovl_keep := -1L]
    }
  }

  if (isTRUE(verbose)) {
    cli_alert_info("Resolving ties.")
  }

  tiebreak_fun = function(.SD, tiebreak_mode = tiebreak) {
    v_keep <- .SD[["ovl_keep"]]

    if (all(is.na(v_keep))) {
      candidates <- seq_along(v_keep)
    } else if (all(is.na(v_keep[!v_keep < 0]))) {
      candidates <- seq_along(v_keep)
      candidates[which(v_keep < 0)] <- NA
    } else if (length(which(v_keep == min(v_keep[!is.na(v_keep) & v_keep > 0]))) > 1) {
      candidates <- which(v_keep == min(v_keep[!is.na(v_keep) & v_keep > 0]))
    } else {
      return(list(.SD[["ovl_keep"]], .SD[["ovl_by"]]))
    }
    if (tiebreak_mode == "first") {
      v_keep <- rep(NA, times = length(v_keep))
      v_keep[candidates[!is.na(candidates)][1]] <- 1L
    } else if (tiebreak_mode == "sample") {
      v_keep <- rep(NA, times = length(v_keep))
      v_keep[sample(candidates[!is.na(candidates)], 1)] <- 1L
    } else if (tiebreak_mode == "remove") {
      v_keep <- rep(-1L, times = length(v_keep))
    } else {
      cli::cli_alert_warning(text = "No way to break ties provided in argument {.var tiebreak}.")
    }
    return(list(
      v_keep,
      "tiebreak"
    )
    )
  }

  x[!is.na(ovl_id), c("ovl_keep", "ovl_by") := tiebreak_fun(.SD, tiebreak_mode = tiebreak), by = "ovl_id"]
  x <- x[x[, .I[which(.SD[["ovl_keep"]] > 0 & .SD[["ovl_keep"]] == min(.SD[["ovl_keep"]], na.rm = TRUE))], by = ovl_id][["V1"]], ]

  ovl_unique_after <- length(unique(x[!is.na(x[["ovl_id"]]), ][["ovl_id"]]))

  if (isTRUE(verbose)) {
    cli::cli_alert_info(
      text = "Resolved {ovl_unique_after} out of {ovl_unique_before} overlap{?s}. Removed {ovl_unique_before - ovl_unique_after} overlap{?s}."
    )
  }

  x[, ovl_keep := NULL]

  # as a result, all overlap IDs should only occur once
  stopifnot(all(table(x$ovl_id) == 1))

  return(x)
}
