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