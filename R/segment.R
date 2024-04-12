#' Cut long string into overlapping segments
#' 
#' Strings that are too long to be processed by DBpedia Spotlight are cut into
#' overlapping segments that can be processed. Overlaps ensure that contextual
#' information is available for all entities.
#' 
#' @return A named character vector. The names are integer numbers that indicate
#'   the character offset from the original string.
#' @param x A string (length-one character vector) to process.
#' @param overlap Number of overlapping characters.
#' @inheritParams get_dbpedia_uris
#' @export
#' @importFrom curl curl_escape
#' @examples
#' library(polmineR)
#' use("RcppCWB") # make REUTERS corpus available
#' 
#' article <- corpus("REUTERS") %>%
#'   subset(id == "236") %>% # the longest article in the REUTERS corpus
#'   get_token_stream(p_attribute = "word", collapse = " ")
#' 
#' segs <- segment(x = article, max_len = 500, overlap = 100)
segment <- function(x, max_len = 7990L, overlap = 500L){
  # check that length(x) == 1L
  
  df <- data.frame(src = strsplit(x, split = " ")[[1]])
  df[["begin"]] <- cumsum(c(1L, (nchar(df$src) + 1L)[1L:(nrow(df) - 1L)]))
  
  df[["esc"]] <- curl::curl_escape(df[["src"]])
  df[["begin_esc"]] <- cumsum(c(1L, (nchar(df$esc) + 3L)[1L:(nrow(df) - 1L)]))
  df[["end_esc"]] <- df[["begin_esc"]] + nchar(df[["esc"]])
  
  # The total number of characters of the escaped string is the beginning of 
  # the last offset plus the nchar of the last token
  nchar_esc <- df$begin_esc[nrow(df)] + nchar(df$esc[nrow(df)]) - 1L
  
  # based on paper & pencil math
  n_segments <- ceiling((nchar_esc - overlap) / (max_len - overlap)) + 2
  
  if (n_segments > 1){
    half <- floor(max_len / 2)
    last <- nchar_esc - half
    anchors <- c(half, last)
    
    if (n_segments > 2){
      anchors <- sort(c(
        anchors,
        half + cumsum(
          rep((last - half) / (n_segments - 1), times = n_segments - 2)
        )
      ))
    }
    
    y <- lapply(
      seq_along(anchors),
      function(i){
        from <- if (i == 1L){
          1L
        } else {
          min(which(df[["begin_esc"]] > (anchors[i] - half)))
        }
        
        to <- if (i == length(anchors)){
          nrow(df)
        } else {
          max(which(df[["end_esc"]] < (anchors[i] + half)))
        }
        df[from:to,]
      }
    )
    
    segments <- lapply(lapply(y, `[[`, "src"), paste, collapse = " ")
    names(segments) <- lapply(y, `[`, 1, "begin")
  } else {
    segments <- list(x)
    names(segments) <- as.character(1)
  }
  
  nchar_seg_esc <- nchar(
    unlist(lapply(lapply(y, `[[`, "esc"), paste, collapse = "%20"))
  )
  
  if (any(nchar_seg_esc > max_len))
    cli_alert_warning("segments exceed `max_len`")

  unlist(segments)
}
