as.data.table.AnnotatedPlainTextDocument <- function(x, what = c("word", "ne")){
  dt <- setDT(as.data.frame(x[["annotation"]]))
  dt_min <- dt[dt[["type"]] %in% what]
  dt_min[, "text" := unlist(lapply(dt_min[["features"]], `[[`, "text"))]
  constituents <- lapply(dt_min[["features"]], `[[`, "constituents")
  dt_min[, "cpos_left" := sapply(constituents, min)]
  dt_min[, "cpos_right" := sapply(constituents, max)]
  dt_min[, "features" := NULL]
  dt_min
}


#' Get DBpedia links.
#' 
#' @param x A `subcorpus` object. Will be coerced to
#'   'AnnotatedPlainTextDocument' from NLP package.
#' @param max_len An `integer` value. The text passed to DBpedia Spotlight may
#'   not exceed a defined length. If it does, an HTTP error results. The known
#'   threshold of 6067 characters is the default value.
#' @param language The language of the input text ("en", "fr", "de", ...) to 
#'   determine the stopwords used. 
#' @param confidence A `numeric` value, the minimum similarity score that serves
#'   as threshold befor DBpedia Spotlight includes a link into the report.
#' @param api An URL of the DBpedia Spotlight API.
#' @param verbose A `logical` value - whether to display progress messages.
#' @export get_dbpedia_links
#' @importFrom cli cli_alert_warning cli_progress_step cli_alert_danger
#' @importFrom polmineR punctuation
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET http_error content
#' @importFrom data.table setnames `:=` setDT setcolorder
#'   as.data.table
get_dbpedia_links <- function(x, language, max_len = 6067L, confidence = 0.35, api = "http://localhost:2222/rest/annotate", verbose = TRUE){
  
  if (verbose) cli_progress_step("turn input into AnnotatedPlainTextDocument")
  stopwords <- dbpedia::dbpedia_stopwords[[language]]
  
  x <- polmineR:::as.AnnotatedPlainTextDocument(
    x = x,
    stoplist = c(stopwords, polmineR::punctuation)
  )
  
  if (nchar(x[["content"]]) > max_len){
    if (verbose) cli_alert_warning(
      "nchar of input text is {nchar(x[['content']])}, truncate to max_len ({.val {max_len}})"
    )
    txt <- substr(x[["content"]], 1, max_len)
  } else {
    txt <- x[["content"]]
  }
  
  
  if (verbose) cli_progress_step("send request to DBpedia Spotlight")
  request <- httr::GET(
    url = api,
    query = list(
      text = txt,
      confidence = confidence
    ),
    httr::add_headers('Accept' = 'application/json')
  )
  
  if (httr::http_error(request)){
    cli_alert_danger("http error response")
    stop("abort")
  }
  
  if (verbose) cli_progress_step("parse result")
  txt <- httr::content(request, as = "text", encoding = "UTF-8")
  json <- jsonlite::fromJSON(txt)
  resources <- as.data.table(json[["Resources"]])
  
  resources_min <- resources[, c("@URI", "@surfaceForm", "@offset")]
  setnames(
    resources_min,
    old = c("@URI", "@surfaceForm", "@offset"),
    new = c("uri", "text", "start")
  )
  resources_min[, "start" := as.integer(resources_min[["start"]]) + 1L]
  setcolorder(resources_min, c("start", "text", "uri"))
  
  ne <- as.data.table(x, what = "entity")
  
  dbpedia_links <- resources_min[ne, on = c("start", "text")]
  dbpedia_links[, "start" := NULL][, "end" := NULL][, "id":= NULL]
  setcolorder(
    dbpedia_links,
    c("cpos_left", "cpos_right", "type", "text", "uri")
  )
  
  dbpedia_links
}

#' Stopwords used by DBpedia Spotlight
#' 
"dbpedia_stopwords"

