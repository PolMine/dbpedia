as.data.table.AnnotatedPlainTextDocument <- function(x, what = c("word", "ne")){
  dt <- setDT(as.data.frame(x[["annotation"]]))
  dt_min <- dt[dt[["type"]] %in% what]
  dt_min[, "text" := unlist(lapply(dt_min[["features"]], `[[`, "text"))]
  constituents <- lapply(dt_min[["features"]], `[[`, "constituents")
  dt_min[, "ne_type" := unlist(lapply(dt_min[["features"]], `[[`, "kind"))]
  dt_min[, "cpos_left" := sapply(constituents, min)]
  dt_min[, "cpos_right" := sapply(constituents, max)]
  dt_min[, "features" := NULL]
  dt_min
}

#' @rdname get_dbpedia_links
setGeneric("get_dbpedia_links", function(x, ...) standardGeneric("get_dbpedia_links"))

#' @exportMethod get_dbpedia_links
#' @rdname get_dbpedia_links
setMethod("get_dbpedia_links", "AnnotatedPlainTextDocument", function(x, language, max_len = 6067L, confidence = 0.35, api = "http://localhost:2222/rest/annotate", verbose = TRUE){
  
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
  
  resources_min
})



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
#' @param p_attribute The p-attribute used for decoding a `subcorpus` object.
#' @param ... Further arguments.
#' @exportMethod get_dbpedia_links
#' @importFrom cli cli_alert_warning cli_progress_step cli_alert_danger
#'   cli_progress_done
#' @importFrom polmineR punctuation
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET http_error content
#' @importFrom data.table setnames `:=` setDT setcolorder
#'   as.data.table
#' @importFrom stats setNames
#' @importFrom grDevices heat.colors
#' @importFrom polmineR decode
#' @import methods
#' @docType methods
#' @rdname get_dbpedia_links
setMethod("get_dbpedia_links", "subcorpus", function(x, language, p_attribute = "word", mw = "ne", max_len = 6067L, confidence = 0.35, api = "http://localhost:2222/rest/annotate", verbose = TRUE){
  
  if (verbose) cli_progress_step("convert input to `AnnotatedPlainTextDocument`")
  doc <- decode(
    x,
    to = "AnnotatedPlainTextDocument",
    p_attributes = p_attribute,
    mw = mw,
    stoplist = c(
      dbpedia::dbpedia_stopwords[[language]],
      polmineR::punctuation
    ),
    verbose = FALSE
  )
  if (verbose) cli_progress_done()
  
  links <- get_dbpedia_links(
    x = doc,
    language = language,
    max_len = max_len,
    confidence = confidence,
    api = api,
    verbose = verbose
  )
  
  ne <- as.data.table(doc, what = mw)
  
  dbpedia_links <- links[ne, on = c("start", "text")]
  dbpedia_links[, "start" := NULL][, "end" := NULL][, "id":= NULL]

  retval <- x
  
  retval@cpos <- as.matrix(dbpedia_links[, c("cpos_left", "cpos_right")])
  retval@annotations <- list(
    highlight = sapply(
      dbpedia_links[["ne_type"]], 
      switch,
      PERSON = "yellow",
      LOCATION = "lightgreen",
      ORGANIZATION = "lightskyblue2",
      MISC = "lightgrey"
    ),
    href = dbpedia_links[["uri"]],
    tooltips = ifelse(
      is.na(dbpedia_links[["uri"]]),
      "[no uri]",
      dbpedia_links[["uri"]]
    )
  )
      

  retval
})


#' Stopwords used by DBpedia Spotlight
#' 
"dbpedia_stopwords"

