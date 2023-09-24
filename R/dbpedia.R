`:=` <- function(...) NULL
.SD <- NULL

#' Set and report status of DBpedia Spotlight
#' 
#' Check whether Docker container with DBpedia Spotlight is running locally 
#' and set options 'dbpedia.lang' and 'dbpedia.endpoint' accordingly.
#' @return Object of class `dbpedia_spotlight_stats`, a `list` with elements
#'   "docker" (`TRUE`/`FALSE`), "lang" and "endpoint".
#' @rdname dbpedia_spotlight_status
#' @export
#' @examples
#' dbpedia_spotlight_status()
#' getOption("dbpedia.endpoint")
#' getOption("dbpedia.lang")
dbpedia_spotlight_status <- function(){
  
  status <- list(docker = FALSE, lang = NA_character_, api = NA_character_)
  class(status) <- c("dbpedia_spotlight_status", class(status))
  
  stdout <- system2(
    command = "docker",
    args = c("container", "ls"),
    stdout = TRUE
  )
  if (grepl("dbpedia/dbpedia-spotlight", stdout[2])){
    status[["docker"]] <- TRUE
    status[["lang"]] <- gsub(
      '^.*"spotlight.sh\\s+(\\w{2})".*$',
      "\\1",
      stdout[2]
    )
    status[["endpoint"]] <- "http://localhost:2222/rest/annotate"
  } else {
    status[["docker"]] <- FALSE
    status[["lang"]] <- "en"
    status[["endpoint"]] <- "http://api.dbpedia-spotlight.org/en/annotate"
  }
  
  options("dbpedia.lang" = status[["lang"]])
  options("dbpedia.endpoint" = status[["endpoint"]])

  status
}

#' @param x `dbpedia_spotlight_status` object to be printed.
#' @param ... Further arguments passed into `print()`. Unused / required for 
#'   technical reasons.
#' @exportS3Method
#' @rdname dbpedia_spotlight_status
#' @importFrom cli col_cyan cli_text cli_bullets style_bold col_cyan
print.dbpedia_spotlight_status <- function(x, ...){
  cli_text(style_bold("DBpedia Spotlight settings:"))
  cli_bullets(
    c(
      "*" = "endpoint: {col_cyan({x[['endpoint']]})}",
      "*" = "language: {col_cyan({x[['lang']]})}"
    )
  )
  
  invisible(NULL)
}


as.data.table.AnnotatedPlainTextDocument <- function(x, what = NULL){
  dt <- setDT(as.data.frame(x[["annotation"]]))
  if (!is.null(what)){
    dt <- dt[dt[["type"]] %in% what]
    dt[, "text" := unlist(lapply(dt[["features"]], `[[`, "text"))]
    constituents <- lapply(dt[["features"]], `[[`, "constituents")
    dt[, "ne_type" := unlist(lapply(dt[["features"]], `[[`, "kind"))]
    dt[, "cpos_left" := sapply(constituents, min)]
    dt[, "cpos_right" := sapply(constituents, max)]
    dt[, "features" := NULL]
  } else {
    dt <- dt[, "features" := NULL]
  }
  dt
}


#' Add annotation for highlighting to subcorpus.
#' 
#' @param x A `subcorpus` object.
#' @export as_annotation
#' @importFrom RcppCWB cl_struc2str
as_annotation <- function(x){
  ne_type <- cl_struc2str(
    corpus = x@corpus,
    registry = x@registry_dir,
    struc = x@strucs,
    s_attribute = x@s_attribute_strucs
  )
  x@annotations <- list(
    highlight = sapply(
      ne_type,
      switch,
      PERSON = "yellow",
      LOCATION = "lightgreen",
      ORGANIZATION = "lightskyblue",
      MISC = "lightgrey"
    )
  )
  x
}

#' @rdname get_dbpedia_uris
#' @return A `data.table` with columns 'dbpedia_uri' and 'text'. Depending on
#'   input object, further columns are ...
setGeneric("get_dbpedia_uris", function(x, ...) standardGeneric("get_dbpedia_uris"))

#' @exportMethod get_dbpedia_uris
#' @rdname get_dbpedia_uris
#' @examples
#' # Process AnnotatedPlainTextDocument (example available in NLP package)
#' doc <- readRDS(system.file("texts", "stanford.rds", package = "NLP"))
#' tab <- get_dbpedia_uris(x = doc, language = "en")
#' 
#' tab <- get_dbpedia_uris(
#'   x = doc,
#'   language = "en",
#'   api = "http://api.dbpedia-spotlight.org/en/annotate"
#' )
#' 
#' tab
setMethod("get_dbpedia_uris", "AnnotatedPlainTextDocument", function(x, language = getOption("dbpedia.lang"), max_len = 6067L, confidence = 0.35, api = getOption("dbpedia.endpoint"), verbose = TRUE){
  
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
    new = c("dbpedia_uri", "text", "start")
  )
  resources_min[, "start" := as.integer(resources_min[["start"]]) + 1L]
  setcolorder(resources_min, c("start", "text", "dbpedia_uri"))
  
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
#' @param s_attribute A length-one `character` vector indicating a s-attribute.
#'   DBpedia URIs will be mapped on this s-attribute. Only regions covered by 
#'   this s-attribute will be kept. If missing, URIs will be
#'   mapped on the token stream, and all URIs suggested will be kept.
#' @param p_attribute The p-attribute used for decoding a `subcorpus` object.
#' @param ... Further arguments.
#' @exportMethod get_dbpedia_uris
#' @importFrom cli cli_alert_warning cli_progress_step cli_alert_danger
#'   cli_progress_done cli_alert_info
#' @importFrom polmineR punctuation
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET http_error content
#' @importFrom data.table setnames `:=` setDT setcolorder
#'   as.data.table
#' @importFrom stats setNames
#' @importFrom grDevices heat.colors
#' @importFrom polmineR decode get_token_stream
#' @importFrom data.table setcolorder
#' @importFrom RcppCWB cl_cpos2struc get_region_matrix
#' @import methods
#' @docType methods
#' @rdname get_dbpedia_uris
#' @examples
#' library(polmineR)
#' use("RcppCWB")
#' 
#' uritab <- corpus("REUTERS") %>% 
#'   subset(id == "127") %>%
#'   get_dbpedia_uris(language = "en", p_attribute = "word")
#' 
#' use("GermaParl2")
#' 
#' uritab2 <- corpus("GERMAPARL2MINI") %>% 
#'   subset(speaker_name == "Carlo Schmid") %>%
#'   subset(p_type == "speech") %>% 
#'   get_dbpedia_uris(language = "de", s_attribute = "ne", max_len = 5067)
#'   
setMethod("get_dbpedia_uris", "subcorpus", function(x, language = getOption("dbpedia.lang"), p_attribute = "word", s_attribute = NULL, max_len = 6067L, confidence = 0.35, api = getOption("dbpedia.endpoint"), verbose = TRUE){
  
  if (verbose) cli_progress_step("convert input to `AnnotatedPlainTextDocument`")
  doc <- decode(
    x,
    to = "AnnotatedPlainTextDocument",
    p_attributes = p_attribute,
    mw = s_attribute,
    stoplist = c(
      dbpedia::dbpedia_stopwords[[language]],
      polmineR::punctuation
    ),
    verbose = FALSE
  )
  if (verbose) cli_progress_done()
  
  links <- get_dbpedia_uris(
    x = doc,
    language = language,
    max_len = max_len,
    confidence = confidence,
    api = api,
    verbose = verbose
  )
  
  dt <- as.data.table(doc, what = s_attribute)
  
  if (is.null(s_attribute)){
    
    links[, "end" := links[["start"]] + nchar(links[["text"]]) - 1L]
    tab <- links[,
                 list(
                   cpos_left = dt[.SD[["start"]] == .SD[["start"]]][["id"]],
                   cpos_right = dt[.SD[["end"]] == .SD[["end"]]][["id"]],
                   dbpedia_uri = .SD[["dbpedia_uri"]], text = .SD[["text"]]
                 ),
                 by = "start",
                 .SDcols = c("start", "end", "dbpedia_uri", "text")
    ]
    tab[, "start" := NULL]
  } else {
    tab <- links[dt, on = c("start", "text")]
    
    # Corpus positions in table tab may deviate from regions of 
    # s-attribute if region starts or ends with stopword (see #11)
    if (verbose)
      cli_progress_step(
        "map DBpedia Spotlight result on regions of s-attribute {.val {s_attribute}}"
      )
    strucs <- cl_cpos2struc(
      corpus = x@corpus,
      s_attribute = s_attribute,
      registry = x@registry_dir,
      cpos = tab[["cpos_left"]]
    ) 
    r <- get_region_matrix(
      corpus = x@corpus,
      s_attribute = s_attribute,
      registry = x@registry_dir,
      strucs = strucs  
    )
    tab[["cpos_left"]] <- r[,1]
    tab[["cpos_right"]] <- r[,2]
    tab[["start"]] <- NULL
    tab[["end"]] <- NULL
    tab[["id"]] <- NULL
    
    setcolorder(x = tab, neworder = c("cpos_left", "cpos_right", "dbpedia_uri", "text"))
    
    if (verbose){
      lapply(
        1L:nrow(tab),
        function(i)
          if (tab[["cpos_left"]][i] !=  r[i,1] || tab[["cpos_right"]][i] != r[i,2]){
            ne <- get_token_stream(
              r[i,1]:r[i,2],
              corpus = x@corpus,
              registry = x@registry_dir,
              p_attribute = p_attribute,
              collapse = " "
            )
            cli_alert_info("annotation mapped: {ne}")
          }
      )
    }
  }

  tab
})


#' @importFrom data.table rbindlist setorderv
#' @rdname get_dbpedia_uris
#' @examples
#' uritab <- corpus("REUTERS") %>% 
#'   split(s_attribute = "id", verbose = FALSE) %>% 
#'   get_dbpedia_uris(language = "en", p_attribute = "word", verbose = TRUE)
setMethod("get_dbpedia_uris", "subcorpus_bundle", function(x, language, p_attribute = "word", s_attribute = NULL, confidence = 0.35, api = "http://localhost:2222/rest/annotate", max_len = 6067L, verbose = TRUE){
  
  if (verbose){
    env <- parent.frame()
    cli_progress_bar("Tasks", total = length(x), type = "tasks", .envir = env)
  }
  
  li <- lapply(
    x@objects, 
    function(sc){
      if (verbose) cli_progress_update(.envir = env)
      get_dbpedia_uris(
        x = sc,
        language = language,
        s_attribute = s_attribute,
        max_len = max_len,
        confidence = confidence,
        api = api,
        verbose = FALSE
      )
    }
  )
  if (verbose) cli_progress_done(.envir = env)
  
  y <- rbindlist(li)
  setorderv(y, cols = "cpos_left", order = 1L)
  
  
  if (verbose){
    if (!is.null(s_attribute)){
      cli_alert_info(
        "coverage of DBpedia URIs: {.val {nrow(y)}} regions of s-attribute {.val {s_attribute}} / {.val {length(na.omit(y[['dbpedia_uri']]))}} URIs"
      )
    }
  }

  y
})


#' Stopwords used by DBpedia Spotlight
#' 
#' `dbpedia_stopwords` is a list of character vecotrs with stopwords used by
#' DBpedia Spotlight before processing chunks of texts. The data is used for
#' mapping offset positions returned from DBpedia Spotlight on corpus positions
#' on the R side.
#' 
#' `dbpedia_stopwords` is prepared using the script 'stopwords.R' in the
#' 'data-raw' folder of the GitHub repository. The original data is included
#' in the [model-quickstarter](https://github.com/dbpedia-spotlight/model-quickstarter)
#' repository of the DBpedia Spotlight project.
#' 
#' @examples
#' names(dbpedia_stopwords) # languages available
"dbpedia_stopwords"

