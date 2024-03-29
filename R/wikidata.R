#' Get Wikipedia IDs for DBpedia IDS.
#' 
#' @param x A character vector with DBpedia URIs. NA values are dropped, only
#'   unique values will be processed.
#' @param optional Optional information to retrieve (passed as length-one
#'   character vector, e.g. 'municipalityCode').
#' @param endpoint Endpoint to query (a `character` vector).
#' @param chunksize Single numeric value with maximum size of chunks to process at
#'   a time.
#' @param limit Single numeric value, the maximum number of results to retrieve
#'   from Wikidata.
#' @param wait A numeric value passed into `Sys.sleep()` to slow down sequence
#'   of requests (and avoid denial of service). Defaults to 100.
#' @param progress Whether to show progress bar (`logical` value).
#' @param verbose Whether to show messages (`logical` value).
#' @export
#' @rdname wikidata_uris
#' @examples
#' dbpedia_uris <- c(
#'   "http://de.dbpedia.org/resource/Killesberg",
#'   "http://de.dbpedia.org/resource/Ljubljana",
#'   "http://de.dbpedia.org/resource/Velbert"
#' )
#' dbpedia_get_wikidata_uris(
#'   dbpedia_uris,
#'   optional = "municipalityCode",
#'   endpoint = "http://de.dbpedia.org/sparql",
#'   wait = 0,
#'   chunksize = 2,
#'   progress = TRUE
#' )
#' @importFrom cli cli_progress_bar cli_progress_done cli_progress_update
dbpedia_get_wikidata_uris <- function(x, optional, endpoint, chunksize = 100, limit = chunksize, wait = 1, verbose = TRUE, progress = FALSE){
  
  stopifnot(
    is.character(x),
    is.numeric(chunksize), length(chunksize) == 1L,
    is.numeric(limit), length(limit) == 1L,
    is.logical(progress), length(progress) == 1L,
    is.numeric(wait), length(wait) == 1L, wait >= 0
  )
  
  if (!missing(optional)){
    stopifnot(is.character(optional), length(optional) == 1L)
    optional <- sprintf('OPTIONAL { ?item dbo:%s ?key . }', optional)
  } else {
    optional <- ""
  }
  
  x <- na_drop(x, verbose = verbose)
  
  if (verbose) cli_alert_info("input length: {.val {length(x)}}")
  x <- unique_msg(x, verbose = verbose)
  
  template <- 'SELECT distinct ?item ?wikidata_uri
      WHERE {
      VALUES ?item {%s}
      ?item owl:sameAs ?wikidata_uri
      %s
      FILTER(regex(str(?wikidata_uri), "www.wikidata.org" ) )}
      LIMIT %d'
  

  chunks <- as_chunks(x = x, size = chunksize)
  retval_li <- list()
  
  if (progress){
    cli_progress_bar(
      "Processing chunks of DBpedia URIs to get Wikidata URIs",
      total = length(chunks),
      type = "tasks"
    )
  }
  for (i in 1L:length(chunks)){
    if (progress) cli_progress_update()
    query <- sprintf(
      template,
      paste(sprintf("<%s>", chunks[[i]]), collapse = " "),
      optional,
      limit
    )
    
    Sys.sleep(wait)
    
    retval_li[[i]] <- sparql_query(endpoint = endpoint, query = query)
  }
  
  if (progress) cli_progress_done()

  y <- as_tibble(do.call(rbind, retval_li))
  colnames(y)[1] <- "dbpedia_uri"
  y[["dbpedia_uri"]] <- gsub("^<(.*?)>$", "\\1", y[["dbpedia_uri"]])
  y[["wikidata_uri"]] <- gsub("^<(.*?)>$", "\\1", y[["wikidata_uri"]])
  y[["wikidata_id"]] <- gsub("^.*/(Q\\d+)$", "\\1", y[["wikidata_uri"]])
  y[["key"]] <- NULL
  y
}


#' Query Wikidata endpoint for additional information.
#' 
#' This is a wrapper for `WikidataQueryServiceR::query_wikidata()` to get
#' additional information for known wikidata IDs.
#' 
#' @return A `tibble`.
#' @param x A vector of wikidata ids.  NA values are dropped, only unique values
#'   will be processed.
#' @param id Wikidata ID for information to retrieve (`character` vector).
#' @param chunksize Single numeric value with maximum size of chunks to process at
#'   a time.
#' @param verbose Whether to output messages (`logical` value).
#' @param progress Whether to show progress information (`logical` value).
#' @param wait A numeric value - slow down requests to avoid denial of service.
#' @param ... Further arguments.
#' @export
#' @rdname wikidata_query
setGeneric("wikidata_query", function(x, ...) standardGeneric("wikidata_query"))

#' @param language Language to be used in Wikidata query.
#' @rdname wikidata_query
#' @examples
#' wikidata_ids <- c("Q1741365", "Q3840", "Q437")
#' wikidata_query(wikidata_ids, id = "P439", progress = TRUE)
#' 
#' wikidata_uris <- c(
#'   "http://www.wikidata.org/entity/Q1020", 
#'   "http://www.wikidata.org/entity/Q27468",
#'   "http://www.wikidata.org/entity/Q2131751"
#' )
#' wikidata_query(wikidata_uris, id = "P439", progress = TRUE)
#' @importFrom stats na.omit
setMethod("wikidata_query", "character", function(x, id, language = getOption("dbpedia.lang"), chunksize = 100L, wait = 1, verbose = TRUE, progress = FALSE){
  
  if (!requireNamespace("WikidataQueryServiceR", quietly = TRUE)){
    stop("R package WikidataQueryServiceR required but not available. ")
  }
  
  stopifnot(
    is.character(id), length(id) == 1L,
    is.numeric(chunksize), chunksize > 0,
    is.numeric(wait), wait > 0, length(wait) == 1L,
    is.logical(progress), length(progress) == 1L
  )
  
  if (any(startsWith(na.omit(x), "http"))){
    if (verbose) cli_alert_info("extract wikidata ID from URI")
    x <- gsub("^http(s|)://www.wikidata.org/entity/", "", x)
  }
  
  if (!all(grepl("^Q\\d+$", x))){
    cli_alert_warning(
      "all wikidata IDs expected to match regex pattern '^Q\\d+$' - not TRUE"
    )
  }
  
  x <- na_drop(x, verbose = verbose)
  
  if (verbose) cli_alert_info("length of input vector: {.val {length(x)}}")
  x <- unique_msg(x, verbose = verbose)

  template <- 'SELECT ?item ?label ?key ?keyLabel
        WHERE {
        VALUES ?item { %s }
        OPTIONAL { ?item wdt:%s ?key . }
        ?item rdfs:label ?label
          filter(lang(?label) = "%s")
        SERVICE wikibase:label { bd:serviceParam wikibase:language "%s". }
      }'
  
  chunks <- as_chunks(x = x, size = chunksize)
  retval_li <- list()
  
  if (progress){
    cli_progress_bar(
      "Processing chunks of Wikidata URIs",
      total = length(chunks),
      type = "tasks"
    )
  }
  for (i in 1L:length(chunks)){
    if (progress) cli_progress_update()
    query <- sprintf(
      template,
      paste0("wd:", chunks[[i]], collapse = " "),
      id,
      language,
      language
    )
    
    if (i > 1L) Sys.sleep(wait)
    
    option_setting <- getOption("readr.show_col_types")
    options("readr.show_col_types" = FALSE)
    retval_li[[i]] <- WikidataQueryServiceR::query_wikidata(
      sparql_query = query,
      format = "simple"
    )
    options("readr.show_col_types" = option_setting)
    
    colnames(retval_li[[i]])[1] <- "wikidata_uri"
  }
  
  if (progress) cli_progress_done()
  
  retval <- as_tibble(do.call(rbind, retval_li))
  colnames(retval)[which(colnames(retval) == "label")] <- "wikidata_label"
  colnames(retval)[which(colnames(retval) == "key")] <- paste(id, "key", sep = "_")
  colnames(retval)[which(colnames(retval) == "keyLabel")] <- paste(id, "keyLabel", sep = "_")
  retval
})

#' @rdname wikidata_query
setMethod(
  "wikidata_query",
  "data.table",
  function(x, id, language = getOption("dbpedia.lang"), chunksize = 100L, wait = 1, verbose = TRUE, progress = FALSE){
    if (!"wikidata_id" %in% colnames(x)){
      cli_alert_danger("{.fn wikidata_query} requires column {.val wikidata_id}")
      stop()
    }
    
    tbl <- wikidata_query(
      x = x[["wikidata_id"]],
      id = id,
      chunksize = chunksize,
      wait = wait,
      verbose = verbose,
      progress = progress
    )
    dt <- as.data.table(tbl, key = "wikidata_uri")
    dt[, (paste(id, "key", sep = "_")) := NULL]
    dt_min <- dt[,{
      list(id = paste(.SD[[paste(id, "keyLabel", sep = "_")]], collapse = "|"))
      }, by = c("wikidata_uri", "wikidata_label")
    ]
    setnames(dt_min, old = "id", new = id)
    setkeyv(x = dt_min, cols = "wikidata_uri")
    setkeyv(x = x, cols = "wikidata_uri")
    y <- dt_min[x]
    setcolorder(x = y, neworder = c(colnames(x), colnames(dt_min)[-which(colnames(dt_min) == "wikidata_uri")]))
    y
  }
)



#' @param ... Further arguments.
#' @rdname wikidata_uris
#' @exportMethod add_wikidata_uris
setGeneric(
  "add_wikidata_uris",
  function(x, ...) standardGeneric("add_wikidata_uris")
)


#' @examples
#' \donttest{
#' library(dbpedia)
#' library(quanteda)
#' 
#' options(dbpedia.lang = "en")
#' options(dbpedia.endpoint = "http://api.dbpedia-spotlight.org/en/annotate")
#' 
#' httr::set_config(httr::config(ssl_verifypeer = 0L, http_version = 1.1))
#'
#' uritab <- data_char_ukimmig2010 %>%
#'   corpus() %>%
#'   get_dbpedia_uris(
#'     progress = TRUE
#'   ) %>% 
#'   add_wikidata_uris(
#'     endpoint = "https://dbpedia.org/sparql/",
#'     progress = TRUE,
#'     chunksize = 100
#'   ) %>% 
#'   wikidata_query(id = "P31")
#' }
#'   
#' @rdname wikidata_uris
#' @importFrom data.table setkeyv
setMethod(
  "add_wikidata_uris",
  "data.table",
  function(
    x,
    optional,
    endpoint,
    chunksize = 100,
    limit = chunksize,
    wait = 1,
    verbose = TRUE,
    progress = FALSE
  ){
    if (!"dbpedia_uri" %in% colnames(x)){
      cli_alert_danger("{.fn add_dbpedia_uris} requires column {.val dbpedia_uri}")
      stop()
    }
    
    wikidata_tbl <- dbpedia_get_wikidata_uris(
      x = x[["dbpedia_uri"]],
      optional = optional,
      endpoint = endpoint,
      wait = wait,
      chunksize = chunksize,
      limit = limit,
      progress = progress
    )
    wikidata_dt <- as.data.table(wikidata_tbl, key = "dbpedia_uri")
    setkeyv(x = x, cols = "dbpedia_uri")
    y <- wikidata_dt[x]
    setcolorder(x = y, neworder = c(colnames(x), "wikidata_uri", "wikidata_id"))
    y
  }
)
