`:=` <- function(...) NULL
.SD <- NULL

#' Set and report status of DBpedia Spotlight
#' 
#' Check whether Docker container with DBpedia Spotlight is running locally 
#' and set options 'dbpedia.lang' and 'dbpedia.endpoint' accordingly.
#' @return Object of class `dbpedia_spotlight_status`, a `list` with elements
#'   "docker" (`TRUE`/`FALSE`), "lang" and "endpoint".
#' @rdname dbpedia_spotlight_status
#' @export
#' @examples
#' dbpedia_spotlight_status()
#' getOption("dbpedia.endpoint")
#' getOption("dbpedia.lang")
dbpedia_spotlight_status <- function(){
  
  status <- list(
    docker = FALSE,
    container_id = NA_character_,
    lang = NA_character_,
    api = NA_character_
  )
  class(status) <- c("dbpedia_spotlight_status", class(status))
  
  # Check whether Docker is installed
  stdout <- system2(command = "docker", stdout = FALSE, stderr = NULL)
  
  if (stdout == 0){
    stdout <- suppressWarnings(system2(
      command = "docker",
      args = c("container", "ls"),
      stdout = TRUE,
      stderr = NULL
    ))
    
    if (!is.null(attr(stdout, "status"))){ # Docker installed, but not running
      if (attr(stdout, "status") == 1L){
        status[["docker"]] <- FALSE
        status[["lang"]] <- "en"
        status[["endpoint"]] <- "http://api.dbpedia-spotlight.org/en/annotate"
      } else {
        cli_alert_warning(
          "status of running `docker container ls`: {attr(stdout, 'status')}"
        )
      }
    } else if (any(grepl("dbpedia/dbpedia-spotlight", stdout))){
      stdout_line <- grep("dbpedia/dbpedia-spotlight", stdout)
      if (length(stdout_line) > 1L){
        cli_alert_warning(paste0(c(
          "found {.val {length(stdout_line)}} dbpedia-spotlight containers ",
          "using first"
        )))
        stdout_line <- stdout_line[1L]
      }
      status[["docker"]] <- TRUE
      status[["lang"]] <- gsub(
        '^.*"spotlight.sh\\s+(\\w{2})".*$',
        "\\1",
        stdout[stdout_line]
      )
      status[["container_id"]] <- gsub(
        '^(.*?)\\s.*$',
        "\\1",
        stdout[stdout_line]
      )
      status[["endpoint"]] <- "http://localhost:2222/rest/annotate"
    } else {
      status[["docker"]] <- FALSE
      status[["lang"]] <- "en"
      status[["endpoint"]] <- "http://api.dbpedia-spotlight.org/en/annotate"
    }
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
      "*" = "docker engine: {col_cyan({if (x[['docker']]) 'running' else 'not running'})}",
      "*" = "container ID: {col_cyan({if (is.na(x[['container_id']])) 'not available' else x[['container_id']]})}",
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
    if (nrow(dt) == 0) return(dt) # if there are no elements of "what" in this text
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


#' Transform XML to NLP::AnnotatedPlainTextDocument
#' @param xml the xml document itself - needed to extract the namespace
#' @param nodes which nodes constitute a single document, i.e. either a
#'   nodeset of different segments. Can also be an entire XML document.
#' @param token_tags ...
#' @param feature_tag ...
#' @importFrom stringi stri_c
#' @importFrom NLP Annotation
#' @importFrom xml2 xml_children
to_annotation = function(nodes, xml, token_tags, feature_tag) {
  
  if (inherits(nodes, "xml_nodeset")) {
    
    lapply(
      nodes,
      to_annotation,
      xml = xml,
      token_tags = token_tags,
      feature_tag = feature_tag
    )
    
  } else {
    
    token_elements <- xml2::xml_find_all(
        nodes,
        xpath = namespaced_xpath(xml = xml, tags = token_tags)
      )
    
    # make token annotation data annotation
    
    # get tokens as character vector
    toks <- xml2::xml_text(token_elements)
    
    # get info on whitespace
    tok_joins <- xml2::xml_attr(token_elements, attr = "join")
    
    # get IDs
    tok_ids <- xml2::xml_attr(token_elements, attr = "id")
    
    # get token length: some tokens are not followed by whitespace. This is indicated
    # by `tok_joins` above.
    whitespace_int <- ifelse(is.na(tok_joins), 1, 0)
    nchars_for_toks <- nchar(toks) + whitespace_int
    
    # the starting position is needed for all but the first token. It is calculated
    # using the length of the tokens.
    
    token_char_len <- nchars_for_toks[1:(length(nchars_for_toks) - 1)]
    
    # the starting position for each token after the first is calculated using
    # cumsum().
    
    if (length(toks) > 1) {
      start_positions <- cumsum(c(1, token_char_len)) 
    } else {
      start_positions <- 1
    }
    
    # the end_position is calculcated using the cumsum of the token length minus the
    # whitespace vector (moving each character one up but for the join tokens)
    
    end_positions <- cumsum(nchars_for_toks) - whitespace_int
    
    # data.frame split to rwos
    
    token_feat_dataframe <- data.frame(word = toks, id = tok_ids)
    token_feat_list <- unname(
      split(token_feat_dataframe, seq(nrow(token_feat_dataframe)))
    )

    token_annotation <- NLP::Annotation(
      seq_along(tok_ids), # IDs must be integer, which is a bit unfortunate
      rep("word", length(tok_ids)),
      start_positions,
      end_positions,
      token_feat_list
    )
    
    # and add feature elements if chosen
    
    if (!is.null(feature_tag)) {
      feature_elements <-  xml2::xml_find_all(
        nodes,
        xpath = namespaced_xpath(xml = xml, tags = feature_tag)
      )
    } else {
      feature_elements <- NULL
    }
    
    if (length(feature_elements) > 0) {
      
      feature_ids <- sapply(
        feature_elements,
        function(element) {
          el <- xml2::xml_find_first(
            element,
            xpath = namespaced_xpath(xml = xml, tags = token_tags)
          )
          xml2::xml_attr(el, "id") 
        })
      
      feature_ids <- sprintf("%s_%s", feature_ids, feature_tag)
      
      # get attributes of features
      feature_ids <- feature_ids # name has no ID. We use the first word ID (assuming that there are no overlaps?)
      feature_kinds <- xml2::xml_attr(feature_elements, "type")
      feature_texts <- sapply(
        feature_elements,
        function(feat) paste(xml_text(xml_children(feat)), collapse = " ")
      )
      
      # get spans for features
      
      entity_spans <- t(sapply(feature_elements, function(element) {
        child_id <- xml_attr(xml_children(element), "id")
        child_idx <- which(tok_ids %in% child_id)
        child_start <- min(start_positions[child_idx])
        child_end <- max(end_positions[child_idx])
        matrix(c(child_start, child_end), nrow = 1L, ncol = 2L)
      }))
      
      
      feature_annotation <- NLP::Annotation(
        seq_along(feature_ids),
        rep("name", length(feature_ids)),
        entity_spans[, 1],
        entity_spans[, 2]
      )
      
      # combine the annotation
      segment_annotation <- c(token_annotation, feature_annotation)
      
      # add feature list to spans
      ne_annotation_components <- NLP::annotations_in_spans(segment_annotation[segment_annotation$type == "word"],
                                                            segment_annotation[segment_annotation$type == feature_tag])
      
      features <- lapply(1:length(ne_annotation_components),
                         function(i) list(
                           text = paste(sapply(ne_annotation_components[[i]]$features, "[[", "word"), collapse = " "),
                           kind = feature_kinds[[i]],
                           constituents = ne_annotation_components[[i]]$id,
                           id = feature_ids[[i]]))
      
      segment_annotation$features[segment_annotation$type == feature_tag] <- features
      
      
      
    } else {
      segment_annotation <- token_annotation
    }
    
    # make string
    word_with_ws <- paste(toks, ifelse(is.na(tok_joins), " ", ""), sep = "")
    s <- trimws(stringi::stri_c(word_with_ws, collapse = ""))
    
    # add segment id as metadata (should work if segment is NULL as the TEI has
    # an ID as well).
    
    meta <- list(
      segment_id = xml2::xml_attr(nodes, "id")
    )
    
    # finally, merge everything to annotation data
    annodata <- NLP::AnnotatedPlainTextDocument(
      s = s,
      a = segment_annotation,
      meta = meta
    )
  }
}


#' @rdname get_dbpedia_uris
setGeneric("get_dbpedia_uris", function(x, ...) standardGeneric("get_dbpedia_uris"))


#' @exportMethod get_dbpedia_uris
#' @rdname get_dbpedia_uris
#' @importFrom data.table data.table
#' @examples
#' \dontrun{
#' # Process AnnotatedPlainTextDocument (example available in NLP package)
#' doc <- readRDS(system.file("texts", "stanford.rds", package = "NLP"))
#' tab <- get_dbpedia_uris(x = doc, language = "en")
#' 
#' tab <- get_dbpedia_uris(
#'   x = doc,
#'   language = "en",
#'   api = "http://api.dbpedia-spotlight.org/en/annotate"
#' )
#' }
#' 
#' # Use argument types to limit result to certain types
#' 
#' library(polmineR)
#' use("RcppCWB") # make REUTERS corpus available
#' 
#' reuters_article <- corpus("REUTERS") %>%
#'   subset(id == "127") %>%
#'   get_token_stream(p_attribute = "word", collapse = " ")
#' 
#' uris <- get_dbpedia_uris(
#'   reuters_article,
#'   language = "en",
#'   types = "Company",
#'   api = "http://api.dbpedia-spotlight.org/en/annotate"
#' )
setMethod("get_dbpedia_uris", "character", function(x, language = getOption("dbpedia.lang"), max_len = 5600L, confidence = 0.35, api = getOption("dbpedia.endpoint"), types = character(), support = 20, verbose = TRUE){
  
  if (nchar(x) > max_len){
    if (verbose) cli_alert_warning(
      "input text has length {nchar(x)}, truncate to max_len ({.val {max_len}})"
    )
    x <- substr(x, 1L, max_len)
  }
  
  if (!is.numeric(support) | !(length(support) == 1)){
    cli_alert_warning("argument `support` required to be a numeric value")
  }
  
  if (verbose) cli_progress_step("send request to DBpedia Spotlight")
  request <- httr::GET(
    url = api,
    query = c(
      list(
        text = x,
        support = as.character(support),
        confidence = confidence
      ),
      if (length(types) == 0L)
        list()
      else
        list(types = paste(types, collapse = ","))
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
  
  if (nrow(resources) == 0L){
    return(
      data.table(
        start = integer(),
        text = character(),
        dbpedia_uri = character(),
        types = character()
      )
    )
  }
  
  resources_min <- resources[, c("@URI", "@surfaceForm", "@offset", "@types")]
  setnames(
    resources_min,
    old = c("@URI", "@surfaceForm", "@offset", "@types"),
    new = c("dbpedia_uri", "text", "start", "types")
  )
  setcolorder(resources_min, c("start", "text", "dbpedia_uri", "types"))
  
  resources_min[, "start" := as.integer(resources_min[["start"]]) + 1L]
  resources_min[, "types" := lapply(
    strsplit(x = resources_min[["types"]], split = ","),
    function(x){
      if (length(x) == 0L) return(list())
      spl <- strsplit(x, split = ":")
      split(
        x = unlist(lapply(spl, `[`, 2L)),
        f = unlist(lapply(spl, `[`, 1L))
      )
    }
  )]

  resources_min
})


#' @exportMethod get_dbpedia_uris
#' @rdname get_dbpedia_uris
setMethod("get_dbpedia_uris", "AnnotatedPlainTextDocument", function(x, language = getOption("dbpedia.lang"), max_len = 5600L, confidence = 0.35, api = getOption("dbpedia.endpoint"), types = character(), support = 20, verbose = TRUE){
  get_dbpedia_uris(
    x = as.character(x[["content"]]),
    language = language,
    max_len = max_len,
    confidence = confidence,
    api = api,
    types = types,
    support = support,
    verbose = verbose
  )
})


#' Get DBpedia links.
#' 
#' #' @details
#' `expand_to_token` is a rather experimental feature that resolves mismatches
#' between entity spans and token spans by expanding the former to the last
#' character position of the corresponding token. See issue #26 in the `dbpedia`
#' GitHub repository.
#' The configuration of the `httr::GET()` calls used can be controlled using
#' `httr::config()`. A relevant scenario is SSL verification issues that can be
#' addressed using `httr::set_config(httr::config(ssl_verifypeer = 0L))` (at own
#' risk!). The error "HTTP/2 stream 1 was not closed cleanly before end of the
#' underlying stream" can be addressed using
#' `httr::set_config(httr::config(http_verson = 1.1))`
#'
#' @param x A `subcorpus` (`xml`, ...) object. Will be coerced to
#'   'AnnotatedPlainTextDocument' from NLP package.
#' @param max_len An `integer` value. The text passed to DBpedia Spotlight may
#'   not exceed a defined length. If it does, an HTTP error results. The known
#'   threshold of 5600 characters is the default value.
#' @param language The language of the input text ("en", "fr", "de", ...) to
#'   determine the stopwords used.
#' @param confidence A `numeric` value, the minimum similarity score that serves
#'   as threshold before DBpedia Spotlight includes a link into the report.
#' @param api An URL of the DBpedia Spotlight API.
#' @param types A `character` vector to restrict result returned to certain
#'   entity types, such as 'Company' or 'Organization'. If the `character` 
#'   vector is empty (default), no restrictions are applied.
#' @param support The number of indegrees at Wikidata. Useful for limiting the 
#'   the number of results by excluding insignificant entities.
#' @param verbose A `logical` value - whether to display messages.
#' @param progress A `logical` value - whether to show progress.
#' @param s_attribute A length-one `character` vector indicating a s-attribute.
#'   DBpedia URIs will be mapped on this s-attribute. Only regions covered by
#'   this s-attribute will be kept. If missing, URIs will be mapped on the token
#'   stream, and all URIs suggested will be kept.
#' @param p_attribute The p-attribute used for decoding a `subcorpus` object.
#' @param expand_to_token A `logical` value - whether the character offsets of
#'   DBpedia Spotlight are mapped to the token boundaries of the `subcorpus` if
#'   the two differ. Also see the explanation in `details`. Defaults to FALSE.
#' @param drop_inexact_annotations A `logical` value - whether to drop entity
#'   annotations when entity spans and token spans do not align exactly.
#' @param ... Further arguments.
#' @return A `data.table` with the following columns:
#' - *dbpedia_uri*: The DBpedia URI.
#' - *text*: Text that has been annotated
#' - *types*: Recognized entity types, for each row a named list, if available 
#'   entries such as 'DBpedia', 'Schema', 'Wikidata', 'DUL'
#' Depending on the input object, further columns may be available.
#' @exportMethod get_dbpedia_uris
#' @importFrom cli cli_alert_warning cli_progress_step cli_alert_danger
#'   cli_progress_done cli_alert_info
#' @importFrom polmineR punctuation
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET http_error content
#' @importFrom data.table setnames `:=` setDT setcolorder as.data.table
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
#'   get_dbpedia_uris(
#'     api = "http://api.dbpedia-spotlight.org/en/annotate",
#'     language = "en",
#'     p_attribute = "word"
#'   )
#'
#' use("GermaParl2")
#'
#' uritab2 <- corpus("GERMAPARL2MINI") %>%
#'   subset(speaker_name == "Carlo Schmid") %>%
#'   subset(p_type == "speech") %>%
#'   get_dbpedia_uris(language = "de", s_attribute = "ne", max_len = 5067)
#'   
setMethod("get_dbpedia_uris", "subcorpus", function(x, language = getOption("dbpedia.lang"), p_attribute = "word", s_attribute = NULL, max_len = 5600L, confidence = 0.35, api = getOption("dbpedia.endpoint"), types = character(), support = 20, expand_to_token = FALSE, drop_inexact_annotations = TRUE, verbose = TRUE){
  
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
    types = types,
    support = support,
    verbose = verbose
  )

  # prepare function to assign cpos_right depending on value and arguments
  expand_fun = function(.SD) {
    cpos_right <- dt[.SD[["end"]] == dt[["end"]]][["id"]]
    if (length(cpos_right) == 0 & isTRUE(expand_to_token)) {
      cpos_right <- dt[["id"]][which(dt[["end"]] > .SD[["end"]])[1]]
    } else {
      cpos_right
    }
  }

  if (is.null(s_attribute)){
    dt <- as.data.table(doc, what = s_attribute)
    links[, "end" := links[["start"]] + nchar(links[["text"]]) - 1L]
    tab <- links[,
                 list(
                   cpos_left = dt[.SD[["start"]] == dt[["start"]]][["id"]],
                   cpos_right = expand_fun(.SD),
                   dbpedia_uri = .SD[["dbpedia_uri"]],
                   text = .SD[["text"]],
                   types = .SD[["types"]]
                 ),
                 by = "start",
                 .SDcols = c("start", "end", "dbpedia_uri", "text", "types")
    ]
    tab[, "start" := NULL]
  } else {
    
    dt <- as.data.table(doc, what = s_attribute)
    if (nrow(dt) == 0){ # if there are no elements of s_attribute #23
      return(
        data.table(
          start = integer(),
          text = character(),
          dbpedia_uri = character(),
          types = character()
        )
      )
    } 

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
    
    setcolorder(x = tab, neworder = c("cpos_left", "cpos_right", "dbpedia_uri", "text", "types"))
    
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

  # drop entities which cannot be mapped exactly to the tokenstream from the
  # output (see issue #26).
  if (isTRUE(drop_inexact_annotations) & any(is.na(tab[["cpos_right"]]))) {
    missing_cpos_idx <- which(is.na(tab[["cpos_right"]]))
    cli_alert_warning("Cannot map {length(missing_cpos_idx)} entit{?y/ies} exactly to tokenstream. Dropping {?it/them} from the annotation.")
    tab <- tab[-missing_cpos_idx, ]
  }

  tab
})


#' @importFrom data.table rbindlist setorderv
#' @rdname get_dbpedia_uris
#' @examples
#' uritab <- corpus("REUTERS") %>% 
#'   split(s_attribute = "id", verbose = FALSE) %>% 
#'   get_dbpedia_uris(
#'     api = "http://api.dbpedia-spotlight.org/en/annotate",
#'     language = "en",
#'     p_attribute = "word",
#'     verbose = TRUE
#'   )
setMethod("get_dbpedia_uris", "subcorpus_bundle", function(x, language = getOption("dbpedia.lang"), p_attribute = "word", s_attribute = NULL, confidence = 0.35, api = getOption("dbpedia.endpoint"), types = character(), support = 20, max_len = 5600L, expand_to_token = FALSE, verbose = TRUE, progress = FALSE){
  
  if (progress){
    env <- parent.frame()
    cli_progress_bar("Tasks", total = length(x), type = "tasks", .envir = env)
  }
  
  li <- lapply(
    x@objects, 
    function(sc){
      if (progress) cli_progress_update(.envir = env)
      get_dbpedia_uris(
        x = sc,
        language = language,
        s_attribute = s_attribute,
        max_len = max_len,
        confidence = confidence,
        api = api,
        types = types,
        support = support,
        expand_to_token = expand_to_token,
        verbose = if (progress) FALSE else verbose
      )
    }
  )
  if (progress) cli_progress_done(.envir = env)
  
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


#' @examples
#' # example code
#' 
#' # Process quanteda corpus 
#' library(quanteda)
#' httr::set_config(httr::config(ssl_verifypeer = 0L, http_version = 1.1))
#' 
#' uritab <- data_char_ukimmig2010 %>%
#'   corpus() %>%
#'   get_dbpedia_uris(verbose = FALSE)
#' @rdname get_dbpedia_uris
setMethod(
  "get_dbpedia_uris",
  "corpus",
  function(
    x,
    language = getOption("dbpedia.lang"),
    max_len = 5600L,
    confidence = 0.35,
    api = getOption("dbpedia.endpoint"),
    types = character(),
    support = 20,
    verbose = TRUE,
    progress = FALSE
  ){
    
    # ensure that input object is corpus class from quanteda pkg
    if (isS4(x)){
      cli::cli_alert_danger(
        "input of {.fn get_dbpedia_uris} is S4 class 'corpus' 
        from package {.pkg {getClass('corpus')@package}} -
        'corpus' class from package {.pkg quanteda} required" ,
        wrap = TRUE
      )
    }

    docs <- as.character(x)
    
    if (progress){
      env <- parent.frame()
      cli_progress_bar("Tasks", total = length(x), type = "tasks", .envir = env)
    }
    
    retval <- rbindlist(
      lapply(
        names(docs),
        function(docname){
          if (progress) cli_progress_update(.envir = env)
          dt <- get_dbpedia_uris(
            x = docs[[docname]],
            language = language,
            max_len = max_len,
            confidence = confidence,
            api = api,
            types = types,
            support = support,
            verbose = if (progress) FALSE else verbose
          )[, "doc" := docname]
        }
      )
    )
    
    if (progress) cli_progress_done(.envir = env)
    
    setcolorder(retval, neworder = "doc")

    retval
  }
)


#' @param feature_tag Name of a tag containing named entities, etc.
#' @param segment name of elements to segment document by
#' @param token_tags names of elements describing tokens
#' @param text_tag name of element that distinguishes text from other elements
#'   such as headers
#' @rdname get_dbpedia_uris
#' @exportMethod get_dbpedia_uris
setMethod(
  "get_dbpedia_uris",
  "xml_document",
  function(
    x,
    language = getOption("dbpedia.lang"),
    feature_tag = NULL,
    segment = NULL,
    token_tags = c("w", "pc"),
    text_tag = NULL,
    max_len = 5600L,
    confidence = 0.35,
    api = getOption("dbpedia.endpoint"),
    types = character(),
    support = 20,
    expand_to_token = FALSE,
    drop_inexact_annotations = TRUE,
    verbose = TRUE
  ){
    
  # sometimes, there are nodes of the same name in different parts of the
  # document (such as <name>) in ParlaMint which describes persons in the TEI
  # header and named entities in the text body. It can be useful to focus on the
  # text part.
  
  if (!is.null(text_tag)) {
    nodes <- xml2::xml_find_all(x, xpath = namespaced_xpath(xml = x, tags = text_tag))
  } else {
    nodes <- x 
    
    # Note: these two nodes objects are different since the first is a nodeset,
    # the second is a xml_document.
  }
  
  # get units which should be send to the DBpedia Spotlight (to account for
  # max_len, etc.). This can be the entire text or a paragraph or a sentence,
  # depending on the structure. Provided by "segment" argument.
  
  # get both tokens and features (NEs, etc.)
  
  if (is.null(segment)) {
    nodes_to_process <- nodes
  } else {
    nodes_to_process <- xml2::xml_find_all(nodes,
                                           xpath = namespaced_xpath(xml = x, tags = segment))
  }
  
  if (verbose) cli_progress_step("preparing {.val {length(nodes_to_process)}} annotation tables.")
  
  docs <- to_annotation(nodes = nodes_to_process,
                        xml = x,
                        token_tags = token_tags,
                        feature_tag = feature_tag)
  
  if (verbose) cli_progress_done()
  
  # prepare function to assign ID depending on value and arguments
  expand_fun = function(.SD, dt) {
    id_right <- dt[.SD[["end"]] == dt[["end"]]][["id"]]
    
    if (length(id_right) == 0 & isTRUE(expand_to_token)) {
      id_right <- dt[["id"]][which(dt[["end"]] > .SD[["end"]])[1]]
    } else {
      id_right
    }
  }
  
  # Note: The following function should probably overload the existing
  # dbpedia:::as.data.table.AnnotatedPlainTextDocument() function.
  
  AnnotatedPlainTextDocument_to_datatable2 = function (x, what = NULL)  {
    dt <- setDT(as.data.frame(x[["annotation"]]))
    if (!is.null(what)) {
      dt <- dt[dt[["type"]] %in% what]
      if (nrow(dt) == 0)  return(dt)
      dt[, `:=`("text", unlist(lapply(dt[["features"]], `[[`, "text")))]
      constituents <- lapply(dt[["features"]], `[[`, "constituents")
      dt[, `:=`("feature_kind", unlist(lapply(dt[["features"]],  `[[`, "kind")))]
      dt[, `:=`("id_left", sapply(constituents, min))]
      dt[, `:=`("id_right", sapply(constituents, max))]
      dt[, `:=`("original_id", unlist(lapply(dt[["features"]],  `[[`, "id")))]
      dt[, `:=`("features", NULL)]
    } else {
      # always retrieve original ID from features
      dt[, `:=`("original_id", unlist(lapply(dt[["features"]],  `[[`, "id")))]
      dt <- dt[, `:=`("features", NULL)]
    }
    dt
  }
  
  annotations <- lapply(docs, function(doc) {
    
    links <- get_dbpedia_uris(
      x = doc,
      language = language,
      max_len = max_len,
      confidence = confidence,
      api = api,
      types = types,
      support = support,
      verbose = verbose
    )
    
    if (nrow(links) == 0) return(NULL) # no entities in this segment
    
    if (is.null(feature_tag)) {
      dt <- AnnotatedPlainTextDocument_to_datatable2(doc, what = feature_tag)
      links[, "end" := links[["start"]] + nchar(links[["text"]]) - 1L]
      tab <- links[,
                   list(
                     original_id = paste(dt[which(.SD[["start"]] == dt[["start"]]):which(.SD[["end"]] == dt[["end"]])][["original_id"]], collapse = "|"),
                     dbpedia_uri = .SD[["dbpedia_uri"]],
                     text = .SD[["text"]],
                     types = .SD[["types"]]
                   ),
                   by = "start",
                   .SDcols = c("start", "end", "dbpedia_uri", "text", "types")
      ]
      tab[, "start" := NULL]
      
    } else {
      
      dt <- AnnotatedPlainTextDocument_to_datatable2(doc, what = feature_tag)
      if (nrow(dt) == 0) return(NULL) # if there are no elements of s_attribute
      
      tab <- links[dt, on = c("start", "text")]
      
      # does #11 apply here, too? For CWB, this can be an issue?
      
      # actually, the original ID can be used to add?
      
      tab[["start"]] <- NULL
      tab[["end"]] <- NULL
      tab[["i.end"]] <- NULL
      tab[["id"]] <- NULL
      tab[["type"]] <- NULL
      tab[["feature_kind"]] <- NULL
      
      tab[["id_left"]] <- NULL
      tab[["id_right"]] <- NULL
      
      setcolorder(x = tab, neworder = c("dbpedia_uri", "text", "types", "original_id"))
      
    }
    
    # add segment id from document's metadata
    tab$segment_id <- doc$meta[["segment_id"]]
    
    if (isTRUE(drop_inexact_annotations) & any(is.na(tab[["original_id"]]))) {
      missing_id_idx <- which(is.na(tab[["original_id"]]))
      cli_alert_warning("Cannot map {length(missing_id_idx)} entit{?y/ies} exactly to tokenstream. Dropping {?it/them} from the annotation.")
      tab <- tab[-missing_id_idx, ]
    }
    
    tab
    # MAYBE SLEEP?
    
  }
  )
  
  data.table::rbindlist(annotations)
})



#' Stopwords used by DBpedia Spotlight
#' 
#' `dbpedia_stopwords` is a list of character vectors with stopwords used by
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

