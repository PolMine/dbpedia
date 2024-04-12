test_that(
  "get_dbpedia_uris() for quanteda corpus",
  {

    withr::local_package("quanteda")
    quanteda_corpus_min <- quanteda::corpus(quanteda::data_char_ukimmig2010[1:3])

    uritab <- get_dbpedia_uris(
      x = quanteda_corpus_min,
      api = "http://api.dbpedia-spotlight.org/en/annotate",
      language = "en",
      progress = FALSE,
      verbose = FALSE,
      retry = 0L
    )
  }
)
