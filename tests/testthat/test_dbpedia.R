testthat::context("dbpedia")

test_that(
  "get_dbpedia_uris() for quanteda corpus",
  {
    library(quanteda)
    
    uritab <- quanteda::data_char_ukimmig2010[1:3] %>%
      corpus() %>%
      get_dbpedia_uris(
        api = "http://api.dbpedia-spotlight.org/en/annotate",
        language = "en",
        progress = FALSE,
        verbose = FALSE
      )
  }
)
