test_that(
  "argument expand_to_tokens results in additional matches",
  {
    withr::local_package("polmineR")
    use("RcppCWB")

    reuters_newswire <- corpus("REUTERS") |>
      subset(id == 194)

    anno_dt_false <- reuters_newswire |>
      get_dbpedia_uris(expand_to_token = FALSE,
                       api = "http://api.dbpedia-spotlight.org/en/annotate",
                       language = "en",
                       confidence = 0.35,
                       support = 20,
                       verbose = FALSE)

    anno_dt_true <- reuters_newswire |>
      get_dbpedia_uris(expand_to_token = TRUE,
                       api = "http://api.dbpedia-spotlight.org/en/annotate",
                       language = "en",
                       confidence = 0.35,
                       support = 20,
                       verbose = FALSE)

    testthat::expect_equal(nrow(anno_dt_false), 11L)
    testthat::expect_equal(nrow(anno_dt_true), 12L)

  }
)