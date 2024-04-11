test_that(
  "categorize_overlap() categorizes entity overlaps based on character vectors",
  {
    
    doc <- "Crude oil prices on the rise."
    
    x <- get_dbpedia_uris(
      x = doc,
      max_len = 5600L,
      confidence = 0.35,
      api = "https://api.dbpedia-spotlight.org/en/annotate",
      language = "en",
      types = character(),
      support = 20,
      types_src = c("DBpedia", "Wikidata"),
      verbose = TRUE
    ) |>
      detect_overlap(start_col = "start", verbose = TRUE)
    
    y <- categorize_overlap(x,
                            start_col = "start",
                            end_col = "end",
                            experimental = TRUE,
                            verbose = TRUE)
    
    # expect four rows
    expect_equal(nrow(y), 4L)
    
    # expect specific entity texts
    expect_contains(y[["text"]], c("Crude oil", "Crude oil prices", "oil", "oil prices"))
    
    # expect specific values in specific row
    example_row <- y[text == "Crude oil prices"]
    
    # concatenated uris
    expect_equal(example_row[["dbpedia_uri"]],
                 "http://dbpedia.org/resource/Petroleum|http://dbpedia.org/resource/Price_of_oil")
    
    expect_equal(
      unname(
        unlist(
          example_row[, ovl_longest:ovl_undetermined]
        )
      ),
      c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE)
    )
  }
)

test_that(
  "categorize_overlap() categorizes entity overlaps based on CWB corpora",
  {
    
    withr::local_package("polmineR")
    
    use("RcppCWB")
    
    reuters_anno <- corpus("REUTERS") |>
      polmineR::subset(id == "353") |>
      get_dbpedia_uris(
        max_len = 5600L,
        confidence = 0.35,
        api = "https://api.dbpedia-spotlight.org/en/annotate",
        language = "en",
        types = character(),
        support = 20,
        verbose = TRUE
      ) |>
      detect_overlap(start_col = "cpos_left", end_col = "cpos_right", verbose = TRUE)
    
    y <- categorize_overlap(reuters_anno,
                            start_col = "cpos_left",
                            end_col = "cpos_right",
                            experimental = TRUE,
                            corpus = "REUTERS",
                            verbose = TRUE)
    
    ymin <- y[!is.na(ovl_id)]
    
    # expect four rows
    expect_equal(nrow(ymin), 4L)
    
    # expect specific entity texts
    expect_contains(ymin[["text"]], c("Crude oil", "Crude oil prices fell", "oil prices fell", "oil"))
    
    # expect specific values in specific row
    example_row <- ymin[text == "Crude oil prices fell"]
    
    # concatenated uris
    expect_equal(example_row[["dbpedia_uri"]],
                 "http://dbpedia.org/resource/West_Texas_Intermediate|http://dbpedia.org/resource/1980s_oil_glut")
    
    expect_equal(
      unname(
        unlist(
          example_row[, ovl_longest:ovl_undetermined]
        )
      ),
      c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE)
    )
  }
)