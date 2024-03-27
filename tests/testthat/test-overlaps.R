test_that(
  "detect overlap detects overlap for characters",
  {

    doc <- "Vice President Gore in Washington D.C."

    withr::local_package("data.table") # for copy
    
    x <- get_dbpedia_uris(
      x = doc,
      max_len = 5600L,
      confidence = 0.35,
      api = "http://api.dbpedia-spotlight.org/en/annotate",
      language = "en",
      types = character(),
      support = 20,
      types_src = c("DBpedia", "Wikidata"),
      verbose = TRUE
    )

    y <- copy(x)

    # modify x by reference
    detect_overlap(x = x, start_col = "start", verbose = TRUE)

    # Test 1: This should add the columns "end" and "ovl_id" to x
    additional_cols <- setdiff(colnames(x), colnames(y))
    expect_equal(additional_cols, c("end", "ovl_id"))

    # Test 2: This should also keep all rows
    expect_equal(nrow(x), nrow(y))

    # Test 3: This should return one unique non-na overlap ID
    length_non_na_unique <- length(unique(x[["ovl_id"]][!is.na(x[["ovl_id"]])]))
    expect_equal(length_non_na_unique, 1L)

    # Test 4: This should return two rows with non-na overlap IDs
    length_non_na <- length(x[["ovl_id"]][!is.na(x[["ovl_id"]])])
    expect_equal(length_non_na, 2L)
    
    # Test 5: Running detect_overlap() again like above should result in an
    # error.
    expect_error(
      detect_overlap(x = x, start_col = "start", verbose = TRUE),
      'Cannot add new column'
    )
  }
)

test_that(
  "detect overlap detects overlap for CWB subcorpora",
  {

    withr::local_package("polmineR")
    use("RcppCWB")

    reuters_anno <- corpus("REUTERS") |>
      subset(id == "353") |>
      get_dbpedia_uris(
        max_len = 5600L,
        confidence = 0.35,
        api = "http://api.dbpedia-spotlight.org/en/annotate",
        language = "en",
        types = character(),
        support = 20,
        verbose = TRUE
      ) |>
      detect_overlap(start_col = "cpos_left", end_col = "cpos_right", verbose = TRUE)
    
    # there should be two rows with overlapping entity spans in this example
    expect_equal(nrow(reuters_anno[!is.na(ovl_id)]), 2L)

    # and there should be only one unique ID
    expect_equal(
      unique(reuters_anno[!is.na(ovl_id)][["ovl_id"]]), "ovl_1"
    )
  }
)

test_that(
  "detect overlap detects overlap for quanteda corpora (with multiple documents)",
  {

    withr::local_package("quanteda")

    x <- data_corpus_inaugural |>
      corpus_subset(President == "Bush" & Year > 2000) |>
      corpus_reshape(to = "paragraphs") |>
      _[2:3] |>
      get_dbpedia_uris(
        max_len = 5000L,
        confidence = 0.35,
        api = "http://api.dbpedia-spotlight.org/en/annotate",
        language = "en",
        types = character(),
        support = 20,
        verbose = FALSE,
        progress = TRUE
      )

    # modify x by reference
    y <- detect_overlap(x, start_col = "start", verbose = TRUE)

    # there should be two rows with overlapping entity spans in this example
    expect_equal(nrow(x[!is.na(ovl_id)]), 2L)

    # and there should be only one unique ID
    expect_equal(unique(x[!is.na(ovl_id)][["ovl_id"]]), "ovl_2_1")
  }
)

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
      subset(id == "353") |>
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