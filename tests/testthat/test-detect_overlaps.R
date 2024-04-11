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
      polmineR::subset(id == "353") |>
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