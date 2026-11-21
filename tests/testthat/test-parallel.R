test_that(
  "invalid values of argument cl are rejected",
  {
    library(polmineR)
    use("RcppCWB", corpus = "REUTERS")
    bundle <- corpus("REUTERS") %>%
      split(s_attribute = "id", verbose = FALSE) %>%
      .[1:2]

    expect_error(get_dbpedia_uris(bundle, language = "en", cl = 0L), "positive integer")
    expect_error(get_dbpedia_uris(bundle, language = "en", cl = "a"), "cluster")

    skip_on_os("windows") # no fork clusters on Windows
    fk <- parallel::makeForkCluster(2L)
    on.exit(parallel::stopCluster(fk))
    expect_error(get_dbpedia_uris(bundle, language = "en", cl = fk), "fork clusters")
  }
)

test_that(
  "parallel processing yields the same result as sequential processing",
  {
    api <- getOption("dbpedia.endpoint")
    reachable <- !is.null(api) && tryCatch(
      !httr::http_error(httr::GET(api, query = list(text = "Berlin"), httr::timeout(5))),
      error = function(e) FALSE
    )
    skip_if_not(reachable, "DBpedia Spotlight endpoint not reachable")

    library(polmineR)
    use("RcppCWB", corpus = "REUTERS")
    bundle <- corpus("REUTERS") %>%
      split(s_attribute = "id", verbose = FALSE)

    args <- list(x = bundle, language = "en", api = api, verbose = FALSE)
    seq <- do.call(get_dbpedia_uris, args)
    par <- do.call(get_dbpedia_uris, c(args, list(cl = 2L, chunksize = 3L)))
    expect_identical(seq, par)

    cl <- parallel::makePSOCKcluster(2L)
    on.exit(parallel::stopCluster(cl))
    par_cl <- do.call(get_dbpedia_uris, c(args, list(cl = cl, progress = TRUE)))
    expect_identical(seq, par_cl)
  }
)
