test_that(
  "connection errors are retried and result in an error if retries fail",
  {
    # nothing listening on port 1: 'Couldn't connect to server'
    api <- "http://localhost:1/rest/annotate"
    expect_error(
      get_dbpedia_uris(x = "Berlin", language = "en", api = api, retry = 0L),
      "connect"
    )
    t <- system.time(
      expect_error(
        get_dbpedia_uris(x = "Berlin", language = "en", api = api, retry = 1L),
        "connect"
      )
    )[["elapsed"]]
    expect_gte(t, 5)
  }
)
