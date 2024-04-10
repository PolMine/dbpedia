library(polmineR)
use("RcppCWB") # make REUTERS corpus available

test_that(
  "ensure that segments add up to original string",
  {
    article <- corpus("REUTERS") %>%
      polmineR::subset(id == "236") %>% # the longest article in the REUTERS corpus
      get_token_stream(p_attribute = "word", collapse = " ")

    segs <- segment(x = article, max_len = 500, overlap = 100)
    
    # we grow the reconstructed string ...
    article_reconstructed <- character()
    for (i in seq_along(segs)){
      article_reconstructed <- paste(
        substr(
          article_reconstructed,
          start = 1L,
          stop = as.integer(names(segs)[[i]]) - 1L
        ),
        segs[[i]],
        sep = ""
      )
    }

    expect_identical(nchar(article), nchar(article_reconstructed))
    expect_identical(article, article_reconstructed)
  }
)


test_that(
  "identity of results",
  {
    article <- corpus("REUTERS") %>%
      polmineR::subset(id == "236") %>% # the longest article in the REUTERS corpus
      get_token_stream(p_attribute = "word", collapse = " ")
    
    dbpedia_uris_ref <- get_dbpedia_uris(
      x = article,
      api = "http://api.dbpedia-spotlight.org/en/annotate",
      language = "en",
      verbose = FALSE,
      max_len = 7500L
    )
    
    dbpedia_uris_seg <- get_dbpedia_uris(
      x = article,
      api = "http://api.dbpedia-spotlight.org/en/annotate",
      language = "en",
      verbose = FALSE,
      max_len = 2000,
      overlap = 750
    )
    
    expect_identical(dbpedia_uris_ref, dbpedia_uris_seg)
    
    expect_identical(dbpedia_uris_ref, dbpedia_uris_seg)
  }
)