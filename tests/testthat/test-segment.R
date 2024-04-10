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