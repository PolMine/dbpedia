# assumes that repo has been cloned and is locally available from:
# https://github.com/dbpedia-spotlight/model-quickstarter.git

library(fs)

repo_dir <- "~/Lab/github_foreign/model-quickstarter/"

languages <- grep("^\\w{2}$", basename(list.dirs(repo_dir)), value = TRUE)

dbpedia_stopwords <- lapply(
  languages,
  function(lang){
    stopwords_file <- path(repo_dir, lang, "stopwords.list")
    readLines(stopwords_file)
  }
)
names(dbpedia_stopwords) <- languages

save(
  dbpedia_stopwords,
  file = "~/Lab/github/dbpedia/data/stopwords.RData"
)