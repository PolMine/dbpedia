R package ‘dbpedia’ - wrapper for DBpedia Spotlight
================

<!-- badges: start -->

[![License: GPL
v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![R-CMD-check](https://github.com/PolMine/dbpedia/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/PolMine/dbpedia/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/PolMine/dbpedia/branch/main/graph/badge.svg)](https://codecov.io/gh/PolMine/dbpedia/branch/main)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## About

Functionality for entity linking from R: Get DBpedia URIs for existing
named entity recognition in a corpus using DBpedia Spotlight.

## Installation

At this stage, the dbpedia R package is a GitHub-only package. Install
it as follows:

``` r
devtools::install_github("PolMine/dbpedia", ref = "main")
```

The SPARQL package is a suggested package used internally by functions
\`\` and was archived at CRAN. The latest version (1.16) can be
installed as follows.

``` r
devtools::install_version("SPARQL", version = "1.16", repos = "http://cran.us.r-project.org") 
```

## Run dockerized DBpedia Spotlight

DBpedia Spotlight docker containers are not available for all
architectures. Particularly not for M1. In this case, build container
from the dockerfile as follows.

``` sh
git clone https://github.com/dbpedia-spotlight/spotlight-docker.git
cd spotlight-docker
docker build -t dbpedia/dbpedia-spotlight:latest .
```

Then run the image as follows.

``` sh
docker run -tid \
  --restart unless-stopped \
  --name dbpedia-spotlight.de \
  --mount source=spotlight-model,target=/opt/spotlight \
  -p 2222:80  \
  dbpedia/dbpedia-spotlight spotlight.sh de
```

## Using the package

Upon loading the dbpedia package, a startup message will issue
information whether a Docker container running locally has been detected
and on the API and language used.

``` r
library(dbpedia)
```

    ## * Using DBpedia Spotlight online API (no local Docker container running)

    ## * endpoint/api: http://api.dbpedia-spotlight.org/en/annotate

    ## * language: en

This information is available during the R session and is used by the
`get_dbpedia_uris()` method.

``` r
getOption("dbpedia.endpoint")
```

    ## [1] "http://api.dbpedia-spotlight.org/en/annotate"

``` r
getOption("dbpedia.lang")
```

    ## [1] "en"

``` r
library(polmineR)
library(dplyr)

speech <- corpus("GERMAPARL2") %>% 
  subset(protocol_date == "2018-02-22") %>%
  subset(speaker_name == "Angela Merkel") %>%
  subset(p_type == "speech") %>%
  as.speeches(
    s_attribute_name = "speaker_name",
    s_attribute_date = "protocol_date",
    progress = FALSE
  ) %>%
  .[[1L]]

named_entites <- speech %>%
  subset(ne_type) %>%
  as_annotation()

read(speech, annotation = named_entites)

dbpedia_uris <- get_dbpedia_uris(
  x = speech,
  language = "de",
  s_attribute = "ne_type"
)

read(speech, annotation = as_subcorpus(dbpedia_uris))
```

``` r
library(RCurl)
library(XML)

dbpedia_uris <- dbpedia_uris %>%
  pull(dbpedia_uri) %>% 
  dbpedia_get_wikidata_uris(
    optional = "municipalityCode",
    endpoint = "http://de.dbpedia.org/sparql",
    wait = 0.5,
    limit = 100,
    progress = TRUE
  ) %>% 
  right_join(dbpedia_uris, by = "dbpedia_uri")
```

``` r
dbpedia_results <- dbpedia_uris %>%
  pull(wikidata_id) %>% 
  wikidata_query(
    id = "P439", # German municipality key
    wait = 1,
    limit = 100,
    progress = TRUE
  )
```

## Related work

- <https://github.com/glaserL/unsc-ne>

- <https://pypi.org/project/spacy-dbpedia-spotlight/>

- <https://github.com/news-r/spotlight>

## Acknowledgements

We gratefully acknowledge funding from the German National Research Data
Infrastructure (Nationale Forschungsdaten-Infrastruktur / NFDI).
Developing the dbpedia package is part of the measure “Linking Textual
Data” as part of the consortium KonsortSWD (project number 442494171).
