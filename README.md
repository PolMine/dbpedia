R wrapper pkg to DBpedia Spotlight
================

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
docker run -tid --restart unless-stopped --name dbpedia-spotlight.de --mount source=spotlight-model,target=/opt/spotlight -p 2222:80 dbpedia/dbpedia-spotlight spotlight.sh de
```

## Using the package

``` r
library(polmineR)
library(dbpedia)

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

dbpedia_links <- get_dbpedia_links(
  x = speech,
  language = "de",
  mw = "ne_type"
)

read(speech, annotation = dbpedia_links)
```

## Related work

- <https://github.com/glaserL/unsc-ne>

- <https://pypi.org/project/spacy-dbpedia-spotlight/>

- <https://github.com/news-r/spotlight>
