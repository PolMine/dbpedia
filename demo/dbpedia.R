library(polmineR)
library(dbpedia)

# arbitrarily chosen article from Sueddeutsche Zeitung (SZ) to annotate
article <- corpus("NADIRASZ") %>% subset(article_id == "A70041931")

# get named entities/NEs present (previous annotation using StanfordCore NLP)
named_entities <- article %>% subset(ne_type) %>% as_annotation()

# inspect article with NE annotation before linking step
read(article, annotation = named_entities)

# get DBpedia links for named entities using DBpedia Spotlight
dbpedia_links <- get_dbpedia_uris(x = article, language = "de", s_attribute = "ne_type")

# inspect DBpedia resource URIs
read(article, annotation = as_subcorpus(dbpedia_links))
