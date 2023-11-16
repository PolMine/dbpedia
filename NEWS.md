## dbpedia v0.1.1.9002

* Error avoided when `get_dbpedia_uris()` does not detect any URI.
* More telling progress messages of `wikidata_query()` and
`dbpedia_get_wikidata_uris()`.
* `dbpedia_get_wikidata_uris()` implemented for `character` strings.
* Preliminary implementation of `dbpedia_get_wikidata_uris()` for 'corpus' objects
from package quanteda.

## dbpedia v0.1.1

* New function `dbpedia_spotlight_status()`.

## dbpedia v0.1.0

* New auxiliary function `as_chunks()`.
* New function `dbpedia_get_wikidata_uris()`.
* New function `wikidata_query()` as high-level wrapper for
`WikidataQueryServiceR::query_wikidata()`.
* Method `get_dbpedia_links()` renamed to `get_dbpedia_uris()`, return value is
now a `data.table`, argument `mw` of method `get_dbpedia_links()` for
`subcorpus` objects renamed as `s_attribute`.
* Auxiliary function `as_subcorpus()` to turn `data.table` with DBpedia URIs
into `subcorpus` that can be used for annotation.


## dbpedia v0.0.1

* `get_dbpedia_links()` returns `subcorpus` with information in slot `annotations`.

## dbpedia v0.0.0.9003

* Vignette with a very, very simple example.

## dbpedia v0.0.0.9001

* Added 'SystemRequirements: docker' to DESCRIPTION
