## dbpedia v0.1.1.9009

* dropped argument `return_types` from `get_dbpedia_uris()`. Column `types` is 
kept in output, with parsed output #24.


## dbpedia v0.1.1.9008

* Error avoided when `get_dbpedia_uris()` is restricted to pre-annotated
named entities and the document does not contain any (addresses issue #23).
* set default `max_len` in `get_dbpedia_uris()` to 5600 to avoid failing queries.
* `get_dbpedia_uris()` optionally returns types now (addresses issue #24).
* added `map_types_to_class` as an utility function to reduce types to a
limited set of classes.
* modified `as_subcorpus()` to make `read()` work without pre-annotated entities
and in tandem with `map_types_to_class()` by avoiding hard-coding the column name
"ne_type". Also added a color code to entities which are not within "PERSON",
"LOCATION", "ORGANIZATION" and "MISC".
* started to implement the `expand_to_token` argument which ultimately should
resolve mismatches between DBpedia Spotlight's entity spans and CWB token spans
(issue #26)
* added `drop_inexact_annotations` argument to `get_dbpedia_uris()` to control
keeping or omitting inexact annotations in output data.table (see issue #26)

## dbpedia v0.1.1.9006

* Error avoided when `get_dbpedia_uris()` does not detect any URI.
* More telling progress messages of `wikidata_query()` and
`dbpedia_get_wikidata_uris()`.
* `dbpedia_get_wikidata_uris()` implemented for `character` strings.
* Preliminary implementation of `dbpedia_get_wikidata_uris()` for 'corpus' objects
from package quanteda.
* New function `add_wikidata_uris()` to add wikidata URIs to a table with
DBpedia URIs.
* `wikidata_query()` is a method now, result does not include columns
"key and keyLabel" any more. The column with values queries from Wikidata is 
ID now (not "label", as previously).
* New auxiliary function `sparql_query()` replaces `SPARQL::SPARQL()` and is 
basis for dropping packages SPARQL, XML and RCurl as dependencies. Package xml2
is a new dependency.
* Default value of argument `max_len` is now 5680.
* Bugged result for `get_dbpedia_uris()` if s_attribute = NULL corrected #18.


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
