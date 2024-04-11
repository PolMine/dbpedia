## dbpedia v0.1.2.9005

* A new auxiliary function `segment()` generates overlapping segments of text for strings longer than the maximum nchar that can be processed by DBpedia Spotlight.
* Method `get_dbpedia_uris()` has new argument `overlap` passed into `segment()`. 
* Method `get_dbpedia_uris()` has new argument `offset` to indicate base offset number other than 1.
* Method `get_dbpedia_urs()` has new argument `doc_id` that is used in logfile and in output table.


## dbpedia v0.1.2.9004

* Method `get_dbpedia_uris()` has new argument `retry` to retry if API is stalled #45 and new argument `logfile` for tracking and debugging longrunning annotation tasks. If the annotation failes, `NULL` is returned (no abort).


## dbpedia v0.1.2.9003

* started introducing functionality to detect and resolve overlaps (see issue #42) with `detect_overlap()` and `categorize_overlap()`
* added `resolve_overlap()` as an (experimental) function to resolve overlaps identified and categorized in `detect_overlap()` and `categorize_overlap()`
* introduced tests for `detect_overlap()` and `categorize_overlap()`
* modified test suite to follow recommendations of "R Packages" (2nd edition) by Wickham and Bryan, in particular by using `withr` for self-contained tests


## dbpedia v0.1.2.9002

* `expand_to_token` of `get_dbpedia_uris()` also expands spans to the left now (#44)
* added `end` to data.table grouping in `get_dbpedia_uris()` for subcorpora to address issue #43. This avoids processing multiple entities at the same time.
* reorganized tests via `testthat`, i.e. removed `context` and renamed files to start with "test-"
* added test for `expand_to_token` argument


## dbpedia v0.1.2.9001

* `entity_types_map()` now creates assignments again (#40) and returns them as character vectors
* `entity_types_map()` also passes all arguments when used with data.table objects
* `types_src` works in `get_dbpedia_uris()` for documents with a single type (#41)
* messages for `types_src` follow verbosity set by the argument `verbose`


## dbpedia v0.1.2
* `get_dbpedia_uris()` has new argument `types` to filter results.
* `dbpedia_spotlight_status()` without warnings if docker not available / not running #32.
* `get_dbpedia_uris()` has new argument `support` #30.
* The confusingly mixed usage of argument 'limit' in `dbpedia_get_wikidata_uris()` is resolved by adding a new argument 'chunksize' #29.
* As a matter of consistency, argument 'limit' of `query_wikidata()` has been renamed to `chunksize` #29.
* `xml_enrich()` now adds new attributes to pre-annotated features
* `get_dbpedia_uris()` method now includes argument `expand_to_token` for subcorpus_bundles as well
* `map_types_to_class()` works with the list representation in the types column
* new functions `to_annotation()`, `xml_enrich()` `namespaced_xpath()` and method `get_dbpedia_uris()` for xml docs.
* dropped argument `return_types` from `get_dbpedia_uris()`. Column `types` is 
kept in output, with parsed output #24.
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
