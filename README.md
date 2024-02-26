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

Functionality for Entity Linking from R: Get DBpedia URIs for entities
in a corpus using DBpedia Spotlight.

## Motivation

The method of Entity Linking is used to disambiguate entities such as
Persons, Organizations and Locations in continuous text and link them to
entries in an external knowledge graph. At its core, the aim of the
`dbpedia` R package is to integrate Entity Linking with the tool
“DBpedia Spotlight” (<https://www.dbpedia-spotlight.org>) into common
workflows for text analysis in R. In particular, it addresses the
following needs:

- facilitate the use of Entity Linking with DBpedia Spotlight from
  within R
- prepare and process textual data in an integrated, easy-to-use Entity
  Linking workflow for users working with R relying on different input
  and output formats
- realize the potential of retrieved Uniform Resource Identifiers (URIs)
  to retrieve additional data from the DBpedia Knowledge Graph and
  Wikidata by further integration in the provided analysis pipeline

For the examples in this README, we also load some additional packages.

``` r
library(kableExtra)
library(dplyr)
library(quanteda)
```

## First Look

The main motivation is to lower barriers to link textual data with other
resources in social science research. As such, it aims to provide a
focused way to interact with DBpedia Spotlight from within R. In its
most basic application, the package can be used to query DBpedia
Spotlight as follows:

``` r
library(dbpedia)

doc <- "Berlin is the capital city of Germany."

uri_table <- get_dbpedia_uris(x = doc,
                              language = getOption("dbpedia.lang"),
                              api = getOption("dbpedia.endpoint")
)
```

DBpedia Spotlight is able to identify which parts of the text represent
entities and decide which resources in the knowledge graph DBpedia they
correspond with. The return value of the method is a data.table
containing identified entities along with their respective DBpedia URIs
and starting positions in the document.

| start | text         | dbpedia_uri                                |
|------:|:-------------|:-------------------------------------------|
|     1 | Berlin       | <http://dbpedia.org/resource/Berlin>       |
|    15 | capital city | <http://dbpedia.org/resource/Capital_city> |
|    31 | Germany      | <http://dbpedia.org/resource/Germany>      |

## Installation and Setup

At this stage, the `dbpedia` R package is a GitHub-only package. Install
it as follows:

``` r
devtools::install_github("PolMine/dbpedia", ref = "main")
```

In a nutshell, the package prepares queries, sends them to an external
tool and parses the returned results. This tool, DBpedia Spotlight, is
running as a Web Service - either remotely or locally. The developers of
DBpedia Spotlight currently maintain a public endpoint for the service
which is selected by default by the `dbpedia` package.

### Running DBpedia Spotlight locally - Docker Setup

As an alternative to the public endpoint, it is possible to run the
service locally. This can be reasonable for reasons of performance, rate
limits of the public endpoint and other considerations. The easiest way
to realize this is to use the tool within a Docker container prepared by
the maintainers of DBpedia Spotlight. The setup is described in some
detail in the corresponding GitHub repository:
<https://github.com/dbpedia-spotlight/spotlight-docker>.

As described on the GitHub page, with Docker running, the quick-start
command to be used in the terminal to load and run a DBpedia Spotlight
model is as follows:

``` sh
docker run -tid \
  --restart unless-stopped \
  --name dbpedia-spotlight.de \
  --mount source=spotlight-model,target=/opt/spotlight \
  -p 2222:80  \
  dbpedia/dbpedia-spotlight spotlight.sh de
```

This will initialize the German Docker DBpedia Spotlight model. Other
available languages are described in the GitHub repository as well.

**Note**: In our tests, we noticed that the DBpedia Spotlight Docker
containers are not available for all architectures, in particular Apple
silicon. In this case, build container from the dockerfile as follows
before loading the model:

``` sh
git clone https://github.com/dbpedia-spotlight/spotlight-docker.git
cd spotlight-docker
docker build -t dbpedia/dbpedia-spotlight:latest .
```

**Note**: When run the first time, the script will download the language
model. Depending on the language, this download and the subsequent
initialization of the model will take some time. This process is not
necessarily obvious in the output of the terminal. If the container is
queried before the language model is fully initialized, the download or
initialization of the model seems to be interrupted which will cause
errors when queried later on. It is thus advisable to wait until the
container is idle before querying the service the first time in a
session.

### Using the package - A Very Quick Warkthrough with `quanteda` corpora

This README will use the common `quanteda` corpus format as input to
provide a quick step-by-step overview about the functionality provided
by the package. A brief second example will illustrate how the extracted
Uniform Resource Identifiers can be mapped back onto the input, using a
Corpus Workbench corpus as an example.

#### Setup - Loading the package

Upon loading the `dbpedia` package, a start up message will print
information about whether the DBpedia Spotlight service is running
locally or if a public endpoint is used. In addition, the language of
the model and the corresponding list of stop words is shown.

``` r
library(dbpedia)
```

This information is available during the R session and is by default
used by the `get_dbpedia_uris()` method.

``` r
getOption("dbpedia.endpoint")
getOption("dbpedia.lang")
```

#### Data

For the following example, we use the “US presidential inaugural address
texts” corpus from the `quanteda` R package. For illustrative purposes,
only speeches since 1970 are used. To create useful chunks of text, we
split the corpus into paragraphs.

``` r
inaugural_paragraphs <- data_corpus_inaugural |>
  corpus_subset(Year > 1970) |>
  corpus_reshape(to = "paragraphs")
```

#### Entity Linking with `get_dbpedia_uris()`

Using a local endpoint for the DBpedia Spotlight service and the sample
corpus from `quanteda`, identifying and disambiguating entities in
documents can be realized with the main worker method the package:
`get_dbpedia_uris()`.

The method accepts the data in different input formats -
`character vectors`, `quanteda` corpora, Corpus Workbench format, XML -
as well as additional parameters, some of which are discussed in more
detail in the package’s vignette.

``` r
uritab_paragraphs <- get_dbpedia_uris(
  x = inaugural_paragraphs,
  language = getOption("dbpedia.lang"),
  max_len = 5600L,
  confidence = 0.5,
  api = getOption("dbpedia.endpoint"),
  verbose = FALSE,
  progress = FALSE
)
```

In this case, the text of each document in the corpus is extracted and
passed to the DBpedia Spotlight service. The results are then parsed by
the method. The return value is a `data.table` containing the document
name as well as the extracted entities along with their starting
position in the text and, most importantly, their respective URI in the
DBpedia Knowledge Graph (only the first five entities are shown here and
the column containing the types of the entities is omitted):

| doc          | start | text           | dbpedia_uri                                                                         |
|:-------------|------:|:---------------|:------------------------------------------------------------------------------------|
| 1973-Nixon.1 |     5 | Vice President | <http://dbpedia.org/resource/Vice_President_of_the_United_States>                   |
| 1973-Nixon.1 |    25 | Speaker        | <http://dbpedia.org/resource/Speaker_of_the_United_States_House_of_Representatives> |
| 1973-Nixon.1 |    38 | Chief Justice  | <http://dbpedia.org/resource/Chief_Justice_of_the_United_States>                    |
| 1973-Nixon.1 |    72 | Eisenhower     | <http://dbpedia.org/resource/Dwight_D._Eisenhower>                                  |
| 1973-Nixon.6 |   154 | Peking         | <http://dbpedia.org/resource/Beijing>                                               |

The package’s vignette provides some more details to the approach and
parameters.

### Token-Level Annotation with the Corpus Workbench

While approaches that enrich documents with entities are very useful,
another important aspect of entity linking is the ability to assign URIs
to precise spans within the text and write them back to the corpus. This
can be crucial if extracted URIs should be used in subsequent tasks when
working with textual data. The Corpus Workbench data format makes it
possible to map annotated entities onto the continuous text of the
initial corpus. The following quick example should illustrate this.

#### Data

For this example, we use a single newswire of the REUTERS corpus. The
corpus is provided as a Corpus Workbench sample corpus in the `RcppCWB`
R package. To work with CWB corpora in R, the R package `polmineR` is
used. Both `RcppCWB` and `polmineR` are dependencies of `dbpedia`.

``` r
library(polmineR)
use("RcppCWB")
```

To extract an illustrative part of the REUTERS corpus, we create a
subcorpus comprising of a single document. To do so, we use `polmineR`’s
`subset()` method for CWB corpus objects.

``` r
reuters_newswire <- corpus("REUTERS") |>
  subset(id == 144)
```

#### Entity Linking with `get_dbpedia_uris()`

Like before, we perform Entity Linking with `get_dbpedia_uris()`. In
addition, we map entity types returned by DBpedia Spotlight to a number
of entity classes (see the vignette for a more comprehensive
explanation).

``` r
mapping_vector = c(
  "PERSON" = "DBpedia:Person",
  "ORGANIZATION" = "DBpedia:Organisation",
  "LOCATION" = "DBpedia:Place"
)

reuters_newswire_annotation <- reuters_newswire |>
  get_dbpedia_uris(verbose = FALSE) |>
  map_types_to_class(mapping_vector = mapping_vector)
```

    ## ! Cannot map 4 entities exactly to tokenstream. Dropping them from the annotation.

    ## ℹ mapping values in column `types` to new column `class`

This results in the following annotations (only the first five entities
are shown here and the column of types is omitted):

| cpos_left | cpos_right | dbpedia_uri                                      | text         | class    |
|----------:|-----------:|:-------------------------------------------------|:-------------|:---------|
|        92 |         92 | <http://dbpedia.org/resource/OPEC>               | OPEC         | LOCATION |
|       117 |        117 | <http://dbpedia.org/resource/Reversal_film>      | slide        | MISC     |
|       119 |        120 | <http://dbpedia.org/resource/Price_of_oil>       | oil prices   | MISC     |
|       121 |        122 | <http://dbpedia.org/resource/Petroleum_industry> | oil industry | MISC     |
|       129 |        130 | <http://dbpedia.org/resource/Price_of_oil>       | oil prices   | MISC     |

#### Mapping the Results to the Corpus

This leaves us with a similar output like before. As explained in more
detail in the vignette, the output of `get_dbpedia_uris()` for CWB
objects additionally contains corpus positions of entities within the
continuous text. This allows us to map the annotations back to the
corpus.

`polmineR`’s `read()` method allows us to visualize this mapping
interactively, using the classes of the entities to provide some visual
clues as well.

``` r
read(reuters_newswire,
     annotation = as_subcorpus(reuters_newswire_annotation, highlight_by = "class"))
```

#### Advanced Scenarios

This README only offers a first look into the functions of the `dbpedia`
package. Specific parameters as well as other scenarios are discussed in
more detail in the vignette of the package. These scenarios include the
integration of SPARQL queries in the workflow to further enrich
disambiguated entities with additional data from the DBpedia and
Wikidata knowledge graphs.

## Related work

- <https://github.com/glaserL/unsc-ne>

- <https://pypi.org/project/spacy-dbpedia-spotlight/>

- <https://github.com/news-r/spotlight>

## Acknowledgements

We gratefully acknowledge funding from the German National Research Data
Infrastructure (Nationale Forschungsdateninfrastruktur / NFDI).
Developing the dbpedia package is part of the measure “Linking Textual
Data” as part of the consortium KonsortSWD (project number 442494171).
