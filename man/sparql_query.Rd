% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{sparql_query}
\alias{sparql_query}
\title{Query SPARQL endpoint}
\usage{
sparql_query(endpoint, query)
}
\arguments{
\item{endpoint}{URL of a SPARQL endpoint.}

\item{query}{A (single) SPARL query.}
}
\description{
Query SPARQL endpoint
}
\examples{
sparql_endpoint <- "http://de.dbpedia.org/sparql"
query <- 'SELECT distinct ?item ?wikidata_uri ?key
  WHERE {
    VALUES ?item {
      <http://de.dbpedia.org/resource/Bayernpartei>
      <http://de.dbpedia.org/resource/Deutsches_Kaiserreich>
    }
    ?item owl:sameAs ?wikidata_uri
    FILTER(regex(str(?wikidata_uri), "www.wikidata.org" ) )
  }
  LIMIT 100
'
sparql_query(endpoint = sparql_endpoint, query = query)
}
