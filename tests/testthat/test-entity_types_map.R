test_that(
  "entity_types_map function maps entity types to categories",
  {

    withr::local_package("data.table")

    # Examples based on processing quanteda's data_corpus_inaugural with the
    # English DBpedia Spotlight endpoint (entities occur in 2013-Obama).

    types_list <- list(
      # types of http://dbpedia.org/resource/Joe_Biden
      list("DBpedia" = c("Species", "Person", "Eukaryote", "Animal", "Politician"),
           "DUL" = "NaturalPerson",
           "Http" = "//xmlns.com/foaf/0.1/Person",
           "Schema" = "Person",
           "Wikidata" = c("Q82955", "Q729", "Q5", "Q215627", "Q19088")
      ),

      # types of http://dbpedia.org/resource/United_States_Congress
      list("DBpedia" = c("Organisation", "Agent", "Legislature"),
           "DUL" = c("SocialPerson", "Agent"),
           "Schema" = "Organization",
           "Wikidata" = c("Q43229", "Q24229398", "Q11204")
      ),

      # types of http://dbpedia.org/resource/Earth
      list("DBpedia" = c("Place", "Location", "CelestialBody", "Planet"),
           "Schema" = "Place",
           "Wikidata" = "Q634"
           ),

      # types of http://dbpedia.org/resource/United_States_Capitol
      list("DBpedia" = c("ArchitecturalStructure", "Building"),
           "Wikidata" = "Q41176"
      ),

      # types of http://dbpedia.org/resource/Vice_President_of_the_United_States (for example)
      list()
    )

    mapping_vector = c(
      "PERSON" = "DBpedia:Person",
      "ORGANIZATION" = "DBpedia:Organisation",
      "LOCATION" = "DBpedia:Place"
    )

    categories <- entity_types_map(
      types_list,
      mapping_vector = mapping_vector
    )

    # first for the list
    testthat::expect_equal(categories,
                           c("PERSON", "ORGANIZATION", "LOCATION", "MISC", "MISC"))

    types_dt <- data.table(
      uri = c(
        "http://dbpedia.org/resource/Joe_Biden",
        "http://dbpedia.org/resource/United_States_Congress",
        "http://dbpedia.org/resource/Earth",
        "http://dbpedia.org/resource/United_States_Capitol",
        "http://dbpedia.org/resource/Vice_President_of_the_United_States"
      ),
      types = types_list
    )

    # modify types_dt by reference
    entity_types_map(
      types_dt,
      mapping_vector = mapping_vector
    )

    # test for input as data.table
    testthat::expect_equal(types_dt[["category"]],
                           c("PERSON", "ORGANIZATION", "LOCATION", "MISC", "MISC"))

  }
)
