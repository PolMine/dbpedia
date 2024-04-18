test_that(
  "entity_types_map function maps entity types to categories after get_dbpedia_uris function",
  {

    doc <- "Berlin is the capital of Germany."

    uritab <- get_dbpedia_uris(
      x = doc,
      max_len = 5600L,
      confidence = 0.35,
      api = "https://api.dbpedia-spotlight.org/en/annotate",
      language = "en",
      types = character(),
      support = 20,
      types_src = c("DBpedia", "Wikidata"),
      verbose = TRUE
    )

    mapping_vector = c(
      "PERSON" = "DBpedia:Person",
      "ORGANIZATION" = "DBpedia:Organisation",
      "LOCATION" = "DBpedia:Place"
    )

    entity_types_map(
      x = uritab,
      mapping_vector = mapping_vector
    )

    testthat::expect_equal(uritab[["category"]],
                           c("LOCATION", "MISC", "LOCATION"))
  }
)

test_that(
  "entity_types_map function maps entity types to categories for character vectors",
  {

    # Examples based on processing quanteda's data_corpus_inaugural with the
    # English DBpedia Spotlight endpoint (entities occur in 2013-Obama).

    types_vector <- c("|Species|Person|Eukaryote|Animal|Politician|",
                      "|Organisation|Agent|Legislature|",
                      "|Place|Location|CelestialBody|Planet|",
                      "|ArchitecturalStructure|Building|",
                      NA
    )

    # for character vectors, omit the source
    mapping_vector_character = c(
      "PERSON" = "Person",
      "ORGANIZATION" = "Organisation",
      "LOCATION" = "Place"
    )

    categories <- entity_types_map(
      types_vector,
      mapping_vector = mapping_vector_character
    )

    # first for the character vector
    testthat::expect_equal(categories,
                           c("PERSON", "ORGANIZATION", "LOCATION", "MISC", "MISC"))
  }
)

test_that(
  "entity_types_map function maps entity types to categories for data.table objects independent from get_dbpedia_uris",
  {

    withr::local_package("data.table")

    # Examples based on processing quanteda's data_corpus_inaugural with the
    # English DBpedia Spotlight endpoint (entities occur in 2013-Obama).

    types_vector <- c("|Species|Person|Eukaryote|Animal|Politician|",
                      "|Organisation|Agent|Legislature|",
                      "|Place|Location|CelestialBody|Planet|",
                      "|ArchitecturalStructure|Building|",
                      NA
    )

    types_dt <- data.table(
      uri = c(
        "http://dbpedia.org/resource/Joe_Biden",
        "http://dbpedia.org/resource/United_States_Congress",
        "http://dbpedia.org/resource/Earth",
        "http://dbpedia.org/resource/United_States_Capitol",
        "http://dbpedia.org/resource/Vice_President_of_the_United_States"
      ),

      DBpedia_type = types_vector
    )

    # for data.tables, add the source
    mapping_vector_dt = c(
      "PERSON" = "DBpedia:Person",
      "ORGANIZATION" = "DBpedia:Organisation",
      "LOCATION" = "DBpedia:Place"
    )

    # modify types_dt by reference
    entity_types_map(
      types_dt,
      mapping_vector = mapping_vector_dt
    )

    # test for input as data.table
    testthat::expect_equal(types_dt[["category"]],
                           c("PERSON", "ORGANIZATION", "LOCATION", "MISC", "MISC"))
  }
)

test_that(
  "entity_types_map function maps entity types to categories for data.table objects with multiple source KBs",
  {

    withr::local_package("data.table")

    # Examples based on processing quanteda's data_corpus_inaugural with the
    # English DBpedia Spotlight endpoint (entities occur in 2013-Obama).

    types_vector <- c("|Species|Person|Eukaryote|Animal|Politician|",
                      "|Organisation|Agent|Legislature|",
                      "|Place|Location|CelestialBody|Planet|",
                      "|ArchitecturalStructure|Building|",
                      NA
    )

    types_dt <- data.table(
      uri = c(
        "http://dbpedia.org/resource/Joe_Biden",
        "http://dbpedia.org/resource/United_States_Congress",
        "http://dbpedia.org/resource/Earth",
        "http://dbpedia.org/resource/United_States_Capitol",
        "http://dbpedia.org/resource/Vice_President_of_the_United_States"
      ),
      DBpedia_type = types_vector,
      Wikidata_type = c(NA, NA, "Q634", NA, NA)
    )

    mapping_vector = c(
      "PERSON" = "DBpedia:Person",
      "ORGANIZATION" = "DBpedia:Organisation",
      "LOCATION" = "DBpedia:Place",
      "PLANET" = "Wikidata:Q634"
    )

    # modify types_dt by reference
    entity_types_map(
      x = types_dt,
      mapping_vector = mapping_vector
    )

    # test for input as data.table
    testthat::expect_equal(types_dt[["category"]],
                           c("MISC|PERSON", "MISC|ORGANIZATION", "LOCATION|PLANET", "MISC", "MISC"))
  }
)

test_that(
  "entity_types_map function maps entity types to categories for data.table objects with multiple types in the same KB",
  {

    withr::local_package("data.table")

    # Examples based on processing quanteda's data_corpus_inaugural with the
    # English DBpedia Spotlight endpoint (entities occur in 2013-Obama).

    types_vector <- c("|Species|Person|Eukaryote|Animal|Politician|",
                      "|Organisation|Agent|Legislature|",
                      "|Place|Location|CelestialBody|Planet|",
                      "|ArchitecturalStructure|Building|",
                      NA
    )

    types_dt <- data.table(
      uri = c(
        "http://dbpedia.org/resource/Joe_Biden",
        "http://dbpedia.org/resource/United_States_Congress",
        "http://dbpedia.org/resource/Earth",
        "http://dbpedia.org/resource/United_States_Capitol",
        "http://dbpedia.org/resource/Vice_President_of_the_United_States"
      ),
      DBpedia_type = types_vector,
      Wikidata_type = c(NA, NA, "Q634", NA, NA)
    )

    mapping_vector = c(
        "PERSON" = "DBpedia:Person",
        "ORGANIZATION" = "DBpedia:Organisation",
        "LEGISLATURE" = "DBpedia:Legislature",
        "LOCATION" = "DBpedia:Place"
    )

    # modify types_dt by reference
    entity_types_map(
      x = types_dt,
      mapping_vector = mapping_vector
    )

    testthat::expect_equal(types_dt[["category"]],
                           c("PERSON", "LEGISLATURE|ORGANIZATION", "LOCATION", "MISC", "MISC"))
    }
)
