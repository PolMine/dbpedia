#' Add entities to original XML
#'
#' Enrich original XML with retrieved Uniform Resource Identifiers and
#' additional information.
#'
#' @param xml The XML document to be modified in place.
#' @param annotation_dt A `data.frame` or `data.table` of annotations returned
#'   by `get_dbpedia_uris()`. Must contain token IDs and values to be added
#'   (e.g. URIs).
#' @param token_tags A `character vector`, the names of XML nodes containing
#'   tokens, i.e. which potentially can be (part of) entities.
#' @param entity_name A `character vector`, the name of added XML nodes
#'   describing entities.
#' @param feature_tag A `character vector`, the name of pre-annotated features
#'   to be used and enriched.
#' @param attributes A `character vector`, the names of attributes to be added.
#'   Must correspond to column names in `annotation_dt`.
#' @export
#' @importFrom xml2 xml_set_attrs
xml_enrich <- function(xml,
                       annotation_dt,
                       token_tags = c("w", "pc"),
                       entity_name = "name",
                       feature_tag = NULL,
                       attributes = "dbpedia_uri"
) {

  # get all nodes which might contain entities
  nodes <- xml |>
    xml2::xml_find_all(xpath = namespaced_xpath(xml = xml, tags = token_tags))

  node_ids <- nodes |>
    xml2::xml_attr("id")

  # for each annotation, extract identified words 

  for (i in 1:nrow(annotation_dt)) {

    # get what to add
    attributes_to_add <- sapply(attributes, function(x) annotation_dt[i, ][[x]])

    # distinguish between enriched features (named entities, etc.) and enriched
    # tokens

    if (is.null(feature_tag)) {
      # if there is no feature tag, pre-annotated named entities weren't
      # provided. Add identified named entities to tokens.

      annotation_id <- annotation_dt[i, ][["original_id"]] |>
        strsplit(split = "\\|") |>
        unlist()

      # there could be additional values such as the type?
      nodes_idx <- which(node_ids %in% annotation_id)
      entity_nodes <- nodes[nodes_idx]

      for (node_idx in 1:length(entity_nodes)) {
        if (node_idx == 1) {
          xml2::xml_add_parent(.x = entity_nodes[[node_idx]], .value = entity_name)
        } else {
          xml2::xml_add_sibling(.x = entity_nodes[[1]], .value = entity_nodes[[node_idx]], .copy  = FALSE)
        }
      }

      # and set attributes to this new parent node
      xml_set_attrs(xml2::xml_parent(entity_nodes), attributes_to_add)

    } else {

      # else there is a pre-annotated feature such as named entities

      # the ID in the annotation data then points to the first word of the
      # entity, with a trailing "_FEATURE-TAG" behind the ID itself.

      annotation_id <- annotation_dt[i, ][["original_id"]]

      feature_tag_end <- paste0("_", feature_tag, "$")
      feature_word_id <- gsub(feature_tag_end, "", annotation_id)

      # get first word node
      nodes_idx <- which(node_ids %in% feature_word_id)
      feature_node <- xml2::xml_parent(nodes[nodes_idx])

      xml2::xml_set_attrs(x = feature_node, value = attributes_to_add)

    }
  }
}



#' function which adds the namespace to the XPATH
#'
#' @param xml xml the namespace is derived from
#' @param tags the tags which should be queried with the XPATH#
#' @export
namespaced_xpath <- function(xml = xml, tags) {
  
  xml_namespace <- xml2::xml_ns(xml)
  
  if (length(xml_namespace) == 1) {
    namespace_nm <- names(xml_namespace)
  } else {
    
    tei_ns_idx <- grep(pattern = "http://www.tei-c.org/ns/1.0",
                       x = xml_namespace)
    
    if (tei_ns_idx == 1) {
      namespace_nm <- names(xml_namespace[tei_ns_idx])
    } else {
      stop("Unspecified Namespace") 
    }
  }
  
  if (length(tags) == 1) {
    sprintf(".//%s:%s", namespace_nm, tags)
  } else {
    namespaced_xpaths <- sapply(tags, namespaced_xpath, xml = xml)
    paste0(namespaced_xpaths, collapse = "|")
  }
}
