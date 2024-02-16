#' Add entities to original XML
#' 
#' @param xml ...
#' @param annotation_dt ...
#' @param entity_tags ...
#' @param entity_name ...
#' @param token_tags ...
#' @param attributes ...
#' @export
#' @importFrom xml2 xml_set_attrs
xml_enrich <- function(xml,
                       annotation_dt,
                       entity_tags = c("w", "pc", "name"),
                       entity_name = "name",
                       token_tags = c("w", "pc"),
                       attributes = "dbpedia_uri"
) {
  
  # get all nodes which might contain entities
  nodes <- xml |>
    xml2::xml_find_all(xpath = namespaced_xpath(xml = xml, tags = entity_tags))
  
  node_ids <- nodes |>
    xml2::xml_attr("id")
  
  # for each annotation, extract identified words 
  
  for (i in 1:nrow(annotation_dt)) {
    
    # in theory, an annotation can comprise more than one word
    annotation_id <- annotation_dt[i, ][["original_id"]] |>
      strsplit(split = "\\|") |>
      unlist()
    
    # get what to add
    attributes_to_add <- sapply(attributes, function(x) annotation_dt[i, ][[x]])
    
    # there could be additional values such as the type?
    nodes_idx <- which(node_ids %in% annotation_id)
    
    entity_nodes <- nodes[nodes_idx]
    
    # this assumes that an entity always describes continuous spans which is
    # plausible.
    
    # check if nodes are words (or punctuation) or already something like an
    # entity. If it is already an entity, an attribute should be added. If not,
    # a new parent node should be added.
    
    if (all(xml2::xml_name(entity_nodes) %in% token_tags)) {
      
      for (node_idx in 1:length(entity_nodes)) {
        if (node_idx == 1) {
          xml2::xml_add_parent(.x = entity_nodes[[node_idx]], .value = entity_name)
        } else {
          xml2::xml_add_sibling(.x = entity_nodes[[1]], .value = entity_nodes[[node_idx]], .copy  = FALSE)
        }
      }
      
      # and set attributes to this new parent node
      xml_set_attrs(xml2::xml_parent(entity_nodes), attributes_to_add)
      
    } else if (entity_nodes == entity_name) { 
      
      # this assumes that the new nodes should be the same name as the old
      # nodes. Are there scenarios in which this does not hold true?
      
      # if it is a named entity already, set attribute here
      # Does this work if there are multiple nodes here?
      
      xml_set_attrs(entity_nodes, attributes_to_add)
      
    }
  }
}



#' function which adds the namespace to the XPATH
#'
#' @param xml xml the namespace is derived from
#' @param tags the tags which should be queried with the XPATH#
#' @export
namespaced_xpath = function(xml = xml, tags) {
  
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
