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
#' @param ref A `character vector`, the name of the URI to be added.
#'   Must correspond to a column name in `annotation_dt`.
#' @param type A `character vector`, the name of the entity type to be added.
#'   Must correspond to a column name in `annotation_dt`.
#' @details
#' If feature_tag is not NULL, then existing entities are enriched when they
#' entirely overlap with new annotations. In this case, found `type`s are added
#' to the node. If they are not identical, new types are added to previously
#' annotated types with a pipe, indicating different annotation results.
#' Regardless of the value in `feature_tag`, all annotations in the annotation
#' data.table are added. To limit the annotation to entities which correspond to
#' pre-annotated entities, consult the documentation of `get_dbpedia_uris()`.
#' @export
#' @importFrom xml2 xml_add_parent xml_add_sibling xml_attr  xml_add_parent xml_add_sibling xml_children xml_find_all xml_name xml_parent xml_set_attr
xml_enrich <- function(xml,
                       annotation_dt,
                       token_tags = c("w", "pc"),
                       entity_name = "name",
                       feature_tag = NULL,
                       ref = "dbpedia_uri",
                       type = NULL
) {

  if (!ref %in% colnames(annotation_dt)) {
    cli::cli_alert_warning(text = "{.var ref} is not in {.strong annotation data.table}. No values will be added.")
  }

  if (!type %in% colnames(annotation_dt)) {
    cli::cli_alert_warning(text = "{.var type} is not in {.strong annotation data.table}. No values will be added.")
  }

  if (is.null(feature_tag)) {
    cli::cli_alert_warning(text = "If {.var feature_tag} is NULL, it is assumed that the input data does not contain named entity annotation. If the text is pre-annotated, add the name of the tags used to encode named anntations.")
  }

  # get all nodes which might contain entities
  nodes <- xml_find_all(
    xml,
    xpath = namespaced_xpath(xml = xml, tags = token_tags)
  )

  node_ids <- xml_attr(nodes, "id")

  # for each annotation, extract identified words 

  for (i in 1:nrow(annotation_dt)) {

    # get what to add
    ref_to_add <- annotation_dt[i, ][[ref]]
    type_to_add <- annotation_dt[i, ][[type]]

    # distinguish between enriched features (named entities, etc.) and enriched
    # tokens

    if (is.null(feature_tag)) {
      # if there is no feature tag, pre-annotated named entities weren't
      # provided. Add identified named entities to tokens.

      annotation_id <- unlist(strsplit(
        annotation_dt[i, ][["original_id"]],
        split = "\\|"
      )
      )

      # there could be additional values such as the type?
      nodes_idx <- which(node_ids %in% annotation_id)
      entity_nodes <- nodes[nodes_idx]

      for (node_idx in 1:length(entity_nodes)) {
        if (node_idx == 1) {
          xml_add_parent(.x = entity_nodes[[node_idx]], .value = entity_name)
        } else {
          xml_add_sibling(.x = entity_nodes[[1]], .value = entity_nodes[[node_idx]], .copy  = FALSE)
        }
      }

      # and set attributes to this new parent node
      xml_set_attr(xml_parent(entity_nodes), attr = "type", value = type_to_add)
      xml_set_attr(xml_parent(entity_nodes), attr = "ref", value = ref_to_add)

    } else {

      # else there is a pre-annotated feature such as named entities

      # there are two scenarios:
      # 1: feature tags were used when getting DBpedia URIs previously
      # 2: feature tags were not used before but now, should be considered when adding tags

      # In scenario 1, the ID in the annotation data then points to the first
      #word of the entity, with a trailing "_FEATURE-TAG" behind the ID itself.

      annotation_id <- annotation_dt[i, ][["original_id"]]
      feature_tag_end <- paste0("_", feature_tag, "$")

      if (grepl(pattern = feature_tag_end, x = annotation_id)) {
        feature_word_id <- gsub(feature_tag_end, "", annotation_id)

        # get first word node
        nodes_idx <- which(node_ids %in% feature_word_id)
        feature_node <- xml_parent(nodes[nodes_idx])

        # check if there is already a type in the node
        original_type <- xml_attr(x = feature_node, attr = "type")
        type_to_add <- paste0(unique(c(type_to_add, original_type)), collapse = "|")

        xml_set_attr(feature_node, attr = "type", value = type_to_add)
        xml_set_attr(feature_node, attr = "ref", value = ref_to_add)

      } else {

        # In scenario 2, all tokens are considered to link. Now it is possible
        # that there already are named entities annotated. If the new annotation
        # and the existing NE annotation  completely overlap, the existing named
        # entity should be enriched.

        anno_token_ids <- unlist(strsplit(annotation_id, split = "\\|"))
        nodes_idx <- which(node_ids %in% anno_token_ids)
        feature_node <- xml_parent(nodes[nodes_idx])

        # if they all have the same parent, add attributes to this parent
        # (the check whether all children are in the new annotation is important
        # for nested entities).

         if (length(feature_node) == 1 & all(xml_attr(xml_children(feature_node), "id") %in% anno_token_ids)) {

          # check if there is already a type in the node
          original_type <- xml_attr(x = feature_node, attr = "type")
          type_to_add <- paste0(unique(c(original_type, type_to_add)), collapse = "|")

          xml_set_attr(feature_node, attr = "type", value = type_to_add)
          xml_set_attr(feature_node, attr = "ref", value = ref_to_add)

        } else {
          # else add a new parent node
          entity_nodes <- nodes[nodes_idx]

          # here, it is possible that some of the children already are <name>
          # nodes. If so, add all nodes with their <name> parent.

          name_nodes_idx <- which(sapply(entity_nodes, function(x) xml_name(xml_parent(x))) == "name")

          original_length <- length(xml_parent(entity_nodes))

          for (node_idx in seq_along(entity_nodes)) {

            # if there is a name node and if there is more than one parent
            # (otherwise, it is possible that all nodes are nested in one single
            # name parent which should be kept).

            if (node_idx %in% name_nodes_idx & original_length > 1L) {
              node_to_add <-  xml_parent(entity_nodes[[node_idx]])
            } else {
              node_to_add <-  entity_nodes[[node_idx]]
            }

            if (node_idx == 1) {
              xml_add_parent(.x = node_to_add, .value = entity_name)
              first_sibling_node <- node_to_add
            } else {
              # check if the nodes are already added due to nested annotations earlier.
              ids_new <- if (length(xml_children(node_to_add)) > 0) {
                xml_attr(xml_children(node_to_add), "id")
              } else {
                xml_attr(node_to_add, "id")
              }
              ids_old <- xml_attr(xml_find_all(
                x = xml_parent(first_sibling_node),
                namespaced_xpath(xml = xml, tags = token_tags)
                ), "id")
              if (!all(ids_new %in% ids_old)) {
                if (length(xml_children(node_to_add)) > 1L) {
                  cli::cli_alert_warning(
                    text = "Pre-Annotated Named Entities and added entities overlap. Adding entire `<name>` node to the new annotation. This can result in inprecise presentation of entity spans."
                  )
                }
                xml_add_sibling(.x = first_sibling_node, .value = node_to_add, .copy  = FALSE)
              }
            }
          }

          # check if there is already a type in the node
          original_type <- xml_attr(x = xml_parent(first_sibling_node), attr = "type")

          if (!is.na(original_type)) {
            type_to_add <- paste0(unique(c(type_to_add, original_type)), collapse = "|")
          }

          xml_set_attr(xml_parent(first_sibling_node), attr = "type", value = type_to_add)
          xml_set_attr(xml_parent(first_sibling_node), attr = "ref", value = ref_to_add)
        }
      }
    }
  }
}



#' function which adds the namespace to the XPATH
#'
#' @param xml xml the namespace is derived from
#' @param tags the tags which should be queried with the XPATH
#' @importFrom xml2 xml_ns
#' @export
namespaced_xpath <- function(xml = xml, tags) {

  xml_namespace <- xml_ns(xml)

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
