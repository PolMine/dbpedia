#' Add hyperlinks to XML/HTML document.
#' 
#' @param x Input HTML object
#' @param url A named list.
#' @importFrom htmltools HTML
#' @importFrom xml2 read_html xml_find_all xml_add_child xml_text xml_name
#'   xml_attrs
#' @export
href <- function(x, url = list()){
  
  doc <- xml2::read_html(x)
  for (color in names(url)){
    nodes <- xml2::xml_find_all(
      doc,
      xpath = sprintf('//span[@style="background-color:%s"]', color)
    )
    lapply(
      nodes,
      function(node){
        # turn node to a child of itself
        xml2::xml_add_child(.x = node, .value = node, copy = TRUE)
        
        # turn node into link
        xml2::xml_text(node) <- ""
        xml2::xml_name(node) <- "a"
        xml2::xml_attrs(node) <- c(href = url[[color]])
        
        return( NULL )
      }
    )
  }
  
  ret <- htmltools::HTML(as.character(doc))
  attr(ret, "browsable_html") <- TRUE
  ret
}
