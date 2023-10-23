library(NLP)
library(openNLP)

if (!"openNLPmodels.en" %in% rownames(installed.packages()))
  install.packages("openNLPmodels.en", repos = "http://datacube.wu.ac.at")

s <- system.file("texts", "crude", "reut-00002.xml", package = "tm") |>
  xml2::read_xml() |>
  xml2::xml_find_first(xpath = "//BODY") |>
  xml2::xml_text() |>
  String()

pipe <- Annotator_Pipeline(
  openNLP::Maxent_Sent_Token_Annotator(),
  openNLP::Maxent_Word_Token_Annotator(),
  openNLP::Maxent_Entity_Annotator(kind = "organization"),
  openNLP::Maxent_Entity_Annotator(kind = "person"),
  openNLP::Maxent_Entity_Annotator(kind = "location")
)

doc <- AnnotatedPlainTextDocument(
  s = s,
  a = annotate(s, pipe)
)
