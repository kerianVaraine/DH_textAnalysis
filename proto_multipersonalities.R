# Refactoring all of protagonists names into one character
# beckmesser, Dee, Alex

protagonist_name_vector <- c("Beckmesser", "Dee", "Alex")

text_renamed_protagonist <- text_parsed_protagonist_labeled %>%
  mutate(token = replace(token, token %in% protagonist_name_vector, "protagonist"))%>%
  mutate(lemma = replace( lemma, token == "protagonist", "protagonist"))
##########################
# create list of chapters, once Protagonist has been renamed
text_by_chapter <- text_renamed_protagonist %>% group_by(chapter) %>% group_split() %>%   map(., ~ filter(., !lemma %in% stop_words$word, pos != "PUNCT"))

correlation_by_chapter <- text_by_chapter %>% 
  map_depth(.,1,~ pairwise_cor(., lemma, doc_id,  sort=TRUE)) %>%
  map_depth(.,1,~ filter(., item1 %in% character_vector, item2 %in% character_vector))

#create empty list
filtered_correlation_by_chapter_list <- list()
#fill list with null items for the length of chapters in text
desired_length <- length(text_by_chapter)
filtered_correlation_by_chapter_list <- vector(mode = "list", length = desired_length)

######
