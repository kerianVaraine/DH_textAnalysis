library(widyr)
# # all text correlation
# correlation_all_text <- text_parsed_protagonist_labeled %>% 
#   filter(!token %in% stop_words$word, pos != "PUNCT") %>%
#   pairwise_cor(token, chapter, sort = TRUE) 
# 
# # filter just character vector from correlation list
# correlation_filtered_char_vector <- correlation_all_text %>%
#   filter(item1 %in% character_vector)
# 
# # plot of most correlated words to do with characters
# correlation_all_text %>%
#   filter(item1 %in% character_vector) %>%
#   group_by(item1) %>%
#   top_n(10) %>%
#   ungroup() %>%
#   mutate(item2 = reorder(item2, correlation)) %>%
#   ggplot(aes(item2, correlation)) +
#   geom_bar(stat = "identity") +
#   facet_wrap(~ item1, scales = "free") +
#   coord_flip()

##########
# I've been doion this wrong.
# Now properly following https://www.tidytextmining.com/ngrams.html#counting-and-correlating-pairs-of-words-with-the-widyr-package
#########
# remember to convert all tokens to lower case for filtering!!!!!
text_no_stops <- text_parsed_protagonist_labeled %>%
  filter(! token %in% stop_words$word, pos != "PUNCT") %>%
  mutate(token = tolower(token))

##### Revisit now that there are sections
correlation_all_text <- text.chapters.sectioned %>%
  group_by(chapter, section) %>%
  filter(n() >= 2) %>%
  pairwise_cor(token, doc_id, sort = TRUE)

# filter character correlations, minus boy & sallow(faced) woman
correlation_character_filtered <- correlation_all_text %>%
  filter(item1 %in% character_vector, item2 %in% character_vector)

correlation_character_filtered <- correlation_character_filtered[!duplicated(t(apply(correlation_character_filtered, 1, sort))),]

