text_parsed_protagonist_labeled <- text_parsed %>% mutate_cond((doc_id %in% test3[,1] & sentence_id %in% test3[,2] & token_id %in% test3[,3]), token = replace(token, token == "I", "PROTAGONIST"))

# Attempt to rename all characters to one name each::
text_parsed_charaters_normalised <- text_parsed_protagonist_labeled


#create character vectors for each different way a character is addressed
# this is done in spacyRTest.r for now, but will be moved

text_parsed_charaters_normalised <- text_parsed_charaters_normalised %>%
  mutate_cond(token %in% beck_vec, token = "Beckmesser") %>% 
  mutate_cond(token %in% dee_vec, token = "M.Dee") %>%
  mutate_cond(token %in% frankie_vec, token = "Frankie") %>%
  mutate_cond(token %in% marco_vec, token = "Marco") %>%
  mutate_cond(token %in% irene_vec, token = "Irene")



#BACKUP FUNCTION
#normalised_correlation <- text_parsed_charaters_normalised %>% filter(!token %in% stop_words$word, pos != "PUNCT") %>% group_by(token) %>% filter(n() >=1) %>% pairwise_cor(token, chapter, sort = TRUE)


# Correlation by chapter
normalised_correlation_chapters <- text_parsed_charaters_normalised %>% filter(!token %in% stop_words$word, pos != "PUNCT") %>% group_by(chapter) %>% filter(n() >=1) %>% pairwise_cor(token, chapter, sort = TRUE)

normalised_char_vec <- c("Marco", "Irene", "Beckmesser", "Frankie", "M.Dee", "PROTAGONIST", "Rosaria", "Cedric" )

normalised_character_correlations <-  normalised_correlation_chapters %>%
  filter(item1 %in% normalised_char_vec & item2 %in% normalised_char_vec)

### TESTING COR GRAPHING
library(ggplot2)
library(igraph)
library(ggraph)
set.seed(321)
normalised_character_correlations %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

#######################
# Goto chapter_by_chapter
# Where we will plot correlations between characters over the entire book, chapter by chapter
# Maybe do sentiment analysis too?
#######################
