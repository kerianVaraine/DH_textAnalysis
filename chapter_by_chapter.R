

normalised_correlation_chapters <- text_parsed_charaters_normalised %>% filter(!token %in% stop_words$word, pos != "PUNCT") %>% group_by(chapter) %>% filter(n() >=1) %>% pairwise_cor(token, chapter, sort = TRUE)

# Testing function creation steps...
# filter stop words and punctuation before looping for plots, and group by chapter.
text_parsed_charaters_normalised_no_stop <-   text_parsed_charaters_normalised %>% filter(!token %in% stop_words$word, pos != "PUNCT") %>% group_by(chapter)


text.chapter.1 <- text_parsed_charaters_normalised_no_stop%>%
  filter(chapter == 1) %>%
  filter(n() >=1) %>% 
  pairwise_cor(token, doc_id, sort = TRUE) %>%
  filter(item1 %in% normalised_char_vec & item2 %in% normalised_char_vec)

text.chapter.1 %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()


chapter_char_correlation <- function (character_vector, chapter_no ){
  text_parsed_charaters_normalised_no_stop%>%
    filter(chapter %in% chapter_no) %>%
    filter(n() >=1) %>% 
    pairwise_cor(token, doc_id, sort = TRUE) %>%
    filter(item1 %in% character_vector & item2 %in% character_vector)
}

chapter_char_correlation_plot <- function(correlation_data) {
  correlation_data %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), repel = TRUE) +
    theme_void()
}

# @TODO make these functions pipable __ more research needed when tidying this whole thing up


# reference
# # Split into lists of tibbles for each chapter.
# character_vector_list_chapters <- character_vector_data %>% group_by(chapter) %>% group_split()
# 
# # This filters through all the tibbles contained in the list. Map is the key function here.
# character_vector_list_chapters %>% map(., ~ filter(., lemma %in% "cedric"))
# 
# character_vector_list_chapters %>% map(., ~ count(. ,lemma, sort = TRUE))



# but for now, let's do it lazy like!
c1 <- chapter_char_correlation(normalised_char_vec, 1)
c2 <- chapter_char_correlation(normalised_char_vec, 2)
c3 <- chapter_char_correlation(normalised_char_vec, 3)
c4 <- chapter_char_correlation(normalised_char_vec, 4)
c5 <- chapter_char_correlation(normalised_char_vec, 5)
c6 <- chapter_char_correlation(normalised_char_vec, 6)
c7 <- chapter_char_correlation(normalised_char_vec, 7)
c8 <- chapter_char_correlation(normalised_char_vec, 8)
c9 <- chapter_char_correlation(normalised_char_vec, 9)
c10 <- chapter_char_correlation(normalised_char_vec, 10)
c11 <- chapter_char_correlation(normalised_char_vec, 11)
c12 <- chapter_char_correlation(normalised_char_vec, 12)
c13 <- chapter_char_correlation(normalised_char_vec, 13)
c14 <- chapter_char_correlation(normalised_char_vec, 14)
c15 <- chapter_char_correlation(normalised_char_vec, 15)
c16 <- chapter_char_correlation(normalised_char_vec, 16)
c17 <- chapter_char_correlation(normalised_char_vec, 17)
c18 <- chapter_char_correlation(normalised_char_vec, 18)
c19 <- chapter_char_correlation(normalised_char_vec, 19)
c20 <- chapter_char_correlation(normalised_char_vec, 20)
c21 <- chapter_char_correlation(normalised_char_vec, 21)
c22 <- chapter_char_correlation(normalised_char_vec, 22)
c23 <- chapter_char_correlation(normalised_char_vec, 23)
c24 <- chapter_char_correlation(normalised_char_vec, 24)
c25 <- chapter_char_correlation(normalised_char_vec, 25)

chapter_char_correlation_plot(c1)
chapter_char_correlation_plot(c2)
chapter_char_correlation_plot(c3)
chapter_char_correlation_plot(c4)
chapter_char_correlation_plot(c5)
chapter_char_correlation_plot(c6)
chapter_char_correlation_plot(c7)
chapter_char_correlation_plot(c8)
chapter_char_correlation_plot(c9)
set.seed(123)
chapter_char_correlation_plot(c10)
