########
# Topic modeling through whole text.
########

# Spilt text into sections and chapters, this should be done in init script!!!!
# TODO init script add this in.

split_into_quarters <- function(text, chap_to_split, no_of_sections) {
  sections_per_chapter <<- no_of_sections
  text %>%
    filter(chapter == chap_to_split) %>%
    mutate(section = row_number() %/%
             ceiling(nrow(text) / no_of_sections) + 1
    )
}
# split
chapters_list <- split(text_parsed, text_parsed$chapter) # with parsed text.
# chapters_list <- split(text.chapters, text.chapters$chapter) # read by lines text
# create list to populate
sectioning_text_by_chapter <- list()
sectioning_text_by_chapter <- vector(mode = "list", length = no_of_chapters)

# apply function to split
for(i in 1:no_of_chapters){
  sectioning_text_by_chapter[[i]] <- split_into_quarters(chapters_list[[i]], i, 4)
}
# Combine
text.chapters.sectioned <- do.call("rbind", sectioning_text_by_chapter)


##########
# topic modeling
# https://www.youtube.com/watch?v=evTuL-RcRpc
##########
# remove stopwords

no.stop.tidy <- text.chapters.sectioned %>%
  mutate(token=tolower(token))%>%
  filter(!token %in% stop_words$word, pos != "PUNCT", token != '’s', token !='’ll', token != 'n’t')

  no.stop.tidy %>% count(token, sort=TRUE)
  
  #tf-idf -- what is that?
t_tfidf <- no.stop.tidy %>%
  count(chapter, token, sort = TRUE) %>%
  bind_tf_idf(token, chapter, n) %>%
  group_by(chapter) %>%
  top_n(10) %>%
  ungroup %>%
  mutate(token = reorder(token, tf_idf))

t_tfidf %>%
  ggplot(aes(token, tf_idf, fill = chapter)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(t_tfidf$chapter, scales = 'free') +
  coord_flip()

# topic modeling
library(stm)
library(quanteda)  

t_dfm <- no.stop.tidy %>%
  count(chapter, token, sort = TRUE) %>%
  cast_dfm(chapter, token, n)

## stm topic model. learn about this? unsupervised machine learning.
# how many topics are there in the text? which words contribute to which topics, which topics contribute to each chapter.
topic_model <- stm(t_dfm, K = 6, init.type = "Spectral")
summary(topic_model)

# tidy the topic_model
td_beta <- tidy(topic_model)

plot <- td_beta %>% group_by(topic) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(term = reorder(term, beta))

  plot %>%
  ggplot(aes(term, beta, fill = topic)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(plot$topic, scales = 'free') +
  coord_flip()

  td_gamma <- tidy(topic_model, matrix = "gamma", document_names = rownames(t_dfm))
  
  ggplot(td_gamma, aes(gamma, fill = as.factor(topic))) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(td_gamma$topic, ncol = 3)

  


