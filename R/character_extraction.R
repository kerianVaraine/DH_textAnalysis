# Create character vector
# Extract PROPN from entire text 
# When this becomes a function, parameter should be a vector of words to remove from the final outcome, (that final filter)
character_vector_data <- text_parsed_protagonist_labeled %>% 
  filter(!token %in% stop_words$word, pos != "PUNCT") %>% filter(pos == "PROPN", lemma != "’s", lemma != "¿", lemma != "mr.")
#character_vector_data <- text_renamed_protagonist %>% filter(!token %in% stop_words$word, pos != "PUNCT") %>% filter(pos == "PROPN", lemma != "’s", lemma != "¿", lemma != "mr.")

# Not protagonist
character_vector_data <- text_parsed %>% 
  filter(!token %in% stop_words$word, pos != "PUNCT") %>% 
  filter(pos == "PROPN", lemma != "’s", lemma != "¿", lemma != "mr.")

# instances of 'the boy'
text_nounphrases <- spacy_extract_nounphrases(source_text)

characters_filtered.boy <- text_nounphrases %>% 
  filter(str_detect(text, regex("the boy$", ignore_case = TRUE))) %>% 
  count(root_text)

#rosaria before name known
characters_filtered.sallow_woman <- text_nounphrases %>%
  filter(str_detect(text, regex("sallow-faced woman|sallow woman", ignore_case = TRUE))) %>%
  filter(root_text == "woman") %>%
  count(root_text)


# instances of 'gypsy'
characters_filtered.gypsy <- text_parsed %>% 
  filter(token == "gypsy") %>% 
  count(token)

characters_filtered.matador<- text_parsed %>% 
  filter(token == "matador") %>% 
  count(token)

# calculate frequency, and leave only those mentioned 3 or more times.
character_vector_frequency <- character_vector_data %>% 
  count(lemma) %>%
  add_row(lemma = characters_filtered.gypsy$token, n = characters_filtered.gypsy$n) %>% 
  add_row(lemma = characters_filtered.boy$root_text, n = characters_filtered.boy$n) %>%
  add_row(lemma = characters_filtered.matador$token, n = characters_filtered.matador$n) %>%
  arrange(desc(n)) %>%
  filter(n >= 3)
  

# need to manually weed out the terms not suitable
# use subset() ie: subset(data, ! lemma == "spanish" && lemma == "day")  ## ! is important.
manual_filter_list <- c('boy', 'ynys', 'gutrin', 'bête', 'angel', 'boss','day', 'el', 'heaven', 'jesus','kinswoman', 'lo', 'belle', 'damn', 'don', 'egyptian', 'la', 'lake' , 'spanish', 'monsieur', 'elena')

character_vector_frequency <- character_vector_frequency %>% 
  subset(! lemma %in% manual_filter_list)

# Pull column from frequency into out character vector to use
character_vector <- character_vector_frequency %>% pull(lemma)


##################
# End of character extraction method
##################
