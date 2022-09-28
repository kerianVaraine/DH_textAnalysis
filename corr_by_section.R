# Correlation by chapter/section

# This should be in correlation scripts./...
corr.sectioned.list <- text.chapters.sectioned %>% group_by(chapter) %>% group_split() %>% map(., ~ filter(., !lemma %in% stop_words$word, pos != "PUNCT"))


# checks correlation if in the same section and chapter sometimes spans more than one sentance?
corr.sectioned <- corr.sectioned.list %>% 
  map_depth(.,1,~ pairwise_cor(., lemma, feature = section,  sort=TRUE)) %>%
  map_depth(.,1,~ filter(., item1 %in% character_vector, item2 %in% character_vector))

# need to remove duplicates....
# correlation_character_filtered <- correlation_character_filtered[!duplicated(t(apply(correlation_character_filtered, 1, sort))),]

#create empty list
corr.filtered <- list()
#fill list with null items for the length of chapters in text
desired_length <- length(text_by_chapter)
corr.filtered <- vector(mode = "list", length = desired_length)

## Loops through list of character correlations by chapters, and filters duplicates.
# holy shit that was difficult
for(i in 1:desired_length) {
  corr.filt.temp <-corr.sectioned[[i]]
  corr.filt.temp <- corr.filt.temp[!duplicated(t(apply(corr.filt.temp, 1, sort))),]
  corr.filtered[[i]] <- corr.filt.temp
}

#########################################################################################
# plotting social network with sna library                                              #
# following::                                                                           #
# https://programminghistorian.org/en/lessons/temporal-network-analysis-with-r          #
#########################################################################################

