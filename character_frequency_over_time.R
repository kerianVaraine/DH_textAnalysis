library(ggplot2)
library(tidyverse)
library(tidytext)

char_occurance_by_chapter <- text_parsed_protagonist_labeled %>%
  group_by(chapter) %>%
  mutate(token=tolower(token))%>%
  count(token, sort = TRUE) %>%
  filter(token %in% character_vector)

# plot character occurance by chapter
# names of characters on Y axis, x axis is count, each graph is a new chapter. or collate into one graph over time
# need to collapse each character in wide format, character | chapter 1 mentions| chapter 2 mentions | etc...

test <- char_occurance_by_chapter %>%
  spread(key = token, value = n, fill = 0)


  

library(ggplot2)
library(dplyr)
library(tidyverse)
library(tidytext)

## create specific order for frequency chart via vector
character_order <- c('protagonist', 'dee','beckmesser', 'alex', 'franky', 'boy', 'rosaria', 'gypsy', 'irene', 'madame','irina', 'candido', 'marco', 'cedric', 'maria', 'eleanor')

char_occurance_by_chapter %>%
  mutate(token = factor(token, levels= character_order)) %>%
  ggplot() +
  geom_path(aes(x=chapter, y = token, size = (n*100))) +
    scale_x_continuous(breaks=seq(1:11)) +
    labs(y = "characters") +
    theme(legend.position = "none")

# that works, now resuse character vector on original text to find them all
# make lower case!

test_char_freq <- text_parsed %>%
  mutate(token=tolower(token))%>%
  filter(!token %in% stop_words$word, pos != "PUNCT") %>%
  group_by(chapter) %>%
  filter(token %in% character_vector) %>%
  count(token, sort = TRUE)

  ggplot(test_char_freq) +
  geom_path(aes(x=chapter, y = token, size = (n))) +
  scale_x_continuous(breaks=seq(1:11)) +
  labs(y = "characters") +
  theme(legend.position = "none")

    
    
    # violinplot attempt, needs more data
     ggplot(test_char_freq, aes(factor = n)) +
       geom_violin(aes(y=chapter, x = token), scale="width") +
       scale_y_continuous(breaks=seq(1:11)) +
       labs(y = "characters") +
       theme(legend.position = "none") +
       coord_flip()
    
     
     
     
     ## Getting how many doc_id (lines) in each chapter, done like this from parsed text.
test_sections <- text_parsed %>%
  mutate(section = row_number()
         %/% length(unique(text_parsed[["doc_id"]])))
     
#######
test_char_freq <- test_sections %>%
  mutate(token=tolower(token))%>%
  filter(!token %in% stop_words$word, pos != "PUNCT") %>%
  group_by(chapter) %>%
  group_by(section) %>%
  filter(token %in% character_vector) %>%
  count(token, sort = TRUE)


ggplot(test_char_freq, aes(factor = n, section)) +
  geom_violin(aes(y=section, x = token), scale= "width" ) +
  # scale_y_continuous(breaks=seq(from = 1, to =11, by = 0.1)) +
  labs(x = "characters") +
  theme(legend.position = "none") +
  coord_flip()
  
     

########
# trying sectioning the text, using the read_lines version, already in sentences, each chapter divided into sections relative to their lengths
# so funny that 2 hours actually produced this next thing, and it still doesnt work right!!!! FUCK

# import text as sentences via readlines
text.original <- as_tibble(read_lines("data/Disappearance - Michael Joyce.txt", skip_empty_rows = TRUE ))
text.original[1805,1] <- "10" # manually change occurance of '\n10' which messed up the chapter count.

# extract all chapter markings and apply them to the lines until the next chapter marking, and have these as a column
text.chapters <- text.original %>%
mutate(linenumber = paste(sep = '', "text", row_number()), chapter = cumsum(str_detect(as.matrix(text.original$value), regex("(0?[1-9]$)|(^1[0-2])", ignore_case = TRUE))))

#########
seq(1:189) %/% ceiling(47.25) ## need to emulate this as we go up chapters

text_chap_sectioned <- text.chapters %>% 
  mutate(section = row_number() %/% 
           ceiling((nrow(filter(text.chapters, chapter %in% .$chapter)))/4)
  )

# test if each chapter actually broken into 1/4
ceiling(nrow(filter(text.chapters, chapter ==1)) / 4) #first find length of chapter, then divide by 4. then something like seq(1:189) %/% ceiling(47.25)

text_chap_sectioned <- text.chapters %>%
  mutate(section = row_number() %/% 
           ceiling((nrow(filter(text.chapters, chapter %in% .$chapter)))/4)
  )



########
# Welcome to 3 hours of work!!!!
# spliting each chapter into sections equal to 1/4 of its length.
# wow.
#######

split_into_sections <- function(text, chap_to_split, no_of_sections) {
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
  sectioning_text_by_chapter[[i]] <- split_into_sections(chapters_list[[i]], i, 7)
}
# Combine
text.chapters.sectioned <- do.call("rbind", sectioning_text_by_chapter)

#############
# YUUUUUUUUUUUUSSSSSSSSSSSS!!!!!!
############

# get character frequency per section now
# now, plotting with more data for the violin chart?
######
test_char_freq <- text.chapters.sectioned %>%
  mutate(token=tolower(token))%>%
  filter(!token %in% stop_words$word, pos != "PUNCT") %>%
  group_by(chapter, section) %>%
  filter(token %in% character_vector) %>%  ## here would be where the character names are combined ??
  count(token, sort = TRUE)
view(test_char_freq)

# not sure if this is getting section numbers
ggplot(test_char_freq, aes(factor = n, section)) +
  geom_violin(aes(y=chapter, x = token), scale= "width" ) +
  scale_y_discrete(breaks=seq(from = 1, to =11, by = 1)) +
  labs(x = "characters") +
  theme(legend.position = "none") +
  coord_flip()


#
# this fills in 0 occurance values for each character.
test <- test_char_freq %>%
  spread(key = token, value = n, fill = 0)

rosaria <- test %>% select(chapter, section, rosaria)
dee <- test %>% select(chapter, section, dee)

# exactly this, but now for each character!
ggplot() +
  geom_area(data = rosaria, aes(x = interaction(chapter, section), y = rosaria, group = 1),colour = "black", fill = "grey") +
  scale_y_continuous(breaks=seq(from = 1, to = 20, by = 1)) +
  annotate(geom = "text", x = seq_len(nrow(rosaria)), y = -0.5, label = rosaria$section, size = 4) +
  annotate(geom = "text", x = -1.5 + 4 * (1:11), y = -1.5, label = unique(rosaria$chapter), size = 5) +
  coord_cartesian(ylim = c(0, 20), expand = FALSE, clip = "off") + 
  theme_bw() +
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())


###
ggplot(test_char_freq, aes(x=interaction(chapter,section), y = token, group = 1)) +
  geom_line() +
  scale_x_discrete(breaks=seq(from = 1, to = 20, by = 1)) +
  coord_cartesian(ylim = c(0, 20), expand = FALSE, clip = "off") + 
  # scale_discrete(breaks=seq(from = 1, to =11, by = 1)) +
  # labs(x = "characters") +
  theme(legend.position = "none")


char_freq_gathered <- test %>% gather(token, n, -c(chapter,section))

# this is almost there!
ggplot() +
  geom_area(data = char_freq_gathered, aes(x = interaction(chapter, section), y = n, group = 1),colour = "black", fill = "grey") +
  scale_y_discrete(breaks=seq(from = 1, to = 40, by = 1)) +
  facet_grid(rows = vars(token))


# create dataframe for geom_text object.
  bottom_labs <- test %>% select(chapter, section)

# now for each character! DONE [X] x axis label not quite right, needs to be clearer.
ggplot() +
  geom_area(data = char_freq_gathered, aes(x = interaction(chapter, section, lex.order = TRUE), y = n, group = 1),colour = "black", fill = "grey") +
  scale_y_discrete(breaks=seq(from = 1, to = 40, by = 1)) +
  ylab(NULL)+
  facet_grid(rows = vars(token)) +
    theme_bw() +
  theme(plot.margin = unit(c(1, 1, 1, 1), "lines"),
        axis.title.x = element_blank(),
        # axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.text.y = element_text(angle = 360)
        ) + # tilt names across 
  coord_cartesian(ylim = c(0, 20), expand = FALSE, clip = "off")   


# get sum of all occurances of each character to only plot the most common with above method.
t_freq <- aggregate(char_freq_gathered$n, by=list(Category=char_freq_gathered$token), FUN=sum) %>% arrange(desc(x)) # get sum of all character occurances to filter

# blend the characters together who share bodies but live in different timelines?, but need to do this individually by section.
# or jump in much earlier and combine them when initially extracting frequency per chapter/section

main_characters <- c('protagonist', 'franky', 'irene', 'rosaria', 'cedric', 'marco')
protagonist_names <- c('dee', 'beckmesser', 'véloce')
irene_names <- c('irene', 'gypsy', 'irina', 'madame')
franky_names <- c('franky', 'boy', 'matador')

t_freq_single_characters <- tibble(
  character = character(),
  n = numeric()
)

t_freq_single_characters <- t_freq_single_characters %>%
  add_row(character = 'protagonist', n = sum(filter(t_freq, Category %in% protagonist_names)$x)) %>%
  add_row(character = 'irene', n = sum(filter(t_freq, Category %in% irene_names)$x)) %>%
  add_row(character = 'franky', n = sum(filter(t_freq, Category %in% franky_names)$x)) %>%
  add_row(character = 'rosaria', n = sum(filter(t_freq, Category %in% "rosaria")$x)) %>%
  add_row(character = 'cedric', n = sum(filter(t_freq, Category %in% 'cedric')$x)) %>%
  add_row(character = 'marco', n = sum(filter(t_freq, Category %in% 'marco')$x))
  
  

################################################################################################
################################################################################################
# Redoing frequ plot, but ordering certain characters together for timeline shifting queues

# This will be the final code for this section, everything above was working towards this!
################################################################################################
################################################################################################

# Sectioning text
split_into_sections <- function(text, chap_to_split, no_of_sections) {
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
  sectioning_text_by_chapter[[i]] <- split_into_sections(chapters_list[[i]], i, 5)
}
# Combine
text.chapters.sectioned <- do.call("rbind", sectioning_text_by_chapter)


# Merge noun phrases parse with sectioned text to get chapter and section id
# this is used to count frequency of "sallow woman", "the boy" properly.
test.noun_phrases.merged <-  text.chapters.sectioned %>% 
  select(doc_id, chapter, section) %>%
  merge(text_nounphrases, by.x = "doc_id") %>%
  unique()

# get frequency of "the boy", "sallow(-faced) woman" from noun phrases parse
test.noun.char.freq.woman <- test.noun_phrases.merged %>%
  group_by(chapter, section) %>%
  filter(str_detect(text, regex("sallow-faced woman|sallow woman", ignore_case = TRUE))) %>%
  filter(root_text == "woman") %>%
  ungroup()%>%
  select(chapter,section,root_text)

test.noun.char.freq.boy <- test.noun_phrases.merged %>%
  group_by(chapter, section) %>%
  filter(str_detect(text, regex("the boy$", ignore_case = TRUE))) %>%
  filter(root_text == "boy") %>%
  ungroup() %>%
  select(chapter,section,root_text)
 
# Bind the two above together, rename column to token, count occurance by section in chapter.
noun.char.freq <- bind_rows(test.noun.char.freq.woman, test.noun.char.freq.boy) %>%
  mutate(token = root_text) %>%
  group_by(chapter, section)%>%
  count(token, sort = TRUE)
    
###############################################
# redoing all freq analysis

test_char_freq <- text.chapters.sectioned %>%
  mutate(token=tolower(token))%>%
  filter(!token %in% stop_words$word, pos != "PUNCT") %>%
  group_by(chapter, section) %>%
  filter(token %in% character_vector) %>%
  count(token, sort = TRUE) %>%
  ungroup() %>%
  # next lines are specific to sections of 10, remove if different section count 
  #add_case(chapter = 10, section = c(3,4), n = 0) %>% # 10 sections: account for unique case of no character mentions in these two sections, (chap 10, section 3 & 4)
  add_case(chapter = 10, section = c(2), n = 0) %>% # 5 sections: account for unique case of no character mentions in these two sections, (chap 10, section 2)
    bind_rows(noun.char.freq)

test <- test_char_freq %>%
  spread(key = token, value = n, fill = 0)

char_freq_gathered <- test %>% gather(token, n, -c(chapter,section))



# The order in which the characters appear in plotting output
character_order <- c('dee','beckmesser', 'franky', 'boy', 'woman', 'rosaria', 'gypsy', 'madame','irina','irene', 'candido', 'marco', 'cedric')
# the x axis chapter delimiters
chapter_ticks <- seq(from= 1, to = (no_of_chapters * sections_per_chapter), by=sections_per_chapter)

# actual plotting of the frequency analysis
char_freq_gathered %>%
  mutate(token = factor(token, levels= character_order)) %>%
  subset(token != is.na(token)) %>%  ## Remove NA, which are the other characters not in the vector
  ggplot() +
  geom_area(aes(x = interaction(chapter, section, lex.order = TRUE), y = n, group = 1),colour = "black", fill = "grey") +
  # scale_y_discrete(breaks=seq(from = 1, to = 10, by = 1)) +
    ylab("Number of mentions")+
    xlab("Chapter split into 10 sections") +
  facet_grid(rows = vars(token)) +
  geom_vline(xintercept= chapter_ticks, size = 0.2) + # lines denoting chapters
  theme_bw() +
  # Aesthetic things, labels, sizing etc...
  theme(plot.margin = unit(c(1, 1, 1, 1), "lines"),
        axis.text.x = element_text(angle = 290, size = 8, hjust = 0),
        axis.text.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.text.y = element_text(angle = 360)
    )
 # coord_cartesian(ylim = c(0, 10), expand = FALSE, clip = "off")   # set y axis limits here!

##########################################
# Stacked graphs for character frequency #
##########################################

# need to pre-group names by token, ie: Dee & Beckmesser together
# create new column in new table indicating group.
char_freq_stacked <- char_freq_gathered
char_freq_stacked$group <- NA # init column

# Go through token column, find the names in the c("",""), and assign the group name to value
char_freq_stacked$group[char_freq_stacked$token %in% c("dee", "beckmesser")] <- "Dee/Beckmesser"
char_freq_stacked$group[char_freq_stacked$token %in% c("franky", "boy")] <- "Franky/Boy"
char_freq_stacked$group[char_freq_stacked$token %in% c("rosaria")] <- "Rosaria"
char_freq_stacked$group[char_freq_stacked$token %in% c("gypsy", "madame", "irina", "irene")] <- "Gypsy/Madame/Irina/Irene"
char_freq_stacked$group[char_freq_stacked$token %in% c("candido")] <- "Candido"
char_freq_stacked$group[char_freq_stacked$token %in% c("marco")] <- "Marco"
char_freq_stacked$group[char_freq_stacked$token %in% c("cedric")] <- "Cedric"



# stacked area plots in grid

char_freq_stacked %>%
  subset(group != is.na(group)) %>%  ## Remove NA, which are the other characters not in the vector
  ggplot(aes(x = interaction(chapter, section, lex.order = TRUE),y=n, fill = token, group = token)) +
  geom_area(position = "identity", alpha = 0.6)+
  facet_grid(rows = vars(group)) +
geom_vline(xintercept= chapter_ticks, size = 0.2) + # lines denoting chapters
  theme_bw() +
  # Aesthetic things, labels, sizing etc...
  theme(plot.margin = unit(c(1, 1, 1, 1), "lines"),
        axis.text.x = element_text(angle = 290, size = 8, hjust = 0),
        axis.text.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.text.y = element_text(angle = 360)
  )

