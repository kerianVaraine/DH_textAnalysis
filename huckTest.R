############# Huck Finn example
source_text_file = here("data/huck.txt")
source_text = read_lines(source_text_file, skip_empty_rows = TRUE)
### manual analysis and preprosessing happens here or in text editor.
### reload text if edited in editor
# huck finn example, change all CHAPTER occurances to numbers
m <- regexpr("CHAPTER \\w+.", source_text)
regmatches(source_text,m) <- 1:length(m)
# single occurance of chapter naming
source_text[9113] <- 43
#########################################


# parse text with spacyr, 
text_parsed <- spacy_parse(source_text, entity = TRUE)
# and add chapter column, turns to DATAFRAME!!!!!!
text_parsed_df <- text_parsed %>% mutate(chapter = cumsum(str_detect(as.matrix(text_parsed$token), regex("[\\d]", ignore_case = TRUE))))
#docid = line number  | sentence id = sentance within docid | tokenId = index in line | token = single word or punct | 

# Get number of chapters (not perfect for huck finn example)
no_of_chapters <- length(unique(text_parsed[["chapter"]]))


character_vector_data <- source_text %>% spacy_extract_entity()

character_vector_frequency <- character_vector_data %>%
  filter(ent_type == "PERSON") %>%
  count(text) %>%
  arrange(desc(n))

character_vector_frequency <-  character_vector_frequency[-1,] # removes first instance of ""
view(character_vector_frequency)

character_vector <- tolower(pull(character_vector_frequency, text))


library(widyr)
text_parsed_df <- text_parsed_df %>% filter(!token %in% stop_words$word, pos != "PUNCT") %>% mutate(token = tolower(token))


correlation_all_text <- text_parsed_df %>%
  group_by(token) %>%
  filter(n() >= 2) %>%
  pairwise_cor(token, chapter, sort = TRUE)

correlation_character_filtered <- correlation_all_text %>%
  filter(item1 %in% character_vector, item2 %in% character_vector)

correlation_character_filtered <- correlation_character_filtered[!duplicated(t(apply(correlation_character_filtered, 1, sort))),]


# plotting network?
library(ggplot2)
library(igraph)
library(ggraph)

net <- correlation_character_filtered
net_plot <- graph_from_data_frame(net, directed = FALSE)
plot(net_plot, edge.width = net_plot[3], edge.curve = TRUE)
clique_num(net_plot)

# circle plot
l <- layout_in_circle(net_plot)
plot(net_plot, layout=l, edge.width = net_plot[3]*5)

# 
l <- layout_with_fr(net_plot)
plot(net_plot, layout=l, edge.width = net_plot[3])

l <- layout_randomly(net_plot)

plot(net_plot, layout=l, edge.width = net_plot[3], vertex.shape = "none", edge.curved = 0)
