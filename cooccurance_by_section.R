# Calculating coocurrance for network plotting
# NOT CORRELATION!!

# does a word from the character vector appear in the same section as another word from character vector? If so, add 1
library(widyr)

# This should be in correlation scripts./...
cooc.sectioned.list <- text.chapters.sectioned %>% group_by(chapter) %>% group_split() %>% map(., ~ filter(., !lemma %in% stop_words$word, pos != "PUNCT"))


# checks correlation if in the same section and chapter sometimes spans more than one sentance?
cooc.sectioned <- cooc.sectioned.list %>% 
  map_depth(.,1,~ pairwise_count(., lemma, feature = section,  sort=TRUE)) %>%
  map_depth(.,1,~ filter(., item1 %in% character_vector, item2 %in% character_vector))

#create empty list
cooc.filtered <- list()
#fill list with null items for the length of chapters in text
desired_length <- length(text_by_chapter)
cooc.filtered <- vector(mode = "list", length = desired_length)

## Loops through list of character correlations by chapters, and filters duplicates.
# holy shit that was difficult
for(i in 1:desired_length) {
  cooc.filt.temp <-cooc.sectioned[[i]]
  cooc.filt.temp <- cooc.filt.temp[!duplicated(t(apply(cooc.filt.temp, 1, sort))),]
  cooc.filtered[[i]] <- cooc.filt.temp
}

# single plot to test
net <- cooc.filtered[[1]]
net_plot <- graph_from_data_frame(net, directed = FALSE)
plot(net_plot, edge.width = net$n)
clique_num(net_plot)




## PLOTTING

plot_cooccurance_from_chapters <- function(section) {
  # net <- filtered_correlation_by_chapter_list[[section]]
  net <- cooc.filtered[[section]]
  net_plot <- graph_from_data_frame(net, directed = FALSE) #vertices = as_tibble(character_vector)
  plot(net_plot, layout = l, edge.width = net$n, vertex.shape = 'none')
}

# save svg of plot, calls plot correlation from chapters function defined above.
save_svg <- function(section){
  svg(filename=paste(sep = '',plot_dir, '/chap_cooc/', section, '.svg'), 
      width=10, 
      height=10, 
      pointsize=12)
  plot_cooccurance_from_chapters(section)
  dev.off()
}


##########
# trying all in loop, works until null points in chapter 13

# set  layout to be the same for each graph
l <- layout_with_fr(net_plot)
l <- layout_with_lgl(net_plot)
l <- layout_nicely(net_plot)
l <- layout_in_circle(net_plot)
#scaling try?
l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)


# test layout before full export
l <- layout_with_kk(net_plot)

plot_cooccurance_from_chapters(2)

for(i in 1:desired_length) {
  plot_cooccurance_from_chapters(i)
  # save_svg(i)
}

#######






############This is producing very good results!
###### Quanteda Method straigh up Frequency corrolation matrix
library(tidyverse)
library(spacyr)
spacy_initialize()

# super naive window method
text_parsed.naive <- spacy_parse(source_text, pos=FALSE, tag=FALSE)
    
    
# quanteda count frequency    
quant.corr.naive.count <- as.tokens(text_parsed.naive, remove_punct = TRUE) %>%  
  tokens_keep(pattern = character_vector) %>%
  fcm(context = "window", count = "frequency", window = 10)

# quanteda weighted
quant.corr.weighted <- as.tokens(text_parsed.naive, remove_punct = TRUE) %>%  
  tokens_keep(pattern = character_vector) %>%
  fcm(context = "window", count = "weighted", window = 10, ordered = TRUE)

## plotting

set.seed(110)
#count based
feat <- names(topfeatures(quant.corr.naive.count, 50))
fcm_select(quant.corr.naive.count, pattern = feat) %>%
  textplot_network(min_freq =2)

# weighted plot
feat <- names(topfeatures(quant.corr.weighted, 50))
fcm_select(quant.corr.weighted, pattern = feat) %>%
  textplot_network(min_freq = 0.2)



########################
# Split source text into chapters,

## Need to create this as a function and apply to all chapters as individual units.

# one chapter test
chap1 <- as.tokens(as.list(text.chapters.sectioned %>% subset(chapter == 1) %>% select(token), remove_punct = TRUE))
  
quant.corr.chap.1.count <- chap1 %>% 
  tokens_keep(pattern = character_vector) %>%
  fcm(context = "document", count = "frequency")
#### 1 chapter test end. works.
# two chapter test, for merge...
chap2 <- as.tokens(as.list(text.chapters.sectioned %>% subset(chapter == 2) %>% select(token), remove_punct = TRUE))

quant.corr.chap.2.count <- chap2 %>% 
  tokens_keep(pattern = character_vector) %>%
  fcm(context = "document", count = "frequency")

# merge corrs together
q.corr.merge_test <- cbind(as_tibble(quant.corr.chap.1.count), as_tibble(quant.corr.chap.2.count))

########
# [split into chapters]

q.chaptered.list <- text.chapters.sectioned %>%
  group_by(chapter) %>%
  group_split() %>%
  map_depth(., 1, ~ select(., token)) %>% # select only tokens once split into chapter lists
  map_depth(.,1,~ as.tokens(as.list(., remove_punct = TRUE)))

# recreating filtered list from earlier
# create empty list
q.corr.chaps <- list()
# go through and create fcm for each chapter
for(i in 1:desired_length) {
  q.filt.temp <- q.chaptered.list[[i]] %>%
    tokens_keep(pattern = character_vector) %>%
    fcm(context = "document", count = "frequency")
  q.corr.chaps[[i]] <- q.filt.temp
}

# TRYING TO LOOP SAVING OF JPEG, BUT NOT WORKING FSR?

for(i in 1:desired_length) {
  jpeg(paste(plot_dir, '/chap_q_cooc/chap',i,'.jpg',sep = ''))
  fcm_select(q.corr.chaps[[i]], pattern = feat) %>%
    textplot_network(min_freq =10)
  dev.off()
  }


# THIS NEEDS TO BE LOOPED FOR FUCKS SAKE
i <-  1
i = i+1
jpeg(paste(plot_dir, '/chap_q_cooc/chap',i,'.jpg',sep = ''))
fcm_select(q.corr.chaps[[i]], pattern = feat) %>%
  textplot_network(min_freq =10)
dev.off() 


#################################################
# Memory needle as focal point in relationships #
# Two plots, One for Dee, one for Beckmesser    #
#################################################
# using the chapter separated token lists from above.
##################

# create fcm for before memory needle (BMN)
# This aint it. only stores last one called by i

# create one document for BMN & AMN, and get window size from average length of chapters sectioned equally...
# based of : ceiling(nrow(text) / no_of_sections) + 1 :: used in character frequency calcs.
# therefore...


# convert all alters of Irene in text to Irene. should this be done for all alters?
# to achieve this, we need to reimport entire text as a single character vector, find all instances of the names,
# replace them, then parse with spacyR
text.cvec <- paste(read_lines(source_text_file, skip_empty_rows = TRUE), collapse = " ")
text.cvec.alters <- text.cvec %>%
  str_replace_all(c("Irina" ="Irene", "gypsy" = "Irene", "Madame" = "Irene"))

text.cvec.alters.parsed <- spacy_parse(text.cvec.alters, pos=FALSE, tag=FALSE)


# select only text before chapter 5, using sentance_id, manually looking up when chapter 5 begins.
BMN_text <- text.cvec.alters.parsed %>%
  subset(sentence_id < 2490)



AMN_text <- text.cvec.alters.parsed %>%
  subset(sentence_id >= 2490)

# Usind window size, count weighted frequency, make fcm

# BMN weighted fcm - for network plot
BMN_text.window <- ceiling(nrow(BMN_text) / 4 / 100)

BMN.weightedFCM <- as.tokens(BMN_text, remove_punct = TRUE) %>%  
  tokens_keep(pattern = character_vector) %>%
  fcm(context = "window", count = "weight", window = 1)

textplot_network(BMN.weightedFCM, min_freq = 0.5)

library(igraph)
bmn_adj <- graph_from_adjacency_matrix(BMN.weightedFCM,  weighted = TRUE)
bmn_adj <- simplify(bmn_adj)
bmn_filename <- paste(plot_dir, '/graphData/bmn.gml',sep = '')

write_graph(bmn_adj, "bmn_graph.gml", format = "gml")




# AMN weighted fcm
AMN_text.window <- 70

AMN.weightedFCM <- as.tokens(AMN_text, remove_punct = TRUE) %>%  
  tokens_keep(pattern = character_vector) %>%
  fcm(context = "window", count = "weight", window = 1)

textplot_network(AMN.weightedFCM,
                 min_freq = 6,
                 edge_alpha = 0.2)

amn_adj <- graph_from_adjacency_matrix(AMN.weightedFCM,  weighted = TRUE)
amn_adj <- simplify(amn_adj)
amn_filename <- paste(plot_dir, '/graphData/amn.gml',sep = '')

write_graph(amn_adj, "amn_graph.gml", format = "gml")




# create fcm for after memory needle (AMN)
for(i in 5:11) {
  q.filt.temp <- q.chaptered.list[[i]] %>%
    tokens_keep(pattern = character_vector) %>%
    fcm(context = "document", count = "frequency")
  q.corr.AMN <- q.filt.temp
}

q.corr.BMN %>% textplot_network()
q.corr.AMN %>% textplot_network()



