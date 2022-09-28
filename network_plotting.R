library(ggplot2)
library(igraph)
library(ggraph)

#Creating network dataset, ie: nodes & edgelists
# nodes for us are just the characters? they are the sources
# edgelist is the correlation chart. to: from: correlation/weight

## BLARGHGHGHG  
# c_vec_nodelist <- tibble(id = 1:length(character_vector))
# c_vec_nodelist$name <- character_vector
# network <- graph_from_data_frame(correlation_character_filtered, directed = TRUE, vertices = c_vec_nodelist )
#BLARGHGHGHGHGH
##
# Working from..
### https://www.r-bloggers.com/interactive-network-visualization-with-r/
nodes <- as.data.frame(character_vector)


##___

# Remove loops
network <- simplify(network, remove.multiple = F, remove.loops = T) 

# experimenting
correlation_character_filtered %>%
  graph_from_data_frame() %>%
  ggraph(layout = ) +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()


###### SUNDAYS ATTEMPT, following linkden courses

net <- correlation_character_filtered
net_plot <- graph_from_data_frame(net, directed = FALSE)
plot(net_plot, edge.width = net_plot[3], edge.curve = TRUE)
clique_num(net_plot)

# circle plot
l <- layout_in_circle(net_plot)
plot(net_plot, layout=l, edge.width = net_plot[3])

# 
l <- layout_with_fr(net_plot)
plot(net_plot, layout=l, edge.width = net_plot[3])


##### MONDAY -- 1 hour struggle with display. sheesh

# attempt to plot for each chapter, disregarding the wierd stuff that happens in chap 10-15

# using map to work with lists: ::
# character_vector_list_chapters <- character_vector_data %>% group_by(chapter) %>% group_split()
# character_vector_list_chapters <- character_vector_list_chapters %>% map(., ~ filter(., lemma %in% character_vector))

## This should be in correlation scripts./...
text_by_chapter <- text_parsed_protagonist_labeled %>% group_by(chapter) %>% group_split() %>%   map(., ~ filter(., !lemma %in% stop_words$word, pos != "PUNCT"))

      
# checks correlation if in the same doc_id. sometimes spans more than one sentance?
correlation_by_chapter <- text_by_chapter %>% 
  map_depth(.,1,~ pairwise_cor(., lemma, doc_id,  sort=TRUE)) %>%
  map_depth(.,1,~ filter(., item1 %in% character_vector, item2 %in% character_vector))

# need to remove duplicates....
# correlation_character_filtered <- correlation_character_filtered[!duplicated(t(apply(correlation_character_filtered, 1, sort))),]

#create empty list
filtered_correlation_by_chapter_list <- list()
#fill list with null items for the length of chapters in text
desired_length <- length(text_by_chapter)
filtered_correlation_by_chapter_list <- vector(mode = "list", length = desired_length)

## Loops through list of character correlations by chapters, and filters duplicates.
# holy shit that was difficult
for(i in 1:desired_length) {
filtered_correlation_by_chapter <- correlation_by_chapter[[i]]
filtered_correlation_by_chapter <- filtered_correlation_by_chapter[!duplicated(t(apply(filtered_correlation_by_chapter, 1, sort))),]
filtered_correlation_by_chapter_list[[i]] <- filtered_correlation_by_chapter
}

# now that we have chapter by chapter correlation, plot these out as network analysis plots?
# do we use map then facet?

#single plot
net <- filtered_correlation_by_chapter_list[[1]]
net_plot <- graph_from_data_frame(net, directed = FALSE)
plot(net_plot, edge.width = net$correlation * 10)
clique_num(net_plot)

# testing save svg function



#make this a function to apply to lists

plot_correlation_from_chapters <- function(section) {
  # net <- filtered_correlation_by_chapter_list[[section]]
  net <- correlation_by_chapter[[section]]
  net_plot <- graph_from_data_frame(net, directed = FALSE, vertices = as_tibble(character_vector))
  plot(net_plot, layout = l, edge.width = net$correlation *10, vertex.shape = 'none')
}

# save svg of plot, calls plot correlation from chapters function defined above.
save_svg <- function(section){
svg(filename=paste(sep = '',plot_dir, '/chap_cor/', section, '.svg'), 
    width=10, 
    height=10, 
    pointsize=12)
plot_correlation_from_chapters(section)
dev.off()
}


##########
# trying all in loop, works until null points in chapter 13

# set  layout to be the same for each graph
l <- layout_with_fr(net_plot)
l <- layout_with_lgl(net_plot)
l <- layout_nicely(net_plot)
#scaling try?
l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)


# test layout before full export
l <- layout_with_kk(net_plot)

net <- filtered_correlation_by_chapter_list[[1]]
net_plot <- graph_from_data_frame(net, directed = FALSE, vertices = as_tibble(character_vector))
plot(net_plot, layout = l * 0.5, rescale =FALSE, edge.width = net$correlation *10, vertex.shape = 'none')

for(i in 1:desired_length) {
  save_svg(i)
}

#######