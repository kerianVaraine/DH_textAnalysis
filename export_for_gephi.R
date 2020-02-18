# export csvs

write_csv(correlation_by_chapter[[1]], path = "data/chap1.csv", col_names = FALSE)

correlation_by_chapter[[1]]

correlation_by_chapter[[1]]

## create edge and node List for gephi

net <- filtered_correlation_by_chapter_list[[1]]
net_plot <- graph_from_data_frame(net, directed = FALSE, vertices = character_vector)

edgelist <- as_tibble(get.edgelist(net_plot)) %>% rename(Source = V1, Target = V2)

write_csv(edgelist, path = "data/chap1edge.csv", col_names = TRUE)
as_adj_edge_list(net_plot)
# 
# ########### Wednesday -- 6 nov
# net <- filtered_correlation_by_chapter_list[[1]]
# net_plot <- graph_from_data_frame(net, directed = FALSE, vertices = as_tibble(character_vector))
# vcount(net_plot)
# ecount(net_plot)
# 
# #from https://www.r-bloggers.com/network-visualization-part-2-gephi/
# ############################################################################################
# library(plyr)
# # Calculate some node properties and node similarities that will be used to illustrate 
# # different plotting abilities
# 
# # Calculate degree for all nodes
# degAll <- degree(net_plot, v = V(net_plot), mode = "all")
# view(degAll)
# 
# # Calculate betweenness for all nodes
# betAll <- betweenness(net_plot, v = V(net_plot), directed = FALSE) / (((vcount(net_plot) - 1) * (vcount(net_plot)-2)) / 2)
# betAll.norm <- (betAll - min(betAll))/(max(betAll) - min(betAll))
# rm(betAll)
# 
# # Calculate Dice similarities between all pairs of nodes
# dsAll <- similarity.dice(net_plot, vids = V(net_plot), mode = "all")
# 
# ############################################################################################
# # Add new node/edge attributes based on the calculated node properties/similarities
# 
# net_plot <- set.vertex.attribute(net_plot, "degree", index = V(net_plot), value = degAll)
# net_plot <- set.vertex.attribute(net_plot, "betweenness", index = V(net_plot), value = betAll.norm)
# 
# # Check the attributes
# summary(net_plot)
# 
# # F1 <- function(x) {data.frame(V4 = dsAll[which(V(net_plot)$name == as.character(x$V1)), which(V(net_plot)$name == as.character(x$V2))])}
# # dataSet.ext <- ddply(net, .variables=c("V1", "V2", "V3"), function(x) data.frame(F1(x)))
# 
# 
# dataSet.ext <- rename(net, replace= c('item1' = "V1", 'item2' = "V2", 'correlation' = "V3"))
# 
# net_plot <- set.edge.attribute(net_plot, "weight", index = E(net_plot), value = 0)
# net_plot <- set.edge.attribute(net_plot, "similarity", index = E(net_plot), value = 0)
# 
# # The order of interactions in net_plot is not the same as it is in dataSet or as it is in the edge list,
# # and for that reason these values cannot be assigned directly
# 
# E(net_plot)[as.character(dataSet.ext$V1) %--% as.character(dataSet.ext$V2)]$weight <- as.numeric(dataSet.ext$V3)
# E(net_plot)[as.character(dataSet.ext$V1) %--% as.character(dataSet.ext$V2)]$similarity <- as.numeric(dataSet.ext$V4)
# 
# # End
# #######



#########This gets very basic nodes and edgelists into gephi to work with. Attributes would be good to add....
### attempting to get to gephi now as original ( the last thing didn't achieve much)
net <- filtered_correlation_by_chapter_list[[1]]
net_plot <- graph_from_data_frame(net, directed = FALSE, vertices = as_tibble(character_vector))
vcount(net_plot)
ecount(net_plot)

# no weight in this one
# edgelist <- as_tibble(get.edgelist(net_plot)) %>% rename(Source = V1, Target = V2)
  
# get from, to, and correlation
edgelist <- filtered_correlation_by_chapter_list[[1]] %>% rename(Source = item1, Target = item2,  Weight = correlation)
edgelist$Weight

write_csv(edgelist, path = "data/chap1edge.csv", col_names = TRUE)

# extract nodes from net_plot and save as csv
nodes <- tibble(Id = V(net_plot)$name, Label = V(net_plot)$name)
write_csv(nodes, path = "data/chap1nodes.csv", col_names = TRUE)

# Looping to get all csv's of chapters individually for gephi, including time interval
# adapting network plotting functions

#make this a function to apply to lists

G_plot_correlation_from_chapters <- function(section) {
  # net <- filtered_correlation_by_chapter_list[[section]] %>% rename(Source = item1, Target = item2,  Weight = correlation)
  net <- correlation_by_chapter[[section]] %>% rename(Source = item1, Target = item2,  Weight = correlation)
  net_plot <- graph_from_data_frame(net, directed = FALSE, vertices = as_tibble(character_vector))
  plot(net_plot, layout = l, edge.width = net$correlation *10, vertex.shape = 'none')
}

## adding time interval column
edgelist <- filtered_correlation_by_chapter_list[[1]] %>% rename(Source = item1, Target = item2,  Weight = correlation) %>% add_column("Interval" = 1)
write_csv(edgelist, path = "data/chap1edge.csv", col_names = TRUE)

# save svg of plot, calls plot correlation from chapters function defined above.
G_save_csv <- function(section){
  edgelist <- filtered_correlation_by_chapter_list[[section]] %>% rename(Source = item1, Target = item2,  Weight = correlation) %>% add_column("Interval" = section)
  file_name <- paste(sep="", "data/chap", section, "edge.csv")
  write_csv(edgelist, path = file_name, col_names = TRUE)
}

for(i in 1:desired_length) {
  G_save_csv(i)
}
