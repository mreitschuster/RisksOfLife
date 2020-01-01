rm(list=ls())

Rgraph=Graph_Build()

# get all active routes between risk and loss
paths<-Graph_getAllPaths(Rgraph, startNode= 'Risk', endNode='Monetary_Loss', onlyActive=TRUE)

# calculate risks for all given paths
path_results=Graph_calculate_paths(paths)

# get paths as strings
names_of_paths=vector(length = length(paths))
for (i in 1:length(paths)){
  names_of_paths[i]=paste(paths[[i]][,1],collapse=' -> ')
  names_of_paths[i]=paste(names_of_paths[i],paths[[i]][nrow(paths[[i]]),3],sep =' -> ')
}

# get static igraph object
Edges=Graph_convert_to_igraph_edges(Rgraph,T)
Edges=Edges[,c(1,3,2,4)]
Nodes=Graph_convert_to_igraph_nodes(Rgraph,T)
# modify the Rgraph object to make it look nice
Nodes=cbind(Nodes,Nodes)
colnames(Nodes)=c('id','label')
colnames(Edges)=c('from','to','label','active')                  

library(visNetwork)
visNetwork(Nodes, Edges)

#shiny::runApp(system.file("shiny", package = "visNetwork"))

#select by sample to select specific paths
#https://datastorm-open.github.io/visNetwork/options.html

#data manipulation!!!

#provide same seed to have same graph
#https://datastorm-open.github.io/visNetwork/layout.html



# convert to igraph object
#g <- graph_from_data_frame(Edges, directed=TRUE, vertices=Nodes)
g <- graph_from_data_frame(Edges, directed=TRUE)
l <- layout_nicely(g)  
l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)