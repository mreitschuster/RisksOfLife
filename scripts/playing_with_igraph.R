#playing with igraph
#http://kateto.net/networks-r-igraph


library(igraph) # Load the igraph package

g1 <- graph( edges=c(1,2, 2,3, 3, 1), n=3, directed=F ) 
plot(g1) # A simple plot of the network - we'll talk more about plots later
class(g1)
g1


g2 <- graph( edges=c(1,2, 2,3, 3, 1), n=10 )
plot(g2)   
g2


g3 <- graph( c("John", "Jim", "Jim", "Jill", "Jill", "John")) # named vertices
# When the edge list has vertex names, the number of nodes is not needed
plot(g3)

g4 <- graph( c("John", "Jim", "Jim", "Jack", "Jim", "Jack", "John", "John"), 
                          isolates=c("Jesse", "Janis", "Jennifer", "Justin") )  
# In named graphs we can specify isolates by providing a list of their names.
plot(g4, edge.arrow.size=.5, vertex.color="gold", vertex.size=15, 
          vertex.frame.color="gray", vertex.label.color="black", 
          vertex.label.cex=0.8, vertex.label.dist=2, edge.curved=0.2) 

plot(graph_from_literal(a---b, b---c)) # the number of dashes doesn't matter
plot(graph_from_literal(a--+b, b+--c))


plot(graph_from_literal(a+-+b, b+-+c)) 

