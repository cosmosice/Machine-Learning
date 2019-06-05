library(igraph)
tree <- graph.empty()
node <- vertex(vcount(tree)+1)
node$shape <- 'circle'
node$label <- '1'
tree <- tree+node
curParNode <- V(tree)[vcount(tree)]
plot(tree,vertex.label=V(tree)$lbl,edge.label=E(tree)$lbl,vertex.size=40,layout=layout_as_tree)

node2 <- vertex(vcount(tree)+1)
node2$shape <- 'circle'
node2$label <- '2'
tree <- tree+node2
eg <- edge(curParNode,V(tree)[vcount(tree)])
eg$lbl <- '1.2'
tree <- tree+eg
plot(tree,vertex.label=V(tree)$label,edge.label=E(tree)$lbl,vertex.size=80,layout=layout_as_tree)
