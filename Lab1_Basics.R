
# install.packages("statnet")

library(statnet)

#change working directory
setwd("E:/Teaching/Emory/TADA/2022_S/Labs")

### Import data
# If no header, set "header = F". 
data <- read.csv("Manager_friends.csv", header = T)

## Import attributes.
att <- read.csv("Manager_att.csv", header = T)

### Create a network object
# If the network is undirected, set "directed = F". 
Fnet <- network(data, directed = T)

### Plot the network
plot.network(Fnet, displaylabels = T, boxed.labels = FALSE, vertex.cex = 0.6, label.cex = 1, 
             vertex.col = att$dept, label.col = "blue", edge.lwd = .5, edge.col = "gray50")

# Interactive plotting
plot.network(Fnet, displaylabels = T, boxed.labels = FALSE, vertex.cex = 0.6, label.cex = 1, 
             vertex.col = att$dept, label.col = "blue", edge.lwd = .5, edge.col = "gray50", interactive = T)

# Save the graph to your computer
png("Manager_friends.png", width = 1600, height = 1600, res=35)
plot.network(Fnet, displaylabels = T, boxed.labels = FALSE, vertex.cex = 0.6, label.cex = 5, 
             vertex.col = att$dept, label.col = "blue", edge.lwd = .5, edge.col = "gray50")
dev.off()

### Node level
degree <- degree(Fnet)

ind <- degree(Fnet, cmode = "indegree")

out <- degree(Fnet, cmode = "outdegree")

# How close an actor is to everyone else (sort of the average shortest paths)
# One ego has short paths to all other vertices in the graph
closeness <- closeness(Fnet, cmode="suminvdir")

# Number of shortest paths passing through the egos (bridges)
betweenness <- betweenness(Fnet)

# The extent to which an ego is connected to other important nodes
# eigenvector centrality
ecent <- evcent(Fnet)

tab1 <- cbind(degree, closeness, betweenness, ecent)

write.csv(tab1, file = "tab1.csv")

### Dyad level
dyad.census(Fnet)

# reciprocity. It counts null ties as mutual ties
reciprocity <- grecip(Fnet)
(24+174)/(24+174+12)

# Ignoring null ties in calculating reciprocity. Mutual ties count as twice.
grecip(Fnet, measure = "edgewise")
24*2/(24*2+12)

# Geodistance
geodist <- geodist(Fnet)

# number of paths
geodist$counts 

### Group level
# Within a clique, everyone is a friend with one another.
clique <- clique.census(Fnet)

tab2 <- clique$clique.count[,1]

write.csv(tab2, file = "tab2.csv")

# Components are the largest inter-connected subnetworks.
# Four components include one node, respectively; one component includes 17 nodes
component.dist(Fnet)

### Network level
# density
density <- gden(Fnet)

# centralization, between zero and one.
# The higher the number is, the more inequality in centrality.
cent <- centralization(Fnet, degree)

# transitivity. Proportion of transitive triads out of possible transitive triads.
# Divide the number of {AB, BC, AC} triads by the number of {AB, BC, anything} triads.
transitivity <- gtrans(Fnet)

