library(data.table)
library(ggplot2)
library(igraph)
library(GGally)
library(modMax)

data <- fread("C:/Users/Ildiko/Documents/Python/Network project/restaurant_network_name.csv",
              sep=';', encoding='UTF-8')

data_net <- data[data$Weight != 0]
data_net <- data_net[sample(nrow(data_net), 10000)]

rest_net = graph_from_data_frame(data_net[,-3], directed = F, vertices = NULL)
# E(rest_net)$weight <- data[,3]

plot.igraph(rest_net)
plot(rest_net, vertex.size=10, vertex.label=NA)


deg <- degree(rest_net, mode="all")
deg.dist <- degree_distribution(rest_net, cumulative=T, mode="all")

plot( x=0:max(deg), y=1-deg.dist, pch=19, cex=1.2, col="orange", 
      
      xlab="Degree", ylab="Cumulative Frequency")


# Modularity
# 
# adj <- get.adjacency(rest_net)
# modul <- localModularityWang(adj)

ceb <- cluster_edge_betweenness(rest_net, weights = data_net$Weight)

plot(ceb, rest_net, vertex.label=NA, vertex.size=0.01)