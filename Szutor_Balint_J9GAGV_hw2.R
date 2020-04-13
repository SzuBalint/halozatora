# load packages

library(igraph)
library(sand)
library(igraphdata)
library(data.table)

# --- 1 --- 

# Create empty graph, then look for edges

new_graph = graph.empty(n=11, directed = F)

for (i in c(2:12)){
  for (h in c(2:12)) {
    if(i %% h == 0 & i != h) {
      
      print(c(i, h, "Edge found"))
      
      new_graph <- add_edges(new_graph, c(i-1,h-1), directed = F) %>%
        set_edge_attr("color", value = "red")
      
    } else {
      print(c(i, h, "No Edge found"))
    }
  }
}

# Rename nodes
V(new_graph)$name <- c(2:12)


# a.
# vertices:
V(new_graph)

# edges
E(new_graph)

# plot
plot(new_graph)


# c.
# Number of components
components(new_graph)$no


# d.
# Largest connceted component's diameter
diameter(new_graph, unconnected = TRUE)


# --- 2 ---

# load data

data(karate)
plot(karate)

# checking whether our data is directed or weighted

is.directed(karate) #FALSE
is.weighted(karate) #TRUE

# Number of nodes

vcount(karate) #34

# Number of edges 

ecount(karate) #78

# Average degree

mean(degree(karate)) #4.59

# Diameter

is.connected(karate) #TRUE

# Connected so we simply use the diameter function

diameter(karate) #13


# --- 3 ---
# Same graph as we used in --- 1 ---

transitivity(new_graph, "local")

between <-betweenness(new_graph, v = V(new_graph))
between["6"] # 3.67


# --- 4 ---

C_elegans <- fread("data/C_elegans.csv", sep = ";")

head(C_elegans)

elegans <- graph_from_data_frame(C_elegans, directed = TRUE, vertices = NULL)

mean_L <- mean_distance(elegans)
clus_coef <- mean(transitivity(elegans), na.rm = T)

erdos <- erdos.renyi.game(length(V(elegans)), length(E(elegans)), type = c("gnm"), directed = TRUE, loops = FALSE)
barabasi <- barabasi.game(length(V(elegans)))
small_world <- sample_smallworld(1, length(V(elegans)), mean(distances(elegans)), 0.05)

mean_L / mean_distance(erdos)
mean_L / mean_distance(barabasi)
mean_L / mean_distance(small_world)

mean_L / mean(transitivity(erdos), na.rm = T)
mean_L / mean(transitivity(barabasi), na.rm = T) # Not scale free
mean_L / mean(transitivity(small_world), na.rm = T)

# From the website we can also see the following:
# Average degree: 764/131 = 5.8
# In case of Erdős-Rényi avarage degree / number of nodes should be equal to mean clustering coefficient

mean(degree(elegans)) / length(V(elegans))

# This is much smaller, therefore we conclude that this should be closest to a Small world model.

# --- 5 ---

# Create graph:

graph_5 <- make_empty_graph(n = 6, directed = FALSE)

graph_5 <- add_edges(graph_5, c(1,2, 1,4, 1,5, 1,6, 2,3))

# Probability of attachment

attachm_prob <- function(graph){
  
  n_nodes = vcount(graph)
  probs = c(1:n_nodes)
  
  for (i in c(1:n_nodes)){
    probs[i] <- degree(graph)[i] / sum(degree(graph))
  }
  
  return(probs)
}

# Interval creation

cr_interval <- function(graph,probs){
  
  n_nodes = vcount(graph)
  interval = c(1:n_nodes)
  
  for (i in c(1:n_nodes)){
    interval[i] = sum(probs[1:i])
  }
  
  return(interval)
}

# Then we add the new nodes

cr_barabasi_nodes <- function(graph, n){
  
  for (i in c(1:n)){
    
    # Call previous functions
    
    probs <- attachm_prob(graph)
    interval <- cr_interval(graph, probs)
    
    # Storing parameters
    
    v_num <- vcount(graph)
    rand <- runif(1)
    node_to_connect = 1
    
    for (bound in interval){
      if  (rand > bound){
        node_to_connect <- node_to_connect + 1
      } else {
        break
      }
    }
    
    # Adding new features
    
    graph <- add_vertices(graph, 1)
    graph <- add_edges(graph, c(node_to_connect,(v_num + 1)))
  }
  
  return(graph)
}

# a.

attachm_prob(graph_5)

# b.

cr_barabasi_nodes(graph_5,1)

# c.

cr_barabasi_nodes(graph_5,2)

# d.
# Adjacency matrix

get.adjacency(graph_5)



