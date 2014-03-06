# network diffusion
# Cheng-Jun Wang @ 20140227
# http://igraph.sourceforge.net/doc/R/fastgreedy.community.html

library(igraph)

#################
"Set transmission rate"
#################
transmission_rate = 0.4
coins = c(rep(1, transmission_rate*1000), rep(0,(1-transmission_rate)*1000))
n = length(coins)
# sample(coins, 1, replace=TRUE, prob=rep(1/n, n))
# toss the coins
toss = function(freq) {
  tossing = NULL
  for (i in 1:freq ) tossing[i] = sample(coins, 1, replace=TRUE, prob=rep(1/n, times=n))
  tossing = sum(tossing)
  return (tossing)
}
#################
"Update graphs"
#################
# function for updating the diffusers
update_diffusers = function(diffusers){
  nearest_neighbors = neighborhood(g, 1, diffusers)
  nearest_neighbors = data.frame(table(unlist(nearest_neighbors)))
  nearest_neighbors = subset(nearest_neighbors, !(nearest_neighbors[,1]%in%diffusers))
  keep = unlist(lapply(nearest_neighbors[,2], toss))
  new_infected = as.numeric(as.character(nearest_neighbors[,1][keep >= 1]))
  diffusers = unique(c(diffusers, new_infected))
  return(diffusers)
  }


#################
"generate networks"
#################
# set the node_number of social graph
node_number = 100
library(igraph)

g = graph.tree(node_number, children = 2); graph_name = "Binary Tree network"
g = graph.star(node_number); graph_name = "Star network"
g = graph.full(node_number); graph_name = "Full network"
g = graph.ring(node_number); graph_name = "Ring network"
g = connect.neighborhood(graph.ring(node_number), 2); graph_name = "Two-Neighbors Ring network"

g = erdos.renyi.game(node_number, 0.1) ; graph_name = "Random network"
g = rewire.edges( graph.ring(node_number), prob = 0.8 ) ; graph_name = "Rewired Random network"
g = watts.strogatz.game(1,node_number,3,0.2) ; graph_name = "Small-world network"
g = barabasi.game(node_number) ; graph_name = "Scale-free network"

#################
"initiate the diffusers"
#################

set.seed(20140301); diffusers = sample(V(g),1) ; diffusers
infected =list()
infected[[1]]= diffusers

# set the color
E(g)$color = "grey"
V(g)$color = "white"
set.seed(2014); layout.old = layout.fruchterman.reingold(g, niter = 1000) # about 3 minute3
V(g)$color[V(g)%in%diffusers] = "red"
# plot(g, layout =layout.old)

#################
"Start the contagion!"
#################

total_time = 1
while(length(infected[[total_time]]) < node_number){ 
  infected[[total_time+1]] = sort(update_diffusers(infected[[total_time]]))
  cat(length(infected[[total_time+1]]), "\n")
  total_time = total_time + 1
}

##################
"save as gif animation"
##################

library(animation)

plot_time_series = function(infected, m){
  num_cum = unlist(lapply(1:m, function(x) length(infected[[x]]) ))
  p_cum = num_cum/node_number
  p = diff(c(0, p_cum))
  time = 1:m
  plot(p_cum~time, type = "b", ylab = "CDF", xlab = "Time",
       xlim = c(0,total_time), ylim =c(0,1))
  plot(p~time, type = "h", ylab = "PDF", xlab = "Time",
       xlim = c(0,total_time), ylim =c(0,1), frame.plot = FALSE)
}

plot_time_series(infected, 1)

plot_gif = function(infected){
  m = 1
  while(m <= length(infected)){
    # start the plot
    layout(matrix(c(1, 2, 1, 3), 2,2, byrow = TRUE), widths=c(3,1), heights=c(1, 1))
    V(g)$color = "white"
    V(g)$color[V(g)%in%infected[[m]]] = "red"
    
    plot(g, layout =layout.old, edge.arrow.size=0.2)
    title(paste(graph_name, "\n Transmission Rate =", transmission_rate, ", Day", m))
    plot_time_series(infected, m)
    m = m + 1}
}




saveGIF({
  ani.options(interval = 0.5, convert = shQuote("C:/Program Files/ImageMagick-6.8.8-Q16/convert.exe"))
  # start the plot
  plot_gif(infected)
}, ani.width = 800, ani.height = 500)



