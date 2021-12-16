library(tidyverse)
library(covidHubUtils)
library(igraph)
library(Matrix)
library(here)

# G <- lex_sats$G[[3]]
# Gnew <- Gclus(G, vsks$cluster)
# map_int(1:2000, ~sum(Gnew[.,]!= G[.,]))

Gclus <- function(G, clus) {
  for (i in 1:nrow(G)) {
    G[i,which(clus!=clus[i])] <- 0 
  }
  return(G)
}

get_sp_coefs <- function(g, dim = 50, 
                         clusdim1 = floor(dim/3), 
                         clusdim2 = dim,
                         lap_emb = NULL,
                         adj_emb = NULL) {
  if (is.null(lap_emb)) {
    lap_emb <- embed_laplacian_matrix(g, no = dim, type = ifelse(is.directed(g), "OAP", "DAD"))
  }
  if (is.null(adj_emb)) {
    adj_emb <- embed_adjacency_matrix(g, no = dim)
  }
  I <- matrix(0, gorder(g), gorder(g), dimnames = list(V(g)$name, V(g)$name))
  lap_f1 <- lap_f2 <- adj_f1 <- adj_f2 <- I
  lap_f1[,] <- lap_emb$Y[,1]%*%t(lap_emb$X[,1])
  lap_f2[,] <- lap_emb$Y[,2]%*%t(lap_emb$X[,2])
  adj_f1[,] <- adj_emb$Y[,1]%*%t(adj_emb$X[,1])
  adj_f2[,] <- adj_emb$Y[,2]%*%t(adj_emb$X[,2])
  ones <- I
  ones[,] <- 1
  lap_kmns1 <- kcca(lap_emb$X, k = clusdim1, control=list(initcent="kmeanspp"))
  lap_kmns2 <- kcca(lap_emb$X, k = clusdim2, control=list(initcent="kmeanspp"))
  adj_kmns1 <- kcca(adj_emb$X, k = clusdim1, control=list(initcent="kmeanspp"))
  adj_kmns2 <- kcca(adj_emb$X, k = clusdim2, control=list(initcent="kmeanspp"))
  lap_clus1 <- Gclus(ones, clus = lap_kmns1@cluster)
  lap_clus2 <- Gclus(ones, clus = lap_kmns2@cluster)
  adj_clus1 <- Gclus(ones, clus = adj_kmns2@cluster)
  adj_clus2 <- Gclus(ones, clus = adj_kmns2@cluster)
  return(list(
    lap_emb = lap_emb,
    adj_emb = adj_emb,
    lap_f1 = lap_f1,
    lap_f2 = lap_f2,
    adj_f1 = adj_f1,
    adj_f2 = adj_f2,
    lap_kmns1 = lap_kmns1,
    lap_kmns2 = lap_kmns2,
    adj_kmns1 = adj_kmns1,
    adj_kmns2 = adj_kmns2,
    lap_clus1 = lap_clus1,
    lap_clus2 = lap_clus2,
    adj_clus1 = adj_clus1,
    adj_clus2 = adj_clus2
  ))
}
