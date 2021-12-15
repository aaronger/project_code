
Gclus <- function(G, clus) {
  for (i in 1:nrow(G)) {
    G[i,which(v!=v[i])] <- 0 
  }
  return(G)
}

G <- lex_sats$G[[3]]
Gnew <- Gclus(G, vsks$cluster)
map_int(1:2000, ~sum(Gnew[.,]!= G[.,]))
