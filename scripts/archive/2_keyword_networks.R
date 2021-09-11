# keyword cooccurance network

# functions --------------------------------------------------------------------

# generate keyword network
# n_word - number of keywords to plot
# biblioDF cleaned (using fun_tidy_multi_wos_bibs()) bibliometric data
fun_keyword_network <- function(biblioDF, n_word) {
  
  dat1 <- biblioDF %>%
    select(ID, AuthorKeywords)
  
  e <- dat1$AuthorKeywords %>%
    str_split(";") %>%
    lapply(function(x) {
      x1 <- str_trim(x, side = "both")
      x2 <- x1[x1 != ""] 
      expand.grid(x2, x2, w = 1 / length(x2), stringsAsFactors = FALSE)
    }) %>%
    bind_rows
  
  # lemmatize 
  e$Var1 <- tolower(e$Var1)
  e$Var2 <- tolower(e$Var2)
  e$Var1 <- SnowballC::wordStem(e$Var1, language = "porter")
  e$Var2 <- SnowballC::wordStem(e$Var2, language = "porter")

  e2 <- apply(e[, -3], 1, str_sort) %>%
    t %>%
    data.frame(stringsAsFactors = FALSE) %>%
    mutate(w = e$w) %>%
    na.omit()
  
  e2 <- e2 %>%
    group_by(X1, X2) %>%
    summarise(w = sum(w)) %>%
    filter(X1 != X2) %>%
    arrange(-w)
  
  # only 50 most common terms
  e3 <- e2[1:n_word, ]
  
  # convert to graph
  e4 <- igraph::graph_from_data_frame(e3, directed = FALSE)
  igraph::E(e4)$weight <- e3$w
  
  n <- network::network(e3[, -3], directed = FALSE)
  network::set.edge.attribute(n, "weight", e3$w)
  
  # weighted degree
  t <- network::as.edgelist(n, attrname = "weight") %>%
    bipartite::symmetrise_w() %>%
    bipartite::as.tnet() %>%
    tnet::degree_w()
  
  # tidy and return
  network::set.vertex.attribute(n, "degree_w", t[, "output" ])
  l <- network::network.vertex.names(n)
  network::set.vertex.attribute(n, "label", l)
  return(n)
}

# Take a network::network,convert to igraph, cluster, add clusters to igraph,
# convert to tidygraph, plot
fun_cluster <- function(net, ttl) {
  
  #
  n.igr <- intergraph::asIgraph(net)
  louv <- igraph::cluster_louvain(n.igr)
  
  IDs_cluster <- cbind(igraph::V(n.igr)$name, louv$membership)
  n.igr <- igraph::set_vertex_attr(n.igr, name = "community", value = IDs_cluster)
  n.igr <- igraph::set_vertex_attr(n.igr, name = "size", value = igraph::degree(n.igr))
  
  n.gg <- n.igr %>%
    tidygraph::as_tbl_graph() 
  
  ggraph::ggraph(graph = n.gg, layout = 'igraph', algorithm = "fr")  +
    ggraph::geom_edge_arc(strength = 0.1, alpha = 0.3) +
    ggraph::geom_node_point(ggplot2::aes(col = factor(community), size = size)) +
    ggraph::scale_edge_width(range = c(0.5, 5)) +
    ggraph::geom_node_text(ggplot2::aes(label = label), repel = TRUE) +
    ggplot2::labs(edge_width = "Links") +
    ggplot2::ggtitle(ttl) +
    ggraph::theme_graph() +
    ggplot2::theme(legend.position = 'none',
                   plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))
  
}
# run functions ----------------------------------------------------------------


# required packages
library(tidyverse)
# library(tidygraph)
#library(intergraph)
# library(network)
# library(tnet)
# library(bipartite)
# library(bibliometrix)

dat <- readRDS("data/biblio_tidy.rds")


# seperate df and db objects
biblioDB <- dat[[1]]
biblioDF <- dat[[2]]

# drop double ;;
biblioDF$AuthorKeywords <- gsub(";;", ";", biblioDF$AuthorKeywords)

# KEYWORD COOCURENCE NETWORK ---------------------------------------------------

# Generate a network::network - can ignore warning
db_all <- fun_keyword_network(biblioDF, n_word = 50)

# pull relevant info, add decade column
biblioDF2 <- biblioDF %>%
  filter(!is.na(YearPublished)) %>%
  select(YearPublished, ID, AuthorKeywords) %>%
  mutate(decade = cut(YearPublished, 
                      breaks = c(1985, 1999, 2009, 2022), 
                      labels = c("1985-1999","2000-2009", "2010-2021")))


# generate networks for separate decades 
biblioDF3 <- split(biblioDF2, biblioDF2$decade)
keyword_networks <- lapply(biblioDF3, fun_keyword_network, n_word = 50)

decade_88_99 <- keyword_networks[[1]]
decade_00_09 <- keyword_networks[[2]]
decade_10_21 <- keyword_networks[[3]]


# plot
library(ggnetwork)
library(patchwork)
library(ggraph)

p1 <- fun_cluster(decade_88_99, ttl = "Decade: 90's")
p2 <- fun_cluster(decade_00_09, ttl = "Decade: 00's")
p3 <- fun_cluster(decade_10_21, ttl = "Decade: 10's")

p1 / p2 / p3

# save
ggsave("figures/keyword_cooc.png", width = 15, height = 45, units = "cm")

