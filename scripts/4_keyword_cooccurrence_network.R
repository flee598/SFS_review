# keyword cooccurance network

# functions --------------------------------------------------------------------

# generate keyword network
# n_word - number of keywords to plot
# biblioDF cleaned (using fun_tidy_multi_wos_bibs()) bibliometric data


db <- df_key_net
n_word <- 50

fun_keyword_network <- function(db, n_word) {
  
  dat1 <- db %>%
    select(entry_number, authkeywords)
  
  e <- dat1$authkeywords %>%
    str_split(";") %>%
    lapply(function(x) {
      x1 <- str_trim(x, side = "both")
      x2 <- x1[x1 != ""] 
      expand.grid(x2, x2, w = 1, stringsAsFactors = FALSE)
    }) %>%
    bind_rows
  
  # stem
  e <- e %>%
  mutate(Var1 = tm::stemDocument(e$Var1),
         Var2 = tm::stemDocument(e$Var2))
  
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
fun_graph <- function(net, ttl){ 
  
  n.igr <- intergraph::asIgraph(net)
  louv <- igraph::cluster_louvain(n.igr)
  
  IDs_cluster <- cbind(igraph::V(n.igr)$name, louv$membership)
  
  n.igr <- igraph::set_vertex_attr(n.igr, name = "community", value = IDs_cluster)
  n.igr <- igraph::set_vertex_attr(n.igr, name = "size", value = igraph::degree(n.igr))
  
  lay = create_layout(n.igr, layout = "fr")
  
  ggraph(lay) + 
    geom_edge_link(edge_colour = "grey") + 
    geom_node_point(aes(size = size, colour = as.factor(community)), shape = 16, alpha = 0.9) +
    geom_node_text(aes(label = label), repel = TRUE) +
    ggplot2::ggtitle(ttl) +
    theme_graph(base_family = "sans") +
    theme(
      legend.position = "none",
      plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))
}
# run functions ----------------------------------------------------------------

# required packages
# library(tidygraph)
#library(intergraph)
# library(network)
# library(tnet)
# library(bipartite)
library(tidyverse)
library(ggnetwork)
library(patchwork)
library(ggraph)


# get data from google sheets
df_meta <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/13OtfyWIqS8XKhuBXzboJIyLzFFUp702ESCmk91Q204Q/edit#gid=1641925563")
df_auth <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/13OtfyWIqS8XKhuBXzboJIyLzFFUp702ESCmk91Q204Q/edit#gid=1222222462")

# drop missing stuff
df_key_net <- df_meta |>
  filter(!is.na(authkeywords)) |>
  filter(!is.na(year_date))

# KEYWORD COOCURENCE NETWORK ---------------------------------------------------

# Generate a network::network - can ignore warning
db_all <- fun_keyword_network(df_key_net, n_word = 100)

# pull relevant info, add decade column
biblioDF2 <- df_key_net %>%
  filter(!is.na(year_date)) %>%
  select(entry_number, year_date, authkeywords) %>%
  mutate(decade = cut(year_date, 
                      breaks = c(1980, 1989, 1999, 2009, 2022), 
                      labels = c("1982-1989","1990-1999", "2000-2009", "2010-2021")))



# generate networks for separate decades 
biblioDF3 <- split(biblioDF2, biblioDF2$decade)

# drop 80's as no keywords available
biblioDF3 <- biblioDF3[2:4]

# generate networks
keyword_networks <- lapply(biblioDF3, fun_keyword_network, n_word = 100)

decade_90_99 <- keyword_networks[[1]]
decade_00_09 <- keyword_networks[[2]]
decade_10_21 <- keyword_networks[[3]]

#plot
p0 <- fun_graph(db_all, ttl = "keywords: all data")
p1 <- fun_graph(decade_90_99, ttl = "keywords: 1990-1999")
p2 <- fun_graph(decade_00_09, ttl = "keywords: 2000-2009")
p3 <- fun_graph(decade_10_21, ttl = "keywords: 2010-2021")

# save
ggsave("./figures/keyword_cooc_all.pdf", plot = p0, width = 25, height = 25, units = "cm")
ggsave("./figures/keyword_cooc_90s.pdf", plot = p1, width = 25, height = 25, units = "cm")
ggsave("./figures/keyword_cooc_00s.pdf", plot = p2, width = 25, height = 25, units = "cm")
ggsave("./figures/keyword_cooc_10s.pdf", plot = p3, width = 25, height = 25, units = "cm")










