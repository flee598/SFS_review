# Function to take a vector of shared strings (e.g. from biblio data)
# and then extract a frequency table, co-occurence network and a plot

# https://www.r-bloggers.com/2016/09/turning-keywords-into-a-co-occurrence-network/

# op <- text.network(full_with_excl$keywords, cluster.alg = "label_prop", min_wgt = 0.99)
rm.dup <- function(x)
{

  fStr <- stringr::str_split(x, ", ", simplify = TRUE)
  dStr <- fStr[!stringi::stri_duplicated(fStr)]
  
  dStr <- paste(dStr, collapse = ", ")
  dStr <- stringr::str_remove(dStr, "\\b, NA\\b|\\bNA, \\b")
  dStr
}

# countries <- read_csv("countries.csv")  # from google with a few additions

# Read list of countries of the world and flatten to a single string
# Generalised below so just send a vector of text and a string to match
# https://developers.google.com/public-data/docs/canonical/countries_csv
parse.countries <- function(countries, bibData)
{  
  # countries <- read_csv('countries.csv')  # from google with a few additions
  countries.flat <- toupper(stringr::str_c(countries$name, collapse = "|"))

#  Make a data-frame from biblioData with the countries matched for each paper
#  Assume countries in <<affiliation>> column
  
  ctry.au <- bibData %>%
    mutate(id = row_number()) %>%
    tidyr::separate_rows(AuthorAddress, sep = ';\\s*') %>%
    group_by(id) %>%
   summarise(match = toString(str_extract(toupper(AuthorAddress), countries.flat)), 
              aff = toString(AuthorAddress))

# Remove duplicates with function from above and make a vector
  ctry.au.nd <- sapply(ctry.au$match, FUN = rm.dup)
  ctry.au$countries <- stack(ctry.au.nd)$values
}  

text.network <- function(char_vec, cluster.alg = "louvain", min_wgt = 0)
{  
  # NB min_wgt here is a percentile of the degree_w attached to each  (so 0.5 = median)
  # nwk_data is a character vector of combinations
  library(tidyverse)
  library(igraph)
  library(network)
  library(tnet)
  library(tidygraph)
  library(ggraph)
  
  # nwk_data <- full_with_excl$keywords[1:50]
    char_vec <- str_replace_all(char_vec, pattern = fixed(", "), replacement = ",")

    nwk_data <- data.frame(txt = tolower(str_trim(char_vec))) #%>%
     # filter(txt != "NA" & txt != "na") # remove NAs
  

  # Get the frequency table for each individual string
  nwk_index <- nwk_data$txt %>%
    str_split(",|;") %>%
    unlist %>%
    table %>%
    data.frame %>%
    arrange(-Freq)
  
  names(nwk_index)[1] = "txt"
  
  # Edges between the strings weighted by occurrence  
  edges <- nwk_data$txt %>%
    str_split(",|;") %>%
    lapply(function(x) {
      expand.grid(x, x, w = 1 / length(x), stringsAsFactors = FALSE)
    }) %>%
    bind_rows()
  
  # Sort so that X1 lower alphabetical than X2 for undirected network ij <-> ji
  edges <- apply(edges[, -3], 1, str_sort) %>%
    t() %>%
    data.frame() %>%
    mutate(w = edges$w) %>%
    group_by(X1) %>%        ## code here to get frequencies in case we need to filter
    mutate(n1 = n()) %>%
    group_by(X2) %>%
    mutate(n2 = n())
  
  edges_clean <- edges %>%
    group_by(X1, X2) %>%    # count of X1, X2
    summarise(w = sum(w)) %>%
    filter(X1 != X2) # lose selfs
  # the weights here are $W_{X1, X2} = \sum{1 / N}$, where the $1 / N$ fraction designates the value that we previously established as the weighting factor of a single edge, or tie between keywords $X1$ and $X2$.  -> Norman-Fowler weights
  
  # see: https://f.briatte.org/r/weighting-authorship-networks
  
  
  # build an undirected network
  n <- network::network(edges_clean[, -3], directed = FALSE)
  
  stopifnot(nrow(edges_clean) == network.edgecount(n))  # sanity!
  set.edge.attribute(n, "weight", edges_clean$w)   # set edge attributes
  
  # weighted degree at alpha = 1
  t <- as.edgelist(n, attrname = "weight") %>%
    symmetrise_w() %>%
    as.tnet() %>%
    degree_w()  # http://toreopsahl.com/2008/11/28/network-weighted-network/
  
  stopifnot(nrow(t) == network.size(n))
  set.vertex.attribute(n, "degree_w", t[, "output" ])
  
  # show only strings at or above median weighted degree
  l <- n %v% "degree_w"    # set/get from network
  l <- ifelse(l >= quantile(l, min_wgt), network.vertex.names(n), NA)
  
  stopifnot(length(l) == network.size(n))
  set.vertex.attribute(n, "label", l)
  
  # At this point we have the network!
  n.igraph <- intergraph::asIgraph(n)   # convert to igraph
  
  cat(paste("Clustering with algorithm ", cluster.alg, "\n"))
  if (cluster.alg == "louvain")
  {  
    cooc.cluster <- cluster_louvain(n.igraph)
  }
  
  if (cluster.alg == "walktrap")
  {  
    cooc.cluster <- cluster_walktrap(n.igraph)
  }  
  if (cluster.alg == "label_prop")
  {
    cooc.cluster <- cluster_label_prop(n.igraph)
  }
  
  IDs_cluster <- cbind(V(n.igraph)$name, cooc.cluster$membership)
  
  
  n.igraph <- set_vertex_attr(n.igraph, name = "community", value = IDs_cluster)
  n.igraph <- set_vertex_attr(n.igraph, name = "size", value = degree(n.igraph))
  
  list(freq_table = nwk_index,
  edges = edges_clean,
       network_igraph = n.igraph,
       network_comm = cooc.cluster)
}

# gg <- plot.text.network(op$network_igraph, op$network_louvain)

plot.text.network <- function(nwk, show = FALSE)
{  
  # takes an igraph ob
  library(tidygraph)
  library(ggraph)
  
  n.gg <- nwk %>%
    as_tbl_graph()    # to todygraph for plotting
  
  cooc.gg <- ggraph(graph = n.gg, layout= 'igraph', algorithm = "fr")  +
    geom_edge_arc(strength = 0.1, alpha = 0.3) + 
    geom_node_point(aes(col = factor(community), size = size * 3)) +
    scale_edge_width(range = c(0.5, 5)) +
    geom_node_text(aes(label = label), repel = TRUE) +
    labs(edge_width = "Links") +
    theme_graph() +
    theme(legend.position = 'none')
  
  if (show == TRUE) {cooc.gg}
  list(network_gg = cooc.gg)
}


###############

# This is code to plot a spatial network (global) on to a map of the world (so nodes have geographic coords)
# Assume that we have an igraph network with country names [nwk], coordinates for each node [node_xy]
# nwk and node_xy need a shared field for joins (e.g. countries and vertex.names)
# n_label is how many countries to label (top n based on degree)

# Inspired by # https://www.r-bloggers.com/2018/05/three-ways-of-visualizing-a-graph-on-a-map/


# map.no.authors <- function(freq_table)
map.network <- function(nwk, node_xy, n_label = 15){
  
  
  require(tidyverse)
  require(assertthat)
  require(igraph)
  require(tidygraph)
  require(ggraph)
  require(ggrepel)
  
  # Make tibbles from nodes and edges for manipulation and plotting
  plot_edges <- nwk %>%
    as_tbl_graph() %>%
    activate(edges) %>%
    as_tibble()
  
  plot_nodes <- nwk %>%
    as_tbl_graph() %>%
    activate(nodes) %>%
    as_tibble()
  
  # case consistency
  node_xy$name.l <- tolower(node_xy$name)
  
  # Wrangle the edges and nodes for plotting (joining to get lat and lon for nodes | edges, etc.)
  plot_nodes <- plot_nodes %>%
    inner_join(node_xy, by = c('vertex.names' = 'name.l')) %>%
    rename(lon = longitude, lat = latitude) %>%
    mutate(id = row_number())
  
  plot_edges <- plot_edges %>%
    inner_join(plot_nodes %>% select(id, lon, lat), by = c('from' = 'id')) %>%
    rename(x = lon, y = lat) %>%
    inner_join(plot_nodes %>% select(id, lon, lat), by = c('to' = 'id')) %>%
    rename(xend = lon, yend = lat)
  
  # Set up mapping theme
  maptheme <- theme(panel.grid = element_blank()) +
    theme(axis.text = element_blank()) +
    theme(axis.ticks = element_blank()) +
    theme(axis.title = element_blank()) +
    theme(legend.position = "bottom") +
    theme(panel.grid = element_blank()) +
    theme(panel.background = element_rect(fill = "#596673")) +
    theme(plot.margin = unit(c(0, 0, 0.5, 0), 'cm'))
  
  country_shapes <- geom_polygon(aes(x = long, y = lat, group = group),
                                 data = map_data('world'),
                                 fill = "#CECECE", color = "#515151",
                                 size = 0.15)
  
  mapcoords <- coord_fixed(xlim = c(-150, 180), ylim = c(-55, 80))
  
  nodes2plot <- plot_nodes
  edges_for_plot <- plot_edges
  
  nodes_lbl <- nodes2plot %>% 
    slice_max(degree_w, n = n_label, with_ties = FALSE) %>%
    mutate(vertex.names = str_to_title(vertex.names)) %>%
    select(lat, lon, vertex.names)
  
  
  nwk_map.gg <- ggplot(nodes2plot) + 
    country_shapes +
    geom_curve(aes(x = x, y = y, xend = xend, yend = yend, size = weight * 3, alpha = weight),
               data = edges_for_plot, curvature = 0.33) +
    scale_size_continuous(guide = FALSE, range = c(0.25, 2)) + # scale for edge widths
    geom_point(aes(x = lon, y = lat, size = degree_w),                          # draw nodes
               shape = 21, fill = 'white',
               color = 'black', stroke = 0.5) +
    geom_text_repel(data = nodes_lbl, aes(x = lon, y = lat, label = vertex.names),             # draw text labels
                    size = 3, color = "white", fontface = "bold") +
    guides(alpha = 'none') +
    mapcoords +
    maptheme
  
  nwk_map.gg
}    


# Function to take a count of some sort (e.g. papers with au form countries)
# and map it; use the naturalearth data but other options...
# map_scale (small, medium, large) gives resolution of underlying maps
# high (large) res may require extra data to be downloaded

# map.no.authors(cnt.nwk$freq_table)
map.no.authors <- function(freq_table, map_scale = "large")
{
# freq_table <- cnt.nwk$freq_table

  require(rnaturalearth)
  require(sf)
  world <- ne_countries(returnclass="sf", scale = map_scale)

  # Makign a few assumptions through here re column names...
  ft <- freq_table %>%
    mutate(txt = str_to_title(txt))

  world <- world %>%
    mutate(name = str_to_title(name))
  
  # Join freq data with the world data and drop columns
  world.ft <- world %>%
    left_join(ft, by = c("name" = "txt")) %>%
    select(name, Freq, geometry)


  freq_map_gg <-  ggplot(world.ft) +
    geom_sf(aes(fill = sqrt(Freq)), lwd = 0.1) + 
    scale_fill_viridis_c(option = "B", na.value  = "#CECECE") +
    theme(panel.background = element_rect(fill = "#596673"))

  freq_map_gg
}