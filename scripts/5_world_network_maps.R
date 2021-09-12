

# Network maps over world maps

# df to graph of country collaborations
fun_country_network <- function(db){
  
  # Names to title case
  df_countries <- db %>%
    select(entry_number, affiliation_country) %>%
    filter(!is.na(affiliation_country)) %>%
    filter(affiliation_country != "NA")

  # Edges between the strings
  edges <-  db %>%
    select(entry_number, affiliation_country) %>%
    split(., db$entry_number) %>%
    lapply(function(x){
      nms <- unlist(x$affiliation_country)
      expand.grid(nms, nms, stringsAsFactors = FALSE)
    }) %>%
    bind_rows(.id = "ID")
  
  # remove duplicates (auth a:auth b == auth b:auth a)
  edges <- edges %>% 
    group_by(ID) %>%
    mutate(key = paste0(pmin(Var1, Var2), ";", pmax(Var1, Var2))) %>% 
    distinct(key) %>%
    separate(key, c("from", "to"), sep = ";")
  
  # get number of collabs/pubs per author
  edges <- edges %>%
    group_by(from, to) %>%
    summarise(collabs = n()) %>%
    arrange(-collabs)
  
  # df to graph
  graph <- edges %>%
    igraph::graph_from_data_frame(directed = FALSE)
  
  return(graph)
}

# map.no.authors <- function(freq_table)
fun_map_network <- function(nwk, node_xy, n_label = 15){
  
  library(tidyverse)
  library(igraph)
  library(ggraph)
  library(ggrepel)

  # drop NA and fix Russia, convert graph to df
  df_nwk <- igraph::as_data_frame(nwk) %>%
    filter(from != "NA" | to != "NA") %>%
    mutate(from = str_replace(from, "Russian Federation", "Russia"),
           to = str_replace(to, "Russian Federation", "Russia"))
  
  # add "from" coords to df 
  df_nwk2 <- node_xy %>%
    select(-country) %>%
    rename(from = name, 
           x = longitude,
           y = latitude) %>%
    right_join(df_nwk)
  
  # add "to" coords to df 
  df_nwk2 <- node_xy %>%
    select(-country) %>%
    rename(to = name, 
           x_end = longitude,
           y_end = latitude) %>%
    right_join(df_nwk2) %>%
    select(from, to, collabs, x, y, x_end, y_end)
  
  # drop self connections
  plot_edges <- df_nwk2 %>%
    filter(from != to)
  
  # country # papers
  df_cnt_sz <- df_nwk2 %>% 
    filter(from == to)  %>%
    select(-to) %>%
    rename(name = from,
           collabs2 = collabs)

  # node labels
  nodes_lbl <- df_cnt_sz %>%
  slice_max(collabs2, n = n_label, with_ties = FALSE) 

  # background map
  country_shapes <- geom_polygon(aes(x = long, y = lat, group = group),
                                 data = map_data('world'),
                                 fill = "#CECECE", color = "#515151",
                                 size = 0.15)
  
  mapcoords <- coord_fixed(xlim = c(-150, 180), ylim = c(-55, 80))
  
  #plot
   ggplot() + 
     country_shapes +
     geom_curve(data = plot_edges,
                aes(x = x, y = y, xend = x_end, yend = y_end, size = collabs, alpha = collabs),
                curvature = 0.33) +
     scale_size_continuous(guide = FALSE, range = c(1, 5)) +
     geom_point(data = df_cnt_sz,
                aes(x = x, y = y, size = collabs2),                          
                shape = 21,
                fill = 'firebrick',
                color = 'black',
                stroke = 0.5) +
     geom_text_repel(data = nodes_lbl,
                     aes(x = x, y = y, label = name),
                     size = 3,
                     color = "firebrick",
                     fontface = "bold") +
    mapcoords +
    theme_void() +
    theme(
       panel.background = element_rect(fill = "#596673"),
       plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), 'cm'),
       legend.position = "none")
}    


# required packages
library(gsheet)
library(tidyverse)
library(igraph)
library(patchwork)

# get data from google sheets
df_meta <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/13OtfyWIqS8XKhuBXzboJIyLzFFUp702ESCmk91Q204Q/edit#gid=1641925563")
df_auth <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/13OtfyWIqS8XKhuBXzboJIyLzFFUp702ESCmk91Q204Q/edit#gid=1222222462")


countries <- read_csv('./data/raw/countries.csv')

# cerate network
nwk <- fun_country_network(df_auth)

#plot
p1 <- fun_map_network(nwk = nwk, node_xy = countries, n_label = 15)
  
# network world map
ggsave("./figures/collaborator_map.png", plot = p1, width = 20, height = 15,
       units = "cm")


