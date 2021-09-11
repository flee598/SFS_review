# author collaborator networks

library(tidyverse)
library(ggraph)
library(igraph)

#####################
## HELPER FUNCTIONS #
#####################

# df to graph of author collaborations
fun_auth_network <- function(db, n_authors){
  
  # Names to title case
  db <- db %>%
    mutate(authname = stringr::str_to_title(authname),
           affiliation_country = replace_na(affiliation_country, "missing"))
  
    # Edges between the strings
  edges <-  db %>%
    select(entry_number, authname) %>%
    split(., db$entry_number) %>%
    lapply(function(x){
      nms <- unlist(x$authname)
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
  
  # get top n authors
  auth_cnt <- edges %>%
    ungroup %>%
    filter(from == to) %>%
    slice_head(n = n_authors)
  
    # filter edges to only the authors above
  df2 <- edges %>%
    filter(from %in% auth_cnt$from) %>%
    filter(to %in% auth_cnt$to)
  
  
  # Get author country combos, if author has multiple countries, use most recent
  cnt_df <- db %>%
    select(authname, affiliation_country, year_date) |>
    group_by(authname) %>%
    filter(year_date == max(year_date)) %>%
    distinct(authname, .keep_all = T) %>%
    select(authname, affiliation_country)
  
  # join author countries and number of papers
  cnt_df <- df2 %>%
    filter(from == to) %>%
    select(from, collabs) %>%
    rename(authname = from) %>%
    left_join(cnt_df) %>%
    rename(papers = collabs)
    
  # df to graph
  graph <- df2 %>%
    graph_from_data_frame(directed = TRUE, vertices = cnt_df)
  
  return(graph)
}


# plot collaborator networks 
fun_graph <- function(dat, colr, ttl){ 

  lay = create_layout(dat, layout = "fr")
  
  ggraph(lay) + 
    geom_edge_link(aes(edge_width = collabs), edge_colour = "grey") + 
    geom_node_point(aes(size = papers, colour = affiliation_country), shape = 16, 
                    alpha = 0.9) +
    scale_size_identity(guide = "legend") +
    scale_colour_manual(values  = colr, limits = force) +
    geom_node_text(aes(label = name), repel = TRUE) +
    ggplot2::ggtitle(ttl) +
    guides(edge_width = "none", size = "none") +
    labs(colour = "Country") +
    theme_graph(base_family = "sans") +
    theme(legend.position = "bottom",
          plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))
}

# START ------------------------------------------------------------------------

library(gsheet)
library(tidyverse)
library(patchwork)

# get data from google sheets
df_meta <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/13OtfyWIqS8XKhuBXzboJIyLzFFUp702ESCmk91Q204Q/edit#gid=1641925563")
df_auth <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/13OtfyWIqS8XKhuBXzboJIyLzFFUp702ESCmk91Q204Q/edit#gid=1222222462")

# drop missing stuff
df_auth_net <- df_auth |>
  filter(!is.na(authname)) |>
  filter(!is.na(year_date))

# create decade column 
df_auth_net <- df_auth_net %>%
  mutate(decade = cut(year_date, 
                      breaks = c(1980, 1989, 1999, 2009, 2022), 
                      labels = c("1982-1989","1990-1999", "2000-2009", "2010-2021")))


# generate networks for separate decades 
df_auth_net_splt <- split(df_auth_net, df_auth_net$decade)
dat_grphs <- lapply(df_auth_net_splt, fun_auth_network, n_authors = 100)

# create colour palette for countries
cnts <- unique(unlist(lapply(dat_grphs, function(x) unique(V(x)$affiliation_country))))
cnts <- as.factor(cnts)
my_cols <- randomcoloR::distinctColorPalette(length(cnts))
my_cols <- setNames(my_cols, levels(cnts))

# make graphs 
p1 <- fun_graph(dat_grphs[[1]], my_cols, ttl = "Decade: 1980's")
p2 <- fun_graph(dat_grphs[[2]], my_cols, ttl = "Decade: 1990's")
p3 <- fun_graph(dat_grphs[[3]], my_cols, ttl = "Decade: 2000's")
p4 <- fun_graph(dat_grphs[[4]], my_cols, ttl = "Decade: 2010's")

# plot all together.. doesn't fit on 1 page nicely
# combined <- (p1 + p2) / (p3 +p4) & theme(legend.position = "bottom")
# combined + plot_layout(guides = "collect")

# save each decade
ggsave("./figures/author_net_1980s.png", plot = p1, width = 25, height = 20, units = "cm", dpi = 300)
ggsave("./figures/author_net_1990s.png", plot = p2, width = 25, height = 20, units = "cm", dpi = 300)
ggsave("./figures/author_net_2000s.png", plot = p3, width = 25, height = 20, units = "cm", dpi = 300)
ggsave("./figures/author_net_2010s.png", plot = p4, width = 25, height = 20, units = "cm", dpi = 300)


# network analysis -------------------------------------------------------------

# greate a network of the top 100 authors each year and look at how the number 
# of components and size of compoents changes through time.

# cluster by year --------------------------------------------------------------
df_auth_yr <- df_auth_net %>%
  select(entry_number, year_date, authname, affiliation_country, decade)

# generate networks for separate years - top 100 authors only 
df_auth_net_splt_yr <- split(df_auth_yr, df_auth_yr$year_date)
dat_grph_yr <- lapply(df_auth_net_splt_yr, fun_auth_network, n_authors = 100)

# only keep years with at least 100 authors
dat_grph_yr2 <- dat_grph_yr[lapply(dat_grph_yr, gorder) > 99]

# clusters and cluster sizes
g_size <- sapply(dat_grph_yr2, function(x) igraph::clusters(x)$csize)
g_size <- setNames(stack(g_size)[2:1], c('year', 'size'))

# summarise
g_size <- g_size %>%
  group_by(year) %>%
  summarise(mean_size = mean(size),
            numb = n()) %>%
  pivot_longer(mean_size:numb) %>%
  mutate(year = as.numeric(as.character(year)),
         name = recode(name, 
                       "mean_size" = "mean size",
                       "numb" = "number")) 

# plot
p1 <- ggplot(g_size, aes(year, value, colour = name, group = name)) +
  geom_point(size = 1, alpha = 0.5) +
  geom_line() +
  scale_colour_brewer(palette = "Dark2") +
  labs(x = "Year", y = "Clusters", colour = "Cluster metric") +
  theme_bw() +
  theme(legend.position = "bottom")


# cluster by decade ------------------------------------------------------------ 
df_auth_yr <- df_auth_net %>%
  select(entry_number, year_date, authname, affiliation_country, decade)

# generate networks for separate years - top 100 authors only 
df_auth_net_splt_yr <- split(df_auth_yr, df_auth_yr$decade)
dat_grph_yr <- lapply(df_auth_net_splt_yr, fun_auth_network, n_authors = 100)

# only keep years with at least 100 authors
dat_grph_yr2 <- dat_grph_yr[lapply(dat_grph_yr, gorder) > 99]

# clusters and cluster sizes
g_size <- sapply(dat_grph_yr2, function(x) igraph::clusters(x)$csize)
g_size <- setNames(stack(g_size)[2:1], c('year', 'size'))

# summarise
g_size2 <- g_size %>%
  group_by(year) %>%
  summarise(mean_size = mean(size),
            numb = n()) %>%
  pivot_longer(mean_size:numb) %>%
  mutate(year = as.character(year),
         name = recode(name, "mean_size" = "mean size",
                       "numb" = "number"))

# plot
p2 <- ggplot(g_size2, aes(year, value, colour = name, group = name)) +
  geom_point(size = 1, alpha = 0.5) +
  geom_line() +
  scale_colour_brewer(palette = "Dark2") +
  labs(x = "Decade", y = "Clusters", colour = "Cluster metric") +
  theme_bw() +
  theme(legend.position = "bottom")

p3 <- p1 + p2 + plot_layout(guides = "collect") & theme(legend.position = "bottom")

ggsave("./figures/author_net_clusters.png", plot = p3, width = 18, height = 10, units = "cm", dpi = 300)





