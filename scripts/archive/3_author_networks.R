# author collaborator networks

library(tidyverse)
library(ggraph)
library(igraph)
library(bibliometrix)

#####################
## HELPER FUNCTIONS #
#####################

# Tidy biblio data
fun_biblio_nwk <- function(biblioDB, n_authors) {
  # Create authors co-occurrences network
  NetMatrix <- biblioNetwork(biblioDB, analysis = "collaboration", 
                             network = "authors", sep = ";")
  
  xx <- as.matrix(NetMatrix)
  g  <- graph.adjacency(xx, weighted = TRUE)
  df <- get.data.frame(g)
  
  # filter top x authors 
  kp <- df[df$from == df$to,]
  kp <- kp %>%
    arrange(-weight) %>%
    top_n(n_authors)
  
  # get author countries 
  zz <- biblioDB %>%
    select(ID, RP)
  
  zz$name <- word(zz$RP, 1,2, sep = " ")
  zz$name <- gsub(",", "", zz$name)
  zz$country <- word(zz$RP, -1)
  kp$country <- NA
  
  for (i in 1:nrow(kp)) {
    nm <- zz %>%
      filter(name == kp[i,1])
    kp$country[i] <- nm[1,4]
  }
  
  df2 <- df %>%
    filter(from %in% kp$from) %>%
    filter(to %in% kp$from) 
  
  colnames(df2)[3] <- "Collabs"
  df2$from <- paste(stringr::str_to_title(stringr::word(tolower(df2$from), 1)), 
                    word(df2$from, 2))
  df2$to <- paste(stringr::str_to_title(stringr::word(tolower(df2$to), 1)), 
                  word(df2$to, 2))
  
  # convert to igraph object
  graph <- graph_from_data_frame(df2)
  graph <- simplify(graph, edge.attr.comb = "max")
  
  # node size
  V(graph)$Papers <- kp$weight
  
  # node colour
  kp <- kp %>%
    mutate(country = ifelse(is.na(country), "missing", country))
  V(graph)$Country <- kp$country
  
  return(graph)
}

# plot collaborator networks 
fun_graph <- function(dat, colr, ttl){ 
  # plot 
  lay = create_layout(dat, layout = "fr")
  ggraph(lay) + 
    geom_edge_link(aes(edge_width = Collabs), edge_colour = "grey") + 
    geom_node_point(aes(size = Papers/2, colour = Country), shape = 16, 
                    alpha = 0.9) +
    scale_size_identity(guide = "legend") +
    #scale_size_continuous(range = c(3, 15)) + 
    scale_colour_manual(values  = colr) +
    geom_node_text(aes(label = name), repel = TRUE) +
    ggplot2::ggtitle(ttl) +
    theme_graph() +
   # guides(color = guide_legend(override.aes = list(size = 5))) +
    theme(legend.position = "none",
          plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))
}

#----------------------------------------------------------------------------

###########
## START ##
###########

dat <- readRDS("data/biblio_tidy.rds")


# seperate df and db objects
biblioDB <- dat[[1]]
biblioDF <- dat[[2]]

# drop double ;;
biblioDF$AuthorKeywords <- gsub(";;", ";", biblioDF$AuthorKeywords)

# drop single record with no year
biblioDB <- biblioDB %>%
  filter(!is.na(PY))

# 1986 and 1987 are missing (50 - 60 articles)
min(biblioDB$PY, na.rm = T)

# create decade column 
biblioDB2 <- biblioDB %>%
  mutate(decade = cut(PY, 
                    breaks = c(1985, 1999, 2009, 2022), 
                    labels = c("1985-1999","2000-2009", "2010-2021")))


# generate networks for separate decades 
biblioDB2 <- split(biblioDB2, biblioDB2$decade)
dat_grphs <- lapply(biblioDB2, fun_biblio_nwk, n_authors = 30)

# generate network 
datall <- fun_biblio_nwk(biblioDB, n_authors = 50)
dat1990s <- dat_grphs[[1]]
dat2000s <- dat_grphs[[2]]
dat2010s <- dat_grphs[[3]]

# create colour palette 
cnt <- unique(c(unique(V(dat1990s)$Country), 
  unique(V(dat2000s)$Country), 
  unique(V(dat2010s)$Country)))

cnt <- data.frame(id = 1:length(cnt), cntry = cnt)

cnt <- cnt %>%
  mutate(cntry = ifelse(is.na(cntry), "missing", cntry))

cnt$cntry <- as.factor(cnt$cntry)
pl <- randomcoloR::distinctColorPalette(nrow(cnt))
my_cols <- setNames(pl, levels(cnt$cntry))

# make graphs 
p1 <- fun_graph(dat1990s, my_cols, ttl = "Decade: 90's")
p2 <- fun_graph(dat2000s, my_cols, ttl = "Decade: 00's")
p3 <- fun_graph(dat2010s, my_cols, ttl = "Decade: 10's")

p1 / p2 / p3

ggsave("figures/author_collab.png", width = 20, height = 20, units = "cm")

