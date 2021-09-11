

# Network maps over world maps
# George's functions

source("./scripts/1_biblio_tidying.R")
source("./scripts/0_georges_country_code.R")

# required packages
library(tidyverse)

# load and tidy wos bib files - ignore warning
dat <- fun_tidy_multi_wos_bibs(pth = "./bibs")
countries <- read_csv('./data/countries.csv')


# seperate df and db objects
biblioDB <- dat[[1]]
biblioDF <- dat[[2]]

# drop double ;;
biblioDF$AuthorKeywords <- gsub(";;", ";", biblioDF$AuthorKeywords)

# country network
cnt <- parse.countries(countries, biblioDF)

cnt <- gsub(", GEORGIA", "", cnt)
cnt <- gsub("GEORGIA, ", "", cnt)
cnt <- gsub("GEORGIA, ", "USA", cnt)

# country network - no map
cnt.nwk <- text.network(cnt, cluster.alg = "louvain", min_wgt = 0)
plot.text.network(cnt.nwk$network_igraph, show = FALSE)

ggsave("figures/country_network.png", width = 20, height = 20, units = "cm")




# fix country names so they match the world map
cnt.nwk$freq_table$txt <- as.character(cnt.nwk$freq_table$txt)
cnt.nwk$freq_table$txt[cnt.nwk$freq_table$txt == "usa"] <- "United States"
cnt.nwk$freq_table$txt[cnt.nwk$freq_table$txt %in% c("england", "scotland", "wales")] <- "United Kingdom"

cnt.nwk$freq_table <- cnt.nwk$freq_table %>%
  group_by(txt) %>%
  summarise(Freq = sum(Freq)) %>%
  mutate_at(vars(txt), as.factor) %>%
  as.data.frame()

# number of studies per country
map.no.authors(cnt.nwk$freq_table, map_scale = "medium")
ggsave("figures/number_studies_per_country_map.png", width = 20, height = 15, units = "cm")


# network world map
map.network(cnt.nwk$network_igraph, node_xy = countries, n_label = 15)
ggsave("figures/country_collaborations_map.png", width = 20, height = 15, units = "cm")



  













