
# required packages
library(tidyverse)
library(tidytext)
library(igraph)
library(ggraph)
library(tidygraph)
library(gsheet)


# get data from google sheets
df_meta <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/13OtfyWIqS8XKhuBXzboJIyLzFFUp702ESCmk91Q204Q/edit#gid=1641925563")
df_auth <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/13OtfyWIqS8XKhuBXzboJIyLzFFUp702ESCmk91Q204Q/edit#gid=1222222462")


df_keyw <- df_meta %>%
  filter(!is.na(dc_description)) 

# my stopwords
my_stp_wrds <- data.frame(word = c("study", "freshwater", "result", "water",
                                   "galaxiid", "galaxiids", "galaxias", "effect"))



tidy_abs <- df_keyw |>
  select(entry_number, year_date, dc_description) |> 
  unnest_tokens(word, dc_description) |> 
  anti_join(stop_words) |> 
  anti_join(my_stp_wrds) |> 
  ungroup()

tidy_abs |>
  mutate(word = SnowballC::wordStem(word)) |>
  count(word, sort = T) |>
  top_n(20) |>
  mutate(word = fct_reorder(word, desc(-n))) |>
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL) +
  theme_minimal()


## Most common word pairs in abstracts (again, no surprises)
tidy_abs_ngrams <- df_keyw |>
  select(entry_number, year_date, dc_description) |> 
  unnest_tokens(bigram, dc_description, token = "ngrams", n = 2) |> 
  separate(bigram, into = c("word1", "word2"), sep = " ") |>
  filter(!word1 %in% c(stop_words$word, NA),
         !word2 %in% c(stop_words$word, NA)) |>
  unite(bigram, c(word1, word2), sep = " ") |>
  count(bigram, sort = T) 

bigram_stp_wrds <- data.frame(bigram = c("rights reserved"))

tidy_abs_ngrams <- tidy_abs_ngrams %>%
  anti_join(bigram_stp_wrds) 

tidy_abs_ngrams |>
  top_n(20) |>
  mutate(bigram = reorder(bigram, n)) |>
  ggplot(aes(n, bigram)) +
  geom_col() +
  labs(y = NULL) +
  theme_minimal()


## Network of word pairs ----------------------------------------
sp_stp_wrds <- data.frame(bigram = c("galaxias maculatus"))

tidy_abs_ngrams_net <- df_keyw |>
  select(entry_number, dc_description) |> 
  mutate(
    dc_description = tolower(dc_description))|>
  unnest_tokens(bigram, dc_description, token = "ngrams", n = 2) |>
  anti_join(sp_stp_wrds) %>%
  separate(bigram, into = c("from", "to"), sep = " ") |>
  filter(!from %in% c(stop_words$word, NA),
         !to %in% c(stop_words$word, NA)) |>
  mutate(from = SnowballC::wordStem(from),
         to = SnowballC::wordStem(to)) |>
  unite(bigram, c(from, to), sep = " ", remove = FALSE) |>
  mutate(from = recode(from, murrai = "murray")) |>
  count(from, to, sort = T) 

# get top bigrams
bigram_graph <- tidy_abs_ngrams_net %>% 
  top_n(100) 

# get total mentios for each word
wrd_cnts <- tidy_abs_ngrams_net %>% 
  pivot_longer(cols = from:to) %>%
  group_by(value) %>%
  summarise(tot = sum(n)) %>%
  arrange(-tot) %>%
  dplyr::filter(value %in% bigram_graph$from | value %in% bigram_graph$to) 


bigram_graph2 <- bigram_graph %>%
  graph_from_data_frame(directed = TRUE, vertices = wrd_cnts)

arrow <- grid::arrow(type = "closed", length = unit(.15, "inches"))

p1 <- ggraph(bigram_graph2, layout = "fr") + 
  geom_node_point(aes(size = tot), shape = 16, colour = "grey") +
  geom_edge_link(aes(edge_colour = log(n)), arrow = arrow, end_cap = circle(0.05, "inches"), edge_width = 1) + 
  scale_size_continuous(range = c(3, 15)) +
  geom_node_text(aes(label = name), repel = TRUE) +
  scale_edge_colour_viridis() +
  theme_graph(base_family="sans") +
  labs(edge_colour = "log(\nbigram \ncount)", size = "word \ncount") +
  guides(
    size = guide_legend(reverse = TRUE))

p1

ggsave("./figures/p3_abstract_bigram_net.pdf", p1, width = 25, heigh = 20, units = "cm")
ggsave("./figures/p3_abstract_bigram_net.png", p1, width = 25, heigh = 20, units = "cm", dpi = 300)
