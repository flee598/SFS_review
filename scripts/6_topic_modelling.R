# Topic modelling (biterm and LDA)

# functions --------------------------------------------------------------------

# plot networks
# do not have ggraph loaded when using this function, it 
# somehow messes up the edge weights
fun_plot_btm_igraph <- function(g){
  ggraph::ggraph(g, layout = 'igraph', algorithm = "fr") +
    ggraph::geom_edge_link0(ggplot2::aes(edge_alpha = cooc, edge_width = cooc/100, 
                                         edge_colour = best_topic)) +
    ggraph::geom_node_point(colour = "red") +
    ggraph::geom_node_text(ggplot2::aes(label = name, size = probability*2), 
                           col = "black", repel = TRUE) +
    ggplot2::theme_void() + 
    ggplot2::theme(legend.position = "none") +
    ggforce::geom_mark_hull(ggplot2::aes(x, y, group = topic,label = topic), 
                            color = NA)
}


# biterm topic modelling ------------------------------------------------------

library(tidyverse)
# library(udpipe)    # Tokenizing, Lemmatising, Tagging and Dependency Parsing
# library(data.table)
# library(stopwords)
# library(ggraph)
# library(tidytext)
# library(topicmodels)
# library(tm)             # stemming words 

# required for ggforce::geom_mark_hull() which is used to add topic labels
# install.packages("concaveman")


# LDA TOPIC MODELLING ---------------------------------------------------------


# get data from google sheets
df_meta <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/13OtfyWIqS8XKhuBXzboJIyLzFFUp702ESCmk91Q204Q/edit#gid=1641925563")
df_auth <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/13OtfyWIqS8XKhuBXzboJIyLzFFUp702ESCmk91Q204Q/edit#gid=1222222462")

colnames(df_meta)

# get relevant cols
lda_data <- df_meta %>%
  select(entry_number, year_date, subtype_description, dc_description) %>%
  filter(!subtype_description %in% c("Erratum", "Conference Paper")) %>%
  filter(!is.na(dc_description))


lda_data <- lda_data %>%
  tidytext::unnest_tokens(word, dc_description)

# pre defined stop words
stpWrds <- rbind(tidytext::get_stopwords(source = "snowball"), 
                 tidytext::get_stopwords(source = "smart"))
stpWrds <- unique(stpWrds[ , 1])

# custom stop words - place holder
stpWrds2 <- data.frame(word = c("my", "custom", "stop", "words"))

# combine all stop words
stpWrds3 <- rbind(stpWrds, stpWrds2)

# # remove numbers (e.g. years)
lda_data_2 <- lda_data %>%
  filter(!(str_detect(word, "\\d")))

#  stemm words & remove stop words 
lda_data_2 <- lda_data %>%
  mutate(word = tm::stemDocument(.$word)) %>%
  anti_join(stpWrds3) %>%
  group_by(entry_number) %>%
  count(word) %>%
  tidytext::cast_dtm(entry_number, word, n)

# # Find optimal number of topics - only needs to be done once
# 
# library(ldatuning)
# 
# result <- FindTopicsNumber(
#   lda_data_2,
#   topics = seq(from = 2, to = 52, by = 5),
#   metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
#   method = "Gibbs",
#   control = list(seed = 77),
#   mc.cores = 3L,
#   verbose = TRUE
# )
# FindTopicsNumber_plot(result)

# optimal suggested = 17 - 50

# run  LDA topic model 
n_topics <- 10
ap_lda <- topicmodels::LDA(lda_data_2, k = n_topics, control = list(seed = 12))

# word-topics 
ap_topics <- tidytext::tidy(ap_lda, matrix = "beta")

# topics
ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(6, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# name of topics - place holder
topic2 <- data.frame(topic = 1:n_topics, 
                     topic2 = paste("t", 1:n_topics))

ap_top_terms <- left_join(ap_top_terms, topic2)

# plot topic beta scores
ap_top_terms %>%
  mutate(term = tidytext::reorder_within(term, beta, topic2)) %>%
  ggplot(aes(beta, term, fill = factor(topic2))) +
  scale_fill_viridis_d(direction = 1, option = "E") +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic2, scales = "free") +
  tidytext::scale_y_reordered() +
  scale_x_continuous(n.breaks = 3)


################################
# document-topics through time #
################################

ap_documents <- tidytext::tidy(ap_lda, matrix = "gamma")

dat4a <- lda_data %>%
  select(year_date , entry_number)

colnames(ap_documents)[1] <- "entry_number" 
ap_documents$entry_number <- as.numeric(ap_documents$entry_number)

ap_documents <- ap_documents %>% inner_join(dat4a)
ap_documents <- left_join(ap_documents, topic2)


ap_documents <- ap_documents %>%
  group_by(year_date, topic2) %>%
  summarise(MnGamma = mean(gamma))

# plot change in topic trends over time
ggplot(ap_documents, aes(year_date, MnGamma)) +
  geom_line() +
  geom_smooth() +
  geom_hline(yintercept = 0.05) +
  coord_cartesian(ylim = c(0, 0.3)) +
  facet_wrap(~topic2) +
  labs(x = "Year", y = "Gamma")

ggsave("./figures/lad_topics_time.png", width = 20, height = 20, units = "cm")



#############
# Bump plot #
#############
library(ggbump)
library(cowplot)


dat_bump <- ap_documents %>%
  as.data.frame() %>%
  na.omit(year_date) %>%
  mutate(decade = cut(year_date, 
                      breaks = c(1980, 1989, 1999, 2009, 2022), 
                      labels = c("1982-1989","1990-1999", "2000-2009", "2010-2021")),
         decade = as.integer(decade),
         topic2  = as.factor(topic2)) %>%
  group_by(decade, topic2) %>%
  summarise(MnGamma = mean(MnGamma)) %>%
  group_by(decade) %>%
  mutate(rank = rank(-MnGamma, ties.method = "random")) %>% 
  ungroup() %>%
  na.omit() %>%
  arrange(rank)


country_order <- dat_bump %>%
  filter(decade == 1) %>%
  arrange(rank) %>%
  pull(topic2)

dat_bump <- dat_bump %>%
  arrange(rank)

lab <- dat_bump %>%
  group_by(topic2) %>%
  filter(decade == 1) %>%
  mutate(lab = rank) %>%
  select(topic2, lab)

dat_bump <- left_join(dat_bump, lab)

xlabs <- c("1980s", "1990s", "2000s", "2010s")

p1 <- ggplot(dat_bump, aes(decade, rank, color = factor(topic2, levels = country_order))) +
  geom_bump(size = 1.5, smooth = 8) +
  geom_point(size = 7) +
  geom_text(data = dat_bump %>% filter(decade == min(decade)),
            aes(x = decade - .1, label = topic2), size = 4, hjust = 1) +
  geom_text(data = dat_bump %>% filter(decade == max(decade)),
            aes(x = decade + .1, label = topic2), size = 4, hjust = 0) +
  geom_text(aes(label = lab),hjust = 0.55, vjust = 0.4, colour = "white") +
  coord_cartesian(clip = 'off') +
  scale_colour_viridis_d(direction = 1, option = "E") +
  labs(x = "", y = "") +
  theme_minimal_grid(line_size = 0) +
  theme(legend.position = "none",
        panel.grid.major = element_blank()) +
  scale_y_reverse() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.margin = margin(10, 100, 10, 100)) +
  scale_x_continuous(breaks = dat_bump$decade %>% unique() %>% sort(),
                     labels = xlabs, 
                     expand = expansion(mult = .1))

# set order for facets 
lvs <- dat_bump %>%
  filter(decade == 1) %>%
  pull(topic2)

ap_top_terms$topic2 <- factor(ap_top_terms$topic2, levels = lvs)


p2 <- ap_top_terms %>%
  mutate(term = tidytext::reorder_within(term, beta, topic2)) %>%
  ggplot(aes(beta, term, fill = factor(topic2))) +
  scale_fill_viridis_d(direction = 1, option = "E") +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic2, scales = "free") +
  tidytext::scale_y_reordered() +
  scale_x_continuous(n.breaks = 3) +
  labs(x = "Beta", y = "Term") +
  theme_bw() 

p3 <- ggpubr::ggarrange(p2, p1, 
                  labels = c("A", "B"),
                  ncol = 1, nrow = 2, heights = c(1.4,1))

ggsave("./figures/lda_topic_modelling.png", width = 20, height = 25, units = "cm")
