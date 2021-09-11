# Topic modelling (biterm and LDA)

# functions --------------------------------------------------------------------

# convert output of BTM model to igraph format for plotting
# copied/edited from https://github.com/bnosac/textplot/blob/master/R/textplot_biterms.R
# Had some issues with the original function (textplot_bitermclusters()) so 
# I have taken the source code and edited it, still janky as, can definitely 
# be clarified a lot.

# top_n_term = integer - the number of words associated with each topic to 
# retain
fun_btm_to_igraph <- function(btm_model_output, top_n_term) {
  
  # pull needed data
  group_terms <- stats::terms(btm_model, top_n = top_n_term)
  group_biterms <- stats::terms(btm_model, type = "biterms")$biterms
  
  # define a bunch of variables
  topic <- .N <- term1 <- term2 <- select <- best_topic <- 
    cooc <- name <- x <- y <- probability <- topic_freq <- NULL
  
  # get labels
  labels = seq_len(length(table(group_biterms$topic)))
  
  displayterms <- data.table::rbindlist(group_terms, idcol = "topic")
  displayterms <- data.table::setDF(displayterms)
  
  # order and remove duplicates
  displayterms <- displayterms[base::order(displayterms$probability, 
                                           decreasing = TRUE), ]
  displayterms <- displayterms[!base::duplicated(displayterms$token), ]
  
  ## Get most occuring topic for each biterm
  biterms <- data.table::copy(group_biterms)
  biterms <- data.table::setDT(biterms)
  
  # data.table stuff .....
  biterms <- biterms[, topic_freq := .N, by = list(term1, term2)]
  
  biterms <- biterms[, list(best_topic = topic[which.max(topic_freq)], 
                            cooc = .N), by = list(term1, term2)]
  
  biterms <- biterms[biterms$term1 %in% displayterms$token & 
                       biterms$term2 %in% displayterms$token, ]
  
  biterms <- biterms[base::order(biterms$cooc, biterms$best_topic, decreasing = TRUE), ]
  biterms <- biterms[, select := seq_len(.N), by = list(best_topic)]
  
  tt <- base::split(displayterms, displayterms$topic)
  biterms <- base::split(biterms, biterms$best_topic)
  
  biterms <- base::lapply(base::intersect(names(tt), names(biterms)), 
                          FUN = function(i){
                            
                            topictokens <- tt[[i]]
                            topictokens <- as.character(topictokens$token)
                            bi <- biterms[[i]]
                            bi <- bi[bi$term1 %in% topictokens & bi$term2 %in% topictokens, ]
                            bi <- bi[bi$term1 != bi$term2, ]
                            bi}
  )
  
  biterms <- data.table::rbindlist(biterms)
  
  nodes <- displayterms[displayterms$token %in% c(biterms$term1, biterms$term2), 
                        c("token", "topic", "probability")]
  
  nodes <- nodes[base::order(nodes$topic, nodes$token), ]
  
  nodes$topic <- base::factor(nodes$topic, levels = seq_len(length(labels)), 
                              labels = labels)
  
  biterms$best_topic <- base::factor(biterms$best_topic, levels = seq_len(length(labels)), 
                                     labels = labels)
  
  # convert df to igraph 
  g <- igraph::graph_from_data_frame(biterms, vertices = nodes, directed = FALSE)
  g
}

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
# library(BTM)       # Biterm Topic Modeling
# library(data.table)
# library(stopwords)
# library(textplot)
# library(ggraph)
# library(tidytext)
# library(topicmodels)
# library(revtools)
# library(bibliometrix)
# library(tm)             # stemming words 

# required for ggforce::geom_mark_hull() which is used to add topic labels
# install.packages("concaveman")

# source biblio tidying function
source("./scripts/1_biblio_tidying.R")

# load and tidy wos bib files (in folder called "bibs" for me) - ignore warning
dat <- fun_tidy_multi_wos_bibs(pth = "./bibs")

# seperate df and db objects
biblioDB <- dat[[1]]
biblioDF <- dat[[2]]


# tidy/select relvant data
biblioDF2 <- biblioDF %>%
  select(ID, Abstract) %>%
  mutate(doc_id = ID,
         text = str_to_lower(Abstract),
         text = str_replace_all(text, "'", ""))


# annotate abstracts
annotated_reviews <- udpipe::udpipe(biblioDF2, "english")
biterms <- data.table::as.data.table(annotated_reviews)

# look for word co-occurrences, only look at certain word types and words 
# longer than 2 characters, using a neighbourhood of 5 words
biterms <- biterms[, 
                   udpipe::cooccurrence(x = lemma,
                                relevant = upos %in% c("NOUN", "ADJ", "VERB") & 
                                  nchar(lemma) > 2 & !lemma %in% stopwords::stopwords("en"),
                                skipgram = 5),
                   by = list(doc_id)]


# Biterm model
set.seed(123456)

# only look at certain word types and words longer than 2 characters
train_data <- annotated_reviews %>% 
  filter(
    upos %in% c("NOUN", "ADJ", "VERB"),
    !lemma %in% stopwords::stopwords("en"),
    nchar(lemma) > 2) %>%
  select(doc_id, lemma)

# k = number of topics to look for
btm_model <- BTM::BTM(train_data, biterms = biterms, k = 10,
                      iter = 2000, background = TRUE)

# plot
# don't have ggraph loaded or the edges won't plot properly

detach("package:ggraph", unload = TRUE)

# btm output to igraph
g_btm <- fun_btm_to_igraph(btm_model, top_n_term = 10)

# plot
fun_plot_btm_igraph(g_btm)

ggsave("figures/biterm_topic_modelling.png", width = 20, height = 20, units = "cm")


# LDA TOPIC MODELLING ---------------------------------------------------------


# trim whitespace
biblioDB$AU <- stringr::str_trim(biblioDB$AU, side = "both")


# check for missing data 
apply(biblioDF, 2, function(x) table(is.na(x)))

# get relevant cols
lda_data <- biblioDF %>%
  select(YearPublished, Title, Abstract)

# check for missing abstracts. 
sum(is.na(lda_data$Abstract))


lda_data <- lda_data %>%
  tidytext::unnest_tokens(word, Abstract)

# pre defined stop words
stpWrds <- rbind(tidytext::get_stopwords(source = "snowball"), 
                 tidytext::get_stopwords(source = "smart"))
stpWrds <- unique(stpWrds[ , 1])

# custom stop words - place holder
stpWrds2 <- data.frame(word = c("my", "custom", "stop", "words"))

# combine all stop words
stpWrds3 <- rbind(stpWrds, stpWrds2)


# remove stop words 
lda_data_2 <- lda_data %>%
  anti_join(stpWrds3)

# remove numbers (e.g. years)
lda_data_2 <- lda_data_2 %>%
  filter(!(str_detect(word, "\\d")))

#  stemm words & remove stop words 
lda_data_2 <- lda_data_2 %>%
  mutate(word = tm::stemDocument(.$word)) %>%
  anti_join(stpWrds3) %>%
  group_by(Title) %>%
  count(word) %>%
  tidytext::cast_dtm(Title, word, n)

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
ap_documents

dat4a <- lda_data %>%
  select(YearPublished, Title)

colnames(ap_documents)[1] <- "Title" 

ap_documents <- ap_documents %>% inner_join(dat4a)
ap_documents <- left_join(ap_documents, topic2)



ap_documents <- ap_documents %>%
  group_by(YearPublished, topic2) %>%
  summarise(MnGamma = mean(gamma))

# plot change in topic trends over time
ggplot(ap_documents, aes(YearPublished, MnGamma)) +
  geom_line() +
  geom_smooth() +
  geom_hline(yintercept = 0.1) +
  coord_cartesian(ylim = c(0, 0.3)) +
  facet_wrap(~topic2) 

ggsave("figures/lda_topics_time.png", width = 20, height = 20, units = "cm")



#############
# Bump plot #
#############
library(ggbump)
library(cowplot)


str(dat_bump)


min(dat_bump$YearPublished, na.rm = T)
max(dat_bump$YearPublished, na.rm = T)


dat_bump <- ap_documents %>%
  as.data.frame() %>%
  na.omit(YearPublished) %>%
  mutate(decade = cut(YearPublished, 
                      breaks = c(1990, 1999, 2009, 2022), 
                      labels = c("d: 1990s", "d: 2000s", "d: 2010s")),
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

ggplot(dat_bump, aes(decade, MnGamma)) +
  geom_line() +
  facet_wrap(~topic2)

dat_bump <- dat_bump %>%
  arrange(rank)

lab <- dat_bump %>%
  group_by(topic2) %>%
  filter(decade == 1) %>%
  mutate(lab = rank) %>%
  select(topic2, lab)

dat_bump <- left_join(dat_bump, lab)

xlabs <- c("1990-1999", "2000-2009", "2010-2021")

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



ggpubr::ggarrange(p2, p1, 
                  labels = c("A", "B"),
                  ncol = 1, nrow = 2, heights = c(1.4,1))


ggsave("figures/lda_topic_modelling.png", width = 20, height = 25, units = "cm")



# Network of the Word Distributions Over Topics (Topic Relation)
# based on probability a word is associated with a given topic

# https://github.com/trinker/topicmodels_learning
post <- topicmodels::posterior(ap_lda)

cor_mat <- cor(t(post[["terms"]]))
cor_mat[ cor_mat < .3] <- 0
diag(cor_mat) <- 0

graph <- graph.adjacency(cor_mat, weighted = TRUE, mode = "lower")

E(graph)$edge.width <- E(graph)$weight * 10
V(graph)$label <- paste("Topic", V(graph))
V(graph)$size <- colSums(post[["topics"]]) * 15
V(graph)$deg <- degree(graph) 


lay = ggraph::create_layout(graph, layout = "fr")

ggraph::ggraph(lay) + 
  ggraph::geom_edge_link(ggplot2::aes(edge_width = edge.width), edge_colour = "grey") + 
  ggraph::geom_node_point(ggplot2::aes(size = deg), colour = "firebrick", shape = 16, 
                    alpha = 0.9) +
  ggplot2::scale_size_continuous(range = c(3, 15)) + 
  ggraph::geom_node_text(ggplot2::aes(label = name)) +
  ggplot2::ggtitle("Strength Between Topics Based On Word Probabilities") +
  ggraph::theme_graph() +
  ggplot2::theme(legend.position = "none")

  ggsave("figures/lda_topic_relations.png", width = 20, height = 20, units = "cm")
  
