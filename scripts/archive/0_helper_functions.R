# plot networks of btm output
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
    ggraph::geom_node_text(ggplot2::aes(label = name, size = probability), 
                           col = "black", repel = TRUE) +
    ggplot2::theme_void() + 
    ggplot2::theme(legend.position = "none") +
    ggforce::geom_mark_hull(ggplot2::aes(x, y, group = topic,label = topic), 
                            color = NA)
}
