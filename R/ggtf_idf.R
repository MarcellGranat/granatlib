#' @title ggtf_idf
#'
#' @description Draws a graph of the bigrams, which contain at least one of the words with the highest TF-IDF value.
#' @param x A data.frame containing one fct column and one column of text. Levels of the fct column gives the categories for the TF-IDF calculation, but only the first and the last level will appear on the plot
#'
#' @return Ggobject \code{overview_print}
#' @examples
#' plot3D()
#' @export
#'

ggtf_idf <- function(x,
                     stem_function = function(x) corpus::stem_snowball(x, "hu"),
                     n = 10,
                     language = "hu",
                     drop_words = stopwords::stopwords("hu"),
                     min_bigram_n = 1,
                     drop_numbers = TRUE
) {
  library(tidytext)
  library(igraph)
  library(ggraph)

  if (!is.factor(x[[1]]) | !is.character(x[[2]])) {
    stop(call. = FALSE, "First column must be a factor, second column must contain the text to analyse.")
  }

  if (!is.null(stem_function)) {
    stem_function = function(x) corpus::stem_snowball(x, language)
  }

  if (all.equal(drop_words, stopwords::stopwords("hu"))) {
    drop_words = stopwords::stopwords(language)
  }

  neutral <- ifelse(language == "hu", "Neutrális", "Neutral")

  x <- x %>%
    select(1:2) %>%
    set_names("g", "text")

  lev <- x %>%
    arrange(desc(g)) %>%
    pull(g) %>%
    levels() %>%
    {c(first(.), neutral, last(.))}

  word_df <- x %>%
    unnest_tokens(word, text) %>%
    set_names("doc", "term")

  if (!is.null(stem_function)) {
    word_df <- word_df %>%
      mutate_at(2, stem_function)
  }

  if (!is.null(drop_words)) {
    word_df <- word_df %>%
      filter(!(term %in% drop_words))
  }

  if (drop_numbers) {
    word_df <- word_df %>%
      filter(!str_detect(term, "\\d"))
  }

  tf_idf_df <- word_df %>%
    na.omit() %>%
    count(doc, term) %>%
    tidytext::bind_tf_idf(term, doc, n) %>%
    group_by(doc) %>%
    slice_max(tf_idf, n = n, with_ties = FALSE)

  bigram_df1 <- x %>%
    filter(g %in% first(lev)) %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    count(bigram) %>%
    separate(bigram, c("word1", "word2"), sep = " ")

  bigram_df2 <- x %>%
    filter(g %in% last(lev)) %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    count(bigram) %>%
    separate(bigram, c("word1", "word2"), sep = " ")

  if (!is.null(stem_function)) {
    bigram_df1 <- bigram_df1 %>%
      mutate_at(1:2, stem_function)

    bigram_df2 <- bigram_df2 %>%
      mutate_at(1:2, stem_function)
  }

  bigram_df1 <- bigram_df1%>%
    filter(word1 %in% filter(tf_idf_df, doc == first(lev))$term |
             word2 %in% filter(tf_idf_df, doc == first(lev))$term)

  bigram_df2 <- bigram_df2%>%
    filter(word1 %in% filter(tf_idf_df, doc == last(lev))$term |
             word2 %in% filter(tf_idf_df, doc == last(lev))$term)

  bigram_df <- bigram_df1 %>%
    bind_rows(bigram_df2) %>%
    filter(n >= min_bigram_n)

  f_colorise <- function(x) {
    ifelse(x %in% filter(tf_idf_df, doc == first(lev))$term, first(lev),
           ifelse(x %in% filter(tf_idf_df, doc == last(lev))$term, last(lev), neutral)
    )
  }

  graph_from_data_frame(bigram_df) %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                   arrow = grid::arrow(type = "closed", length = unit(.15, "inches")),
                   end_cap = circle(.07, 'inches')) +
    geom_node_point(aes(fill = f_colorise(name)), size = 5, color = "black", shape = 21) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1, size = 3) +
    scale_fill_manual(
      breaks = fct_inorder(lev),
      values = c("#bc4749", "#f2e8cf", "#386641"), name = NULL)

}
