library(tidytext)
library(dplyr)
library(ggplot2)
library(wordcloud)
library(reshape2)

create_tidy_df <- function(df, text_column = 'text', remove_stop_words=TRUE) {
  res <- df %>% 
    mutate(document = row_number()) %>% 
    tidytext::unnest_tokens('word', !!text_column) 
  
  if(remove_stop_words)
    res <- anti_join(res, stop_words, by='word')
  
  res
}


plot_word_freq <- function(df, n_words = 20, tf_idf=FALSE) {
  title <- paste0("Top ", n_words, " words by frequency")
  
  if(!tf_idf) {
    df %>% 
      count(word, sort=TRUE) %>% 
      dplyr::top_n(n_words, wt=n) %>% 
      dplyr::mutate(word = reorder(word, n)) %>% 
      ggplot(aes(word, n)) + 
      geom_col() +
      coord_flip() + 
      labs(y = "Word Frequency", x = NULL, title = title)
  } else
  {
    df %>% 
      count(document, word, sort = TRUE) %>% 
      bind_tf_idf(word, document, n) %>% 
      arrange(desc(tf_idf)) %>% 
      mutate(word = factor(word, levels = rev(unique(word)))) %>% 
      top_n(15) %>% 
      ungroup() %>% 
      ggplot(aes(word, tf_idf)) +
      geom_col() +
      coord_flip() +
      scale_y_continuous(labels=scales::percent, name = "TF/IDF Frequency") +
      labs(x=NULL, title=title)
  }
  
}

plot_sentiment <- function(df, n_words=15) {

  sen_df <- df %>% 
    inner_join(get_sentiments("afinn")) %>% 
    filter(score!=0) %>% 
    mutate(sentiment = ifelse(score>0,"positive","negative")) %>% 
    count(sentiment, word, wt=score, sort=TRUE) 
  
  top_sen <- top_n(sen_df, n = floor(n_words/2), wt = n)  %>% 
    union(top_n(sen_df, n = floor(n_words/2), wt = -n)) %>% 
    arrange(desc(n))
  
  top_sen %>% 
    mutate(word = reorder(word, n)) %>% 
    ggplot(aes(word,n, fill=sentiment)) +
    geom_col() +
    coord_flip()

}


plot_word_cloud <- function(df) {
  df %>%
    inner_join(get_sentiments("bing"), by='word') %>%
    count(word, sentiment, sort = TRUE) %>%
    acast(word ~ sentiment, value.var = "n", fill = 0) %>%
    comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                     max.words = 100)
}


plot_topic_model <- function(df, topics = 4, n_words=10) {
  wc_df <- df %>% 
    count(document, word, sort=TRUE) 
  
  dtm_df <- cast_dtm(wc_df, document, word, n)
  topic_lda <- LDA(dtm_df, k = topics)
  topics <- tidy(topic_lda, matrix = "beta")
  
  top_terms <- topics %>% 
    group_by(topic) %>% 
    top_n(n_words, beta) %>% 
    ungroup() %>% 
    arrange(topic, -beta)
  
  roles <- function(x) sub("[^_]*_","",x )   
  
  top_terms %>% 
    mutate(term = paste0(topic, '_', term),
           topic = paste0("Topic ", topic)) %>% 
    arrange(topic, -beta) %>% 
    mutate(term = reorder(term, beta)) %>% 
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip() +
    scale_x_discrete(labels=roles) + 
    scale_y_continuous(labels=NULL) +
    labs(title="Topics")
}




