library(tidyverse)
library(tidytext)
library(ggplot2)
library(textstem)

data <- read.csv('./sa_expl.csv', 
                 col.names = c('idx', 'label', 'text'))

# Get the counts for pos words
positive_words <- data %>% 
  filter(label == 'pos') %>% 
  unnest_tokens(output = word, input = text) %>%
  filter(!str_detect(word, '\\d')) %>% 
  anti_join(stop_words) %>%
  mutate(word = lemmatize_words(word)) %>% 
  count(word, sort = TRUE)

# Get the counts for neg words
negative_words <- data %>% 
  filter(label == 'neg') %>% 
  unnest_tokens(output = word, input = text) %>% 
  filter(!str_detect(word, '\\d')) %>% 
  anti_join(stop_words) %>% 
  mutate(word = lemmatize_words(word)) %>% 
  count(word, sort = TRUE)

# Join to put the counts of a word in negative and positive
# if the word appears in pos but not in neg (and vice versa),
# put a 0 as the value.
word_counts <- full_join(
  positive_words, 
  negative_words, 
  by = 'word', 
  suffix = c('_pos', '_neg')
  ) %>% 
  replace_na(list(n_pos=0, n_neg=0))

# total counts for each class
total_pos <- sum(word_counts$n_pos)
total_neg <- sum(word_counts$n_neg)

# alpha constant
alpha <- 1

# compute the log-odds and add it to a DF
log_odds <- word_counts %>% 
  filter(n_pos >= 10 & n_neg >= 10) %>% 
  mutate(
    log_odds = log((n_pos + alpha) / (total_pos + alpha)) - log((n_neg + alpha) / (total_neg + alpha))
  ) %>% 
  arrange(desc(log_odds))

top_pos <- head(log_odds, 30)
top_neg <- tail(log_odds, 30)

write.csv(top_neg, './neg_log_odds.csv', row.names = FALSE)
write.csv(top_pos, './pos_log_odds.csv', row.names = FALSE)
