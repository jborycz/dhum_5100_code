# LIBRARIES
# install.packages(pacman)
library(pacman)
pacman::p_load(tm,            # text mining
               textmineR,     # text mining
               janeaustenr,   # Text of Jane Austen novels
               tidyverse,     # Data cleaning & plotting packages (includes ggplot2)
               tidytext,      # Sentiment and term frequency analysis
               forcats,       # Tools for working with categorical variables
               ggpubr)        # Makes publication ready plots

# Let’s start by looking at the published novels of Jane Austen and examine first term frequency 
# We can start just by using dplyr verbs such as group_by() and join().

###########################
#    Word frequencies     #
###########################
## Compute word frequencies in each Jane Austen novel
book_words <- austen_books() %>%   
  unnest_tokens(word, text) %>%   # unnest_tokens() flattens a table into one token per row
  count(book, word, sort = TRUE)  # count() counts the words in each book and sorts them

total_words <- book_words %>% 
  group_by(book) %>%              # group_by() combines portions of a dataframe into based on specified column
  summarize(total = sum(n))       # summarize() or mutate() can be used to compute results based on groups

book_words <- left_join(book_words, total_words) # left_join() combines tables while preserving data order

book_words
#> # A tibble: 40,379 × 4
#>    book              word      n  total
#>    <fct>             <chr> <int>  <int>
#>  1 Mansfield Park    the    6206 160460
#>  2 Mansfield Park    to     5475 160460
#>  3 Mansfield Park    and    5438 160460
#>  4 Emma              to     5239 160996

# Now let’s look at the distribution of n/total for each novel, the number of times a word 
# appears in a novel divided by the total number of terms (words) in that novel.
ggplot(book_words, aes(n/total, fill = book)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~book, ncol = 2, scales = "free_y")

###########################
#       Zipf's law        #
###########################
# Zipf’s law states that the frequency that a word appears is inversely proportional to its rank. 
# Since the table was already ordered by n, we could use row_number() to find the rank. 
# Let's make a log-log plot of Zipf's Law
freq_by_rank <- book_words %>% 
  group_by(book) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/total) %>%
  ungroup()

View(freq_by_rank)
#> # A tibble: 40,379 × 6
#>    book              word      n  total  rank `term frequency`
#>    <fct>             <chr> <int>  <int> <int>            <dbl>
#>  1 Mansfield Park    the    6206 160460     1           0.0387
#>  2 Mansfield Park    to     5475 160460     2           0.0341
#>  3 Mansfield Park    and    5438 160460     3           0.0339
#>  4 Emma              to     5239 160996     1           0.0325
#> # … with 40,369 more rows

# Let’s see what the exponent of the power law is for the middle section of the rank range.

freq_by_rank %>% 
  ggplot(aes(x= rank, y= `term frequency`, color = book)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

rank_subset <- freq_by_rank %>% 
  filter(rank < 500,
         rank > 10)

lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)
#> 
#> Call:
#> lm(formula = log10(`term frequency`) ~ log10(rank), data = rank_subset)
#> 
#> Coefficients:
#> (Intercept)  log10(rank)  
#>     -0.6226      -1.1125

# Let’s plot a power law (line on the log-log plot) with the data:
## Create theme for plots
plot_theme <- theme(legend.position = "bottom", 
                    legend.title = element_text(face="bold",size=24,hjust = 0.5),
                    legend.text = element_text(size=20,hjust = 0.5),
                    plot.title = element_text(face="bold",size=24,hjust = 0.5),
                    strip.text = element_text(size=16,hjust = 0.5),
                    axis.text.x = element_text(color="black",size=16, angle=0),
                    axis.title.x = element_text(face="bold",color="black",size=20, angle=0),
                    axis.text.y = element_text(color="black",size=16, angle=0),
                    axis.title.y = element_text(face="bold",color="black",size=20, angle=90))

## Plot Zipf's law
freq_by_rank_plot <- freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = book)) + 
  geom_abline(intercept = -0.62, slope = -1.1, 
              color = "gray50", linetype = 3, size=1.5) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = TRUE) + 
  labs(color="Book") + 
  scale_x_log10() +
  scale_y_log10() +
  theme_classic() + plot_theme

ggexport(plotlist = list(freq_by_rank_plot), 
         filename = "plots/freq_by_rank_plot.png",width=1000,height = 1000)

# The statistic tf-idf is intended to measure how important a word is to a document in a collection (or corpus) of documents. It finds the important words  by decreasing the weight for commonly used words and increasing the weight for words that are not used very much in a collection or corpus of documents (e.g. Jane Austen’s novels as a whole).


# The bind_tf_idf() function in the tidytext package takes a tidy text dataset as input 
# with one row per token (term), per document. One column (word here) contains the terms/tokens, 
# one column contains the documents (book in this case), and the last necessary column contains 
# the counts, how many times each document contains each term (n in this example). 
book_tf_idf <- book_words %>%
  bind_tf_idf(word, book, n)

book_tf_idf
#> # A tibble: 40,379 × 7
#>    book              word      n  total     tf   idf tf_idf
#>    <fct>             <chr> <int>  <int>  <dbl> <dbl>  <dbl>
#>  1 Mansfield Park    the    6206 160460 0.0387     0      0
#>  2 Mansfield Park    to     5475 160460 0.0341     0      0
#>  3 Mansfield Park    and    5438 160460 0.0339     0      0

#> # … with 40,369 more rows

# Now, let's look at terms with high tf-idf in Jane Austen’s works. Note Some of the values for
# idf are the same for different terms because there are 6 documents in this corpus and we 
# are seeing the same numerical values for the statistic. 

book_tf_idf %>%
  select(-total) %>%
  arrange(desc(tf_idf))
#> # A tibble: 40,379 × 6
#>    book                word          n      tf   idf  tf_idf
#>    <fct>               <chr>     <int>   <dbl> <dbl>   <dbl>
#>  1 Sense & Sensibility elinor      623 0.00519  1.79 0.00931
#>  2 Sense & Sensibility marianne    492 0.00410  1.79 0.00735
#> # … with 40,369 more rows


# Now, Let’s look at a visualization for these high tf-idf words
# Plot most important words
book_tf_idf_plot <- book_tf_idf %>%
  group_by(book) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = "Term Frequency") + plot_theme

ggexport(plotlist = list(book_tf_idf_plot), 
         filename = "plots/book_tf_idf_plot.png",width=800,height = 1000)

###########################
#    Sentiment analysis   #
###########################
# Upload books to dataframe and split by chapter
tidy_books <- austen_books() %>%
  group_by(book) %>%
  mutate(
    linenumber = row_number(),
    chapter = cumsum(str_detect(text, 
                                regex("^chapter [\\divxlc]", 
                                      ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)

# Choose sentiment lexicon and type, select a book and join dataframes
nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

tidy_books %>%
  filter(book == "Emma") %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)

# Separate book into lines and compute sentiments by line 
jane_austen_sentiment <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%   # Use bing sentiment lexicon 
  count(book, index = linenumber %/% 80, sentiment) %>% # Compute sentiment for each line
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%  # Use pivot_wider() so that we have negative and positive sentiment in separate columns
  mutate(sentiment = positive - negative)

# Plot sentiments for each book
austen_sentiments_plot <- ggplot(jane_austen_sentiment, aes(index, sentiment, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x") + plot_theme

ggexport(plotlist = list(austen_sentiments_plot), 
         filename = "plots/austen_sentiments_plot.png",width=800,height = 1000)
