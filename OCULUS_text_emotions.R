# Step 1 – Install and load the required packages.
pacman::p_load(dplyr, ggplot2, tidytext, wordcloud2, tidyr, dplyr, ggplot2, textdata,
               tidyverse)

oculus_orig <- read.csv(file.choose(), stringsAsFactors = FALSE)
names(oculus_orig)

# select only the comments and titles from the data and examine it
quest <-oculus_orig %>% select(comments, title)
glimpse(quest)

# Step 2 – Make a list of words that we do not want to bias our analyses
#customize your remove words based on the names, common words, etc.
remove_words <- c("quest", "oculus","quest2","facebook")

# Step 3 – Delete these words from the comments
#unnest and remove stop, undesirable and short words
oculus_words_filtered <- quest %>%
  unnest_tokens(word, comments) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  filter(!word %in% remove_words) %>%
  filter(nchar(word) > 3)

# Step 4 – full word count 
# Step 4a – get the full word count from the comments 
oculus_comments_word_count <- quest %>%
  unnest_tokens(word, comments) %>%
  group_by(title) %>%
  summarise(num_words = n()) %>%
  arrange(desc(num_words))
# examine the distribution of words per song
View(oculus_comments_word_count)

# Step 4b –  full word count (filtered)
oculus_word_summary <- oculus_words_filtered %>%
  group_by(title) %>%
  mutate(word_count = n_distinct(word)) %>%
  select(title, word_count) %>%
  distinct() %>% 
  ungroup()
View(oculus_word_summary)

# Step 5 – plot the most commonly used words in the comments

oculus_words_filtered %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot() +
  geom_col(aes(word, n)) +
  xlab("") +
  ylab("Word Count") +
  ggtitle("Most Frequently Used Words in Oculus Review") +
  coord_flip()

# Step 6 – create a wordcloud of the words in the comments 
oculus_word_counts <- oculus_words_filtered %>% count(word, sort = TRUE)
wordcloud2(oculus_word_counts[1:300, ], size = .5)

# Step 7  – word list of the three major lexicons

bing <- get_sentiments("bing") %>% mutate(lexicon = "bing", words_in_lexicon =
                                            n_distinct(word))
View(bing)
nrc <- get_sentiments("nrc") %>% mutate(lexicon = "nrc", words_in_lexicon =
                                          n_distinct(word))
View (nrc)
afinn <- get_sentiments("afinn") %>% mutate(lexicon = "afinn", words_in_lexicon =
                                              n_distinct(word))
View (afinn)


# Step 8 – combine the data from all three major lexicons
new_sentiments <- bind_rows(bing, nrc, afinn)
#check out the data frame created.
View(new_sentiments)
#create a listing for us to review that summarizes all the lexicon data
new_sentiments %>% group_by(lexicon, sentiment, words_in_lexicon) %>%
  summarise(distinct_words = n_distinct(word)) %>%
  ungroup() %>%
  spread(sentiment, distinct_words)
#view this data frame for you to get an idea of how these are implemented
View(new_sentiments)

# Step 9 –  inner-join to get sentiments for filtered words and plot

# 9a  bing
oculus_bing <- oculus_words_filtered %>% inner_join(get_sentiments("bing"))
ggplot(oculus_bing, aes(x = sentiment)) +
  geom_bar()
# 9b – next is NRC, which will provide emotions on top of + and -
oculus_nrc <- oculus_words_filtered %>% inner_join(get_sentiments("nrc"))
ggplot(oculus_nrc, aes(x = sentiment)) +
  geom_bar()
# 9c – AFINN is different from bing in that it provides 5 levels of + and -
oculus_afinn <- oculus_words_filtered %>% inner_join(get_sentiments("afinn"))
ggplot(oculus_afinn, aes(x = value)) +
  geom_bar()

#Step 10 - Lexical Diversity over the years
# tidy text version of the data 

oculus_tidy <- oculus_orig %>%
  unnest_tokens(word, comments) %>%
  filter(!word %in% remove_words) %>%
  filter(!nchar(word) < 3) %>%
  anti_join(stop_words)
# calculate the number of words per comment removing stop words 
oculus_lexical_diversity <- oculus_tidy %>%
  mutate(year = ifelse(is.na(year),"NONE", year)) %>%
  group_by(year, track_title) %>%
  mutate(word_count = n_distinct(word)) %>%
  select(title, year, word_count) %>%
  distinct() %>% 
  ungroup()
# display a beautiful pirate plot of the distribution of words over the years
pirateplot(formula = word_count ~ year, #Formula
           data = oculus_lexical_diversity, #Data frame we just created
           xlab = NULL, ylab = "Song Distinct Word Count", #Axis labels
           main = "Lexical Diversity Per Year", #Plot title
           pal = "google", #Color scheme
           point.o = .2, #Points
           avg.line.o = 1, #Turn on the Average/Mean line
           theme = 0, #Theme Change these from 1 to 5 to see different display themes
           point.pch = 16, #Point `pch` type
           point.cex = 1.5, #Point size
           jitter.val = .1, #Turn on jitter to see the songs better
           cex.lab = .9, cex.names = .7) #Axis label size