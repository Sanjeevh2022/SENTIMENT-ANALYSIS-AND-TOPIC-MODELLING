# Step 1 – add the required libraries and the data 
# Install and load the required packages.
pacman::p_load(tidyr, dplyr, stringr, data.table, sentimentr, ggplot2, text2vec, tm, ggrepel)

# load the data 
reviews_all = read.csv(file.choose(), stringsAsFactors = F)

# create a rowid for the reviews
review_df <- reviews_all %>% mutate(id = row_number())

# examine the structure
str(reviews_all)

# Step 2 – define the lexicon 
nrow(lexicon::hash_sentiment_jockers_rinker) 


# words to replace 
replace_in_lexicon <- tribble(
  ~x, ~y,
  "oculus", 0, 
  "quest 2", 0, 
  "quest2", 0, 
  
)
# create a new lexicon with modified sentiment
review_lexicon <- lexicon::hash_sentiment_jockers_rinker %>%
  filter(!x %in% replace_in_lexicon$x) %>%
  bind_rows(replace_in_lexicon) %>%
  setDT() %>%
  setkey("x")

# Step 3 – get sentence-level sentiment
sent_df <- review_df %>%
  get_sentences() %>%
  sentiment_by(by = c('id', 'prod', 'author', 'date', 'stars','comments'), polarity_dt =
                 review_lexicon)

# Step 4 – relationship between star rating and sentiment
ggplot(sent_df, aes(x = stars, y = ave_sentiment, color = factor(stars), group = stars)) +
  geom_boxplot() +
  geom_hline(yintercept=0, linetype="dashed", color = "red") +
  geom_text(aes(5.2, -0.05, label = "Neutral Sentiment", vjust = 0), size = 3, color = "red")
+
  guides(color = guide_legend(title="Star Rating")) +
  ylab("Average Sentiment") +
  xlab("Review Star Rating") +
  ggtitle("Sentiment of Oculus Quest 2 Reviews, by Star Rating")

# Step 4 –GloVe (Global Vectors for Word Representation)
# create lists of reviews split into individual words (iterator over tokens)
tokens <- space_tokenizer(reviews_all$comments %>% tolower() %>%
                            removePunctuation())
# Create vocabulary as unigrams 
it <- itoken(tokens, progressbar = FALSE)
vocab <- create_vocabulary(it)
# prune (remove) words that appear less than 3 times
vocab <- prune_vocabulary(vocab, term_count_min = 3L)
# Use our filtered vocabulary
vectorizer <- vocab_vectorizer(vocab)
# use skip gram window of 5 for context words
tcm <- create_tcm(it, vectorizer, skip_grams_window = 5L)
# fit the model. It can take several minutes based on how much data you have
glove = GloVe$new(rank = 100, x_max = 5)
glove$fit_transform(tcm, n_iter = 20)
# get the processed word vector
word_vectors = glove$components

# Step 5 – check which words have contextual similarity

# check for oculus 
oculus <- word_vectors[, "oculus", drop = F]
# cosine similarity between word vectors tells us how similar they are
cos_sim = sim2(x = t(word_vectors), y = t(oculus), method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 20)

# Step 6 – t-SNE to visualize reviews by similarity of words
# load packages
pacman::p_load(tm, Rtsne, tibble, tidytext, scales)
#create vector of words to keep, before applying tsne (remove stop words)
keep_words <- setdiff(colnames(word_vectors), stopwords())
# keep words in vector
word_vec <- word_vectors[, keep_words]
# prepare data frame to train
train_df <- data.frame(t(word_vec)) %>% rownames_to_column("word")
# train tsne for visualization
tsne <- Rtsne(train_df[,-1], dims = 2, perplexity = 50, verbose=TRUE, max_iter = 500)

#Step 7 – plot t-SNE and examine

colors = rainbow(length(unique(train_df$word)))
names(colors) = unique(train_df$word)
plot_df <- data.frame(tsne$Y) %>% mutate(
  word = train_df$word,
  col = colors[train_df$word]
) %>% left_join(vocab, by = c("word" = "term")) %>%
  filter(doc_count >= 20)
ggplot(plot_df, aes(X1, X2)) +
  geom_text(aes(X1, X2, label = word, color = col), size = 3) +
  xlab("") + ylab("") +
  theme(legend.position = "none")

review_df1 <- sent_df %>%
  select(id,comments,ave_sentiment)

# Step 8 – calculate word level sentiment and overlay these on the t-SNE
# calculate word-level sentiment
word_sent <- review_df %>%
  left_join(sent_df, by = "id") %>%
  select(id, comments.y, ave_sentiment) %>%
  unnest_tokens(word, comments.y) %>%
  group_by(word) %>%
  summarise(
    count = n(),
    avg_sentiment = mean(ave_sentiment),
    sum_sentiment = sum(ave_sentiment),
    sd_sentiment = sd(ave_sentiment)
  ) %>%
  anti_join(stop_words, by = "word")
# filter to words that appear at least 5 times
pd_sent <- plot_df %>%
  left_join(word_sent, by = "word") %>%
  drop_na() %>%
  filter(count >= 7)

# Step 9 – Plot the results
ggplot(pd_sent, aes(X1, X2)) +
  geom_point(aes(X1, X2, size = count, alpha = .1, color = avg_sentiment)) +
  geom_text(aes(X1, X2, label = word), size = 2) +
  scale_colour_gradient2(low = muted("red"), mid = "white",
                         high = muted("blue"), midpoint = 0) +
  scale_size(range = c(5, 20)) +
  xlab("") + ylab("") +
  ggtitle("2-dimensional t-SNE Mapping of Word Vectors") +
  guides(color = guide_legend(title="Avg. Sentiment"), size = guide_legend(title =
                                                                             "Frequency"), alpha = NULL) +
  scale_alpha(range = c(1, 1), guide = "none")
