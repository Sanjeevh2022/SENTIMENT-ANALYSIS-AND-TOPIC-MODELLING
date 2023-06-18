# Step 1 – Install and load the required packages.
pacman::p_load(dplyr, ggplot2, stringr, udpipe, lattice)

comments <- read.csv(file.choose(), stringsAsFactors = F)
head(comments)
str(comments)

udmodel_english <- udpipe_load_model(file = "english-ewt-ud-2.5-191206.udpipe")

# Step 2 – count the number of total comments by date and plot the results

library(lubridate)

comments$date <- mdy(comments$date)

comments$month <- month(comments$date)
comments$day <- day(comments$date)
comments$year <- year(comments$date)

comments %>% group_by(month, year) %>% count() %>% arrange(desc(n))

# Step 3 – extract comments for 2020, 2021

comments_2020 <- comments %>% filter(year == 2020)
comments_2021 <- comments %>% filter(year == 2021)

# Step 4 – use udpipe to annotate the comments
s <- udpipe_annotate(udmodel_english, comments$comments)
x <- data.frame(s)
s1 <- udpipe_annotate(udmodel_english, comments_2021$comments)
x1 <- data.frame(s)

# Step 5 – extract and display frequencies for universal parts of speech (upos) in text
stats <- txt_freq(x$upos)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = stats, col = "red",
         main = "UPOS (Universal Parts of Speech)\n frequency of occurrence",
         xlab = "Freq")

# Step 6 – extract and display most occurring NOUNS
stats <- subset(x, upos %in% c("NOUN"))
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue",
         main = "Most occurring nouns", xlab = "Freq")

# Step 7 –extract and display most occurring ADJECTIVES
stats <- subset(x, upos %in% c("ADJ"))
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "purple",
         main = "Most occurring adjectives", xlab = "Freq")

# Step 7 –extract and display most occurring VERBS
stats <- subset(x, upos %in% c("VERB"))
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "gold",
         main = "Most occurring Verbs", xlab = "Freq")

# Step 8 – RAKE (Rapid Automatic Keyword Extraction algorithm)
stats <- keywords_rake(x = x, term = "lemma", group = "doc_id",
                       relevant = x$upos %in% c("NOUN", "ADJ"))
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ rake, data = head(subset(stats, freq > 3), 20), col = "red",
         main = "Keywords identified by RAKE",
         xlab = "Rake")

# Step 9 - extract top phrases
## display by plot a sequence of POS tags (noun phrases / verb phrases)
x$phrase_tag <- as_phrasemachine(x$upos, type = "upos")
stats <- keywords_phrases(x = x$phrase_tag, term = tolower(x$token),
                          pattern = "(A|N)*N(P+D*(A|N)*N)*",
                          is_regex = TRUE, detailed = FALSE)
stats <- subset(stats, ngram > 1 & freq > 3)
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ freq, data = head(stats, 20), col = "magenta",
         main = "Keywords - simple noun phrases", xlab = "Frequency")

#n-gram >2
x$phrase_tag <- as_phrasemachine(x$upos, type = "upos")
stats <- keywords_phrases(x = x$phrase_tag, term = tolower(x$token), pattern = "(A|
N)*N(P+D*(A|N)*N)*", is_regex = TRUE, detailed = FALSE)
stats <- subset(stats, ngram >  2 & freq > 3)
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ freq, data = head(stats, 25), col = "light blue", main = "Keywords - simple
noun phrases", xlab = "Frequency")

#n-gram >3
x$phrase_tag <- as_phrasemachine(x$upos, type = "upos")
stats <- keywords_phrases(x = x$phrase_tag, term = tolower(x$token), pattern = "(A|
N)*N(P+D*(A|N)*N)*", is_regex = TRUE, detailed = FALSE)
stats <- subset(stats, ngram >  3 & freq > 3)
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ freq, data = head(stats, 25), col = "light blue", main = "Keywords - simple
noun phrases", xlab = "Frequency")

#n-gram >4
x$phrase_tag <- as_phrasemachine(x$upos, type = "upos")
stats <- keywords_phrases(x = x$phrase_tag, term = tolower(x$token), pattern = "(A|
N)*N(P+D*(A|N)*N)*", is_regex = TRUE, detailed = FALSE)
stats <- subset(stats, ngram >  4 & freq > 2)
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ freq, data = head(stats, 25), col = "light blue", main = "Keywords - simple
noun phrases", xlab = "Frequency")


# Step 10 - Collocation identification – basically words following one another)
stats <- keywords_collocation(x = x, term = "token", group = c("doc_id", "paragraph_id",
                                                               "sentence_id"), ngram_max = 4)
## How frequently do words occur in the same sentence (nouns and adjectives)
stats <- cooccurrence(x = subset(x, upos %in% c("NOUN", "ADJ")), term = "lemma", group
                      = c("doc_id", "paragraph_id", "sentence_id"))
## Co-occurrences: How frequent do words follow one another
stats <- cooccurrence(x = x$lemma, relevant = x$upos %in% c("NOUN", "ADJ"))
## Co-occurrences: How frequent do words follow one another if we would ## skip 2 words in between. 
stats <- cooccurrence(x = x$lemma, relevant = x$upos %in% c("NOUN", "ADJ"), skipgram =
                        2)
head(stats)

# Step 11 –  to explore this visually
pacman::p_load(igraph, ggraph)
wordnetwork <- head(stats, 25)
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") + geom_edge_link(aes(width = cooc, edge_alpha =
                                                          cooc), edge_colour = "red") +
  geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
  theme_graph(base_family = "Arial Narrow") +
  theme(legend.position = "none") +
  labs(title = "Co-occurrences within 3 words distance", subtitle = "Nouns & Adjectives")
