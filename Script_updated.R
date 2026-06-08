# ASSIGNMENT - TEXT MINING AND SENTIMENT ANALYSIS

library(tidyverse)
library(rvest)
library(RSelenium)
library(netstat)
library(cld2)
library(tidytext)
library(wordcloud)

# PART 1: WEB SCRAPING - AMAZON REVIEWS
# Product chosen: "Atomic Habits" by James Clear
# URL: https://www.amazon.co.uk/product-reviews/1847941834/
# We scrape 100 reviews (title, text, stars) using RSelenium for dynamic content loading.

# SETUP RSELENIUM
rD = rsDriver(browser = "firefox",
               verbose = FALSE,
               port = free_port(),
               chromever = NULL,
               phantomver = NULL)
remDr = rD[["client"]]

# Navigate to Amazon UK reviews page
url = "https://www.amazon.co.uk/product-reviews/1847941834/"
remDr$navigate(url)

# LOGIN TO AMAZON
# Identify email field and type email
field = remDr$findElement(using = "css", "#ap_email")
email = "INSERT THE EMAil HERE"
field$sendKeysToElement(list(email))

# Click continue
click = remDr$findElement(using = "css", "#continue")
click$clickElement()

# Identify password field and type password
field = remDr$findElement(using = "css", "#ap_password")
pwd = "INSER THE PASSWORD HERE"
field$sendKeysToElement(list(pwd))

# Click sign in
click = remDr$findElement(using = "css", "#signInSubmit")
click$clickElement()

# LOAD REVIEWS DYNAMICALLY
# Create folder to save the HTML output
folder = "C:/Users/Marco Zubani/Documents/MAGISTRALE/SECONDO ANNO/SECONDO SEMESTRE/TEXT MINING/ASSIGNMENT/Amazon"
dir.create(folder)

#Do this for 100 reviews
pages = 11 #IT IS NOT POSSIBLE TO DO IT 20 or 25 TIMES!

# Click "show more" button repeatedly to load all reviews
for (i in 2:pages) {
  button = remDr$findElement(using = "css",
                              value = "[data-hook='show-more-button']")
  button$clickElement()
  Sys.sleep(3)  # Wait for the page to load
}

# Save the fully loaded HTML page
output = remDr$getPageSource(header = TRUE)
write(output[[1]], file = str_c(folder, "Amazon_reviews_", pages, ".html"))

# Close the browser connection
remDr$close()
rD$server$stop()


# PART 2: HTML SCRAPING - EXTRACT TITLE, TEXT, STARS

# Read the saved HTML file
html = read_html(str_c(folder, "Amazon_reviews_", pages, ".html"),
                  encoding = "utf-8")

# EXTRACT REVIEW TITLES
# UK reviews
title = html %>%
  html_elements("[class='a-size-base a-link-normal review-title a-color-base review-title-content a-text-bold']") %>%
  html_elements("span:nth-child(3)") %>%
  html_text2()

# Non-UK reviews (combined)
title = title %>% c(
  html %>%
    html_elements("[class = 'a-size-base review-title a-color-base review-title-content a-text-bold']") %>%
    html_text(trim = TRUE)
)

# EXTRACT REVIEW TEXT
# Same CSS class for UK and non-UK
text = html %>%
  html_elements("[class='a-size-base review-text review-text-content']") %>%
  html_text(trim = TRUE)

# EXTRACT STAR RATINGS
# UK reviews
star = html %>%
  html_elements("[data-hook='review-star-rating']") %>%
  html_text2()

# Non-UK reviews (combined)
star = star %>% c(
  html %>%
    html_elements("[data-hook='cmps-review-star-rating']") %>%
    html_text2()
)

# BUILD THE DATASET
data = tibble(title, text, star)

# Add document ID
data = data %>%
  mutate(id = seq_along(text))

View(data)

dim(data)
# PART 3: DATA CLEANING

# DETECT LANGUAGE AND KEEP ONLY ENGLISH
data$title_language = detect_language(data$title)
data$text_language  = detect_language(data$text)

# Two-way frequency table: language of title vs. language of text
table(text = data$text_language, title = data$title_language, useNA = "always")

# Keep only English reviews
data = data %>%
  filter(text_language == "en")

# EXTRACT NUMERIC SCORE FROM STARS
# The star column looks like "5.0 out of 5 stars"; extract the first character
data = data %>%
  mutate(score = as.numeric(str_sub(star, 1, 1)))

dim(data)
View(data)


# Save for later use
#saveRDS(data, file = "C:/Users/Marco Zubani/Documents/MAGISTRALE/SECONDO ANNO/SECONDO SEMESTRE/TEXT MINING/ASSIGNMENT/data.rds")
data = readRDS("C:/Users/Marco Zubani/Documents/MAGISTRALE/SECONDO ANNO/SECONDO SEMESTRE/TEXT MINING/ASSIGNMENT/data.rds")


# ADD STAR_SENT COLUMN
# Binary sentiment label based on star rating:
# "positive" = 4 or 5 stars, "negative" = 1, 2, or 3 stars

data = data %>%
  mutate(star_sent = ifelse(score >= 4, "positive", "negative"))

# Quick check
table(data$star_sent)
#View(data)


# Summary statistics of the score
data %>%
  summarise(
    mean   = mean(score, na.rm = TRUE),
    median = median(score, na.rm = TRUE),
    sd     = sd(score, na.rm = TRUE),
    min    = min(score, na.rm = TRUE),
    max    = max(score, na.rm = TRUE)
  )

# Frequency table of scores
data %>%
  count(score) %>%
  mutate(p = round(n / sum(n) * 100, 1))


# ADDITIONAL TEXT CLEANING 

clean_text = function(text) {
  text = str_replace_all(text, '([!?]){2,}', '\\1')         # normalise repeated ! or ?
  text = str_replace_all(text, '([\\.,!?=])(\\S)', '\\1 \\2') # add space after punctuation if missing
  text = str_replace_all(text, '(\\.\\s?){2,}', '... ')     # normalise ellipses
  text = str_replace_all(text, '([a-z])([A-Z])', '\\1 \\2') # add space between merged words
  trimws(text)
}

data = data %>%
  mutate(text = clean_text(text))

# Tidy tokenisation with stopwords and numbers removed
tidy_text = data %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  filter(!str_detect(word, '[0-9]'))

# Tidy tokenisation WITHOUT stopwords removed (for comparison)
tidy_text_long = data %>%
  unnest_tokens(word, text)


# PART 4: VISUALIZATIONS

# VISUALIZATION 1: Distribution of Star Ratings
data %>%
  ggplot(aes(x = score)) +
  geom_bar(fill = "steelblue") +
  labs(
    title    = "Distribution of Amazon Review \nStar Ratings",
    subtitle = "Atomic Habits by James Clear – Amazon UK",
    x        = "Stars",
    y        = "Number of Reviews"
  ) +
  theme_bw() +
  theme(
    plot.title    = element_text(color = "steelblue", size = 13, face = "bold"),
    plot.subtitle = element_text(color = "steelblue2")
  )

# VISUALIZATION 1b: Binary Sentiment Distribution (star_sent)
data %>%
  count(star_sent, sort = TRUE) %>%
  ggplot(aes(star_sent, n, fill = star_sent)) +
  geom_col(show.legend = FALSE) +
  ggtitle('True sentiment distribution with \n           two categories') +
  labs(x = "Sentiment", y = "Number of Reviews") +
  theme_minimal() +
  theme(plot.title = element_text(color = "steelblue", size = 13, face = "bold"))


# VISUALIZATION 2: Word Cloud of Most Frequent Words
tidy_text %>%
  count(word, sort = TRUE) %>%
  slice_max(order_by = n, n = 20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 20 Most Frequent Words \n in Reviews",
    x     = NULL,
    y     = "Frequency"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(color = "steelblue", size = 13, face = "bold")
  )


# PART 5: SENTIMENT ANALYSIS – BING LEXICON (TIDY APPROACH)

# LOAD BING LEXICON
bing = get_sentiments(lexicon = "bing")
bing

# Quick exploration
bing %>% count(sentiment)

# PIPELINE A: Document sentiment (Stopwords REMOVED)
data_bing = tidy_text %>%
  rename(doc_id = id) %>%
  select(doc_id, word) %>%
  inner_join(bing) %>%
  count(doc_id, sentiment) %>%
  pivot_wider(
    names_from  = sentiment,
    values_from = n,
    values_fill = 0
  ) %>%
  mutate(
    sentiment = positive - negative,
    method    = 'bing_no_stop'
  ) %>%
  select(doc_id, sentiment, method)

# PIPELINE B: Document sentiment (Stopwords RETAINED)
data_withbing = tidy_text_long %>%
  rename(doc_id = id) %>%
  select(doc_id, word) %>%
  inner_join(bing) %>%
  count(doc_id, sentiment) %>%
  pivot_wider(
    names_from  = sentiment,
    values_from = n,
    values_fill = 0
  ) %>%
  mutate(
    sentiment = positive - negative,
    method    = 'bing_with_stop'
  ) %>%
  select(doc_id, sentiment, method)

# Dimension check
dim(data)
dim(data_bing)
dim(data_withbing)
# data         = 94 rows: all English reviews after language filtering.
# data_bing    = 81 rows: 13 reviews are lost because, after stopwords removal,
#                none of their remaining tokens match any word in the Bing lexicon
#                (inner_join excludes them). This typically affects short or
#                informal reviews with vocabulary not covered by the dictionary.
# data_withbing = 91 rows: retaining stopwords increases token availability,
#                so 10 more reviews find at least one Bing match. Still, 3 reviews
#                remain unmatched even with stopwords included.
# Key insight: stopwords removal reduces coverage (fewer reviews classified)
#              but improves signal quality by avoiding sentiment-loaded stopwords
#              (e.g. "well", "like") that could distort the polarity score.

# COMBINE AND JOIN WITH ORIGINAL DATA
# Combine both pipelines vertically
# bind_rows() stacks data_bing and data_withbing, keeping the 'method' column
# to distinguish which preprocessing (with/without stopwords) produced each score.
bing_all_wide = bind_rows(data_bing, data_withbing) %>%
  # pivot_wider() transforms the long format into wide: creates separate columns
  # for bing_no_stop and bing_with_stop, one sentiment score per review.
  # values_fill = 0 imputes missing scores (reviews not matched by that pipeline)
  # as 0 — critical for later comparisons.
  pivot_wider(
    names_from  = method,
    values_from = sentiment,
    values_fill = 0
  ) %>%
  # inner_join() merges with the original data (doc_id = id) to restore all
  # features: score, star_sent, title, text, etc.
  inner_join(data, join_by('doc_id' == 'id'))

dim(bing_all_wide)
bing_all_wide


# CONVERT TO BINARY LABELS FOR VALIDATION
# Scores > 0 (positive) → "positive"
# Scores <=  0 (more negative words)  → "negative"
# Design choice: treating 0 as negative is a deliberate decision.
# A score of zero means no net sentiment signal: the review contains
# no sentiment words, or positive and negative cancel out exactly.
bing_all_wide = bing_all_wide %>%
  mutate(
    bing_nostop_lab  = as.factor(ifelse(bing_no_stop  > 0, 'positive', 'negative')),
    bing_withstop_lab = as.factor(ifelse(bing_with_stop > 0, 'positive', 'negative')),
    star_sent         = as.factor(star_sent)
  )


# VISUALIZATION: Stopwords effect on most frequent words

# Words contributing to sentiment WITH stopwords
word_bing = tidy_text_long %>%
  inner_join(bing)

word_bing %>%
  count(word, sentiment, sort = TRUE) %>%
  group_by(sentiment) %>%
  slice_head(n = 10) %>%
  mutate(word = reorder_within(word, n, sentiment)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("positive" = "green4", "negative" = "firebrick3")) +
  xlab(NULL) + ylab("Word count") +
  ggtitle("Most frequent sentiment words\n(WITH stopwords)") +
  facet_wrap(~ sentiment, scales = 'free_y') +
  coord_flip() +
  scale_x_reordered() +
  theme_bw() +
  theme(plot.title = element_text(color = "black", size = 12, face = "bold"))

# Words contributing to sentiment WITHOUT stopwords
word_nobing = tidy_text %>%
  inner_join(bing)

word_nobing %>%
  count(word, sentiment, sort = TRUE) %>%
  group_by(sentiment) %>%
  slice_head(n = 10) %>%
  mutate(word = reorder_within(word, n, sentiment)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("positive" = "green4", "negative" = "firebrick3")) +
  xlab(NULL) + ylab("Word count") +
  ggtitle("Most frequent sentiment words \n(WITHOUT stopwords)") +
  facet_wrap(~ sentiment, scales = 'free_y') +
  coord_flip() +
  scale_x_reordered() +
  theme_bw() +
  theme(plot.title = element_text(color = "black", size = 12, face = "bold"))


# COMPARATIVE DISTRIBUTION: predicted vs true labels
bing_all_wide %>%
  select(bing_nostop_lab, bing_withstop_lab, star_sent) %>%
  pivot_longer(everything(), names_to = 'method', values_to = 'sentiment') %>%
  count(method, sentiment) %>%
  ggplot(aes(sentiment, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ method) +
  scale_fill_manual(values = c('positive' = 'green4', 'negative' = 'firebrick3')) +
  theme_minimal()


# CONFUSION MATRICES: Tidy approach
library(caret)

cm_bing_true  = confusionMatrix(bing_all_wide$bing_nostop_lab,  bing_all_wide$star_sent)
cm_bing2_true = confusionMatrix(bing_all_wide$bing_withstop_lab, bing_all_wide$star_sent)

cm_bing_true
cm_bing2_true

# Graphical confusion matrices
cm_t  = as.data.frame(as.table(t(cm_bing_true$table)))
cm_t2 = as.data.frame(as.table(t(cm_bing2_true$table)))
colnames(cm_t)  = c('Actual', 'Predicted', 'Freq')
colnames(cm_t2) = c('Actual', 'Predicted', 'Freq')

ggplot(cm_t, aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile(color = 'white') +
  geom_text(aes(label = Freq), size = 5) +
  scale_fill_gradient(low = 'azure', high = 'dodgerblue3') +
  theme_minimal() +
  labs(title = 'Confusion matrix: Bing \n(no stopwords) vs true labels',
       x = 'Predicted', y = 'True label') +
  scale_x_discrete(limits = c('negative', 'positive')) +
  scale_y_discrete(limits = c('positive', 'negative'))

ggplot(cm_t2, aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile(color = 'white') +
  geom_text(aes(label = Freq), size = 5) +
  scale_fill_gradient(low = 'azure', high = 'dodgerblue3') +
  theme_minimal() +
  labs(title = 'Confusion matrix: Bing \n(with stopwords) vs true labels',
       x = 'Predicted', y = 'True label') +
  scale_x_discrete(limits = c('negative', 'positive')) +
  scale_y_discrete(limits = c('positive', 'negative'))


# PART 6: SENTIMENT ANALYSIS – UDPIPE APPROACH
library(udpipe)

# Annotate the corpus with UDPipe (English GUM model)
output = data %>%
  rename(doc_id = id) %>%
  udpipe('english-gum')

# Rebuild Bing dictionary in UDPipe format (binary: 1 positive, -1 negative)
bing_dict = get_sentiments('bing') %>%
  mutate(sentiment = ifelse(sentiment == 'negative', -1, 1)) %>%
  rename(term = word, polarity = sentiment)

bing_dict


# UDBING: simple UDPipe approach (no amplifiers/negators)
udbing = txt_sentiment(
  x               = output,
  term            = 'lemma',
  polarity_terms  = bing_dict,
  polarity_negators    = NULL,
  polarity_amplifiers  = NULL,
  amplifier_weight     = 0.8,
  n_before  = 0,
  n_after   = 0,
  constrain = FALSE
)

#view(udbing$overall)
#view(udbing$data)

# Explore most frequent adverbs to choose amplifiers/deamplifiers
as.data.frame(udbing$data) %>%
  filter(upos == 'ADV') %>%
  count(lemma, sort = TRUE)%>%
  head(50)


# UDPIPE: with negators, amplifiers and deamplifiers
udpipe_sent = txt_sentiment(
  x              = output,
  term           = 'lemma',
  polarity_terms = bing_dict,
  polarity_negators     = c('no', 'not'),
  polarity_amplifiers   = c('very', 'so', 'really', 'highly', 'extremely', 'completely', 'absolutely', 'much'),#no just here, it depends by context!
  polarity_deamplifiers = c('quite', 'rather', 'almost', 'merely'),
  amplifier_weight = 0.8,
  n_before  = 3,
  n_after   = 3,
  constrain = FALSE
)

dim(udpipe_sent$overall)

# Add UDPipe sentiment scores to the main dataset
data$udbing  = udbing$overall$sentiment_polarity
data$udpipe  = udpipe_sent$overall$sentiment_polarity

glimpse(data)


# CONFUSION MATRICES: UDPipe approach
udpipe_var = data %>%
  select(star_sent, udbing, udpipe) %>%
  mutate(
    udbing_lab = as.factor(ifelse(udbing > 0, 'positive', 'negative')),
    udpipe_lab = as.factor(ifelse(udpipe > 0, 'positive', 'negative')),
    star_sent  = as.factor(star_sent)
  )

cm_udbing_true = confusionMatrix(udpipe_var$udbing_lab, udpipe_var$star_sent)
cm_udpipe_true = confusionMatrix(udpipe_var$udpipe_lab, udpipe_var$star_sent)

cm_udbing_true
cm_udpipe_true

# Graphical confusion matrices
cm_df_udbing   = as.data.frame(as.table(t(cm_udbing_true$table)))
cm2_df_udpipe  = as.data.frame(as.table(t(cm_udpipe_true$table)))
colnames(cm_df_udbing)  = c('Actual', 'Predicted', 'Freq')
colnames(cm2_df_udpipe) = c('Actual', 'Predicted', 'Freq')

ggplot(cm_df_udbing, aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile(color = 'white') +
  geom_text(aes(label = Freq), size = 5) +
  scale_fill_gradient(low = 'azure', high = 'dodgerblue3') +
  theme_minimal() +
  labs(title = 'Confusion matrix: UDbing vs true labels', x = 'Predicted', y = 'True label') +
  scale_x_discrete(limits = c('negative', 'positive')) +
  scale_y_discrete(limits = c('positive', 'negative'))

ggplot(cm2_df_udpipe, aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile(color = 'white') +
  geom_text(aes(label = Freq), size = 5) +
  scale_fill_gradient(low = 'azure', high = 'dodgerblue3') +
  theme_minimal() +
  labs(title = 'Confusion matrix: UDpipe vs true labels', x = 'Predicted', y = 'True label') +
  scale_x_discrete(limits = c('negative', 'positive')) +
  scale_y_discrete(limits = c('positive', 'negative'))


# PART 7: FINAL SYNTHESIS – ALL METHODS COMPARED

# Add Tidy approach scores back to main dataset
data = data %>%
  left_join(
    bing_all_wide %>% select(doc_id, bing_no_stop, bing_with_stop),
    join_by('id' == 'doc_id')
  ) %>%
  mutate(
    bing_no_stop   = ifelse(is.na(bing_no_stop),   0, bing_no_stop),
    bing_with_stop = ifelse(is.na(bing_with_stop), 0, bing_with_stop)
  )

data

# Normalised distribution comparison across all 4 methods
data_long = data %>%
  mutate(
    udbing_s         = as.numeric(scale(udbing)),
    udpipe_s         = as.numeric(scale(udpipe)),
    bing_no_stop_s   = as.numeric(scale(bing_no_stop)),
    bing_with_stop_s = as.numeric(scale(bing_with_stop))
  ) %>%
  select(udbing_s, udpipe_s, bing_with_stop_s, bing_no_stop_s) %>%
  pivot_longer(everything(), names_to = 'method', values_to = 'normalised_sentiment')

data_long %>%
  ggplot(aes(normalised_sentiment)) +
  geom_histogram(fill = 'navy', color = 'white', bins = 30) +
  facet_wrap(~ method, nrow = 1) +
  theme_minimal() +
  labs(
    title = 'Comparison of sentiment normalised distributions',
    x     = 'Z-score sentiment',
    y     = 'Frequency'
  )


# PART 8: SUPERVISED MACHINE LEARNING – NAIVE BAYES
# We implement a supervised probabilistic classifier (Naive Bayes) to perform
# sentiment analysis. The labelled dataset is built from the product star ratings
# recoded into two categories (star_sent: "positive" = 4-5 stars,
# "negative" = 1-3 stars), which we already created in PART 1.
#
# Two assumptions hold for the model:
#  - BAG OF WORDS: only words and their frequencies matter, not position/syntax.
#  - CONDITIONAL INDEPENDENCE: each word is independent of the others given the class.

# We implement the approach using quanteda (dfm format, not the tidy format)
require(quanteda)
require(quanteda.textmodels)
library(caret)

# BUILD THE LABELLED CORPUS
# Select only the relevant columns: id, text and the true label (star_sent)
glimpse(data)
data_q = data %>%
  select(id, text, star_sent)

corpus = corpus(data_q, text_field = 'text')

# Tokenise: remove punctuation, numbers, stopwords, then stem the words
corpus = tokens(corpus,
                remove_punct  = TRUE,
                remove_number = TRUE) %>%
  tokens_remove(pattern = stopwords('en')) %>%
  tokens_wordstem()

# Transform the tokens into the document-feature matrix (dfm)
dfm = dfm(corpus)
dfm

# TRAIN / TEST SPLIT
# We split the data to evaluate the efficacy of the model (~70% train, ~30% test)
set.seed(6272)
n_docs   = nrow(data)
id_train = sample(seq_len(n_docs), round(0.7 * n_docs), replace = FALSE)
head(id_train, 10)

# Training set
dfm_train = dfm[id_train, ]

# Test set
dfm_test = dfm[-id_train, ]

# NAIVE BAYES IMPLEMENTATION
# textmodel_nb is trained on the dfm using the recoded star labels (star_sent)
tmod_nb = textmodel_nb(dfm_train, dfm_train$star_sent)
summary(tmod_nb)

# Test the model on the held-out data. dfm_match aligns test features to train features
dfm_matched     = dfm_match(dfm_test, features = featnames(dfm_train))
predicted_class = predict(tmod_nb, newdata = dfm_matched)

# Comparison between predicted sentiment vs true sentiment
actual_class = dfm_matched$star_sent
tab_class    = table(predicted_class, actual_class)
tab_class

# Performance metrics for the Naive Bayes classifier
cm_nb = confusionMatrix(tab_class, mode = 'everything')
cm_nb


# PART 8b: COMPARE NAIVE BAYES WITH BING AND UDPIPE
# To fairly compare the supervised NB classifier with the dictionary-based
# approaches (Bing and UDPipe), we evaluate ALL methods on the SAME documents,
# i.e. the test set used for Naive Bayes.

# Save the NB predictions in a data frame
comparison_test = data.frame(
  id        = docnames(dfm_matched),
  true_sent = dfm_matched$star_sent,
  pred_NB   = predicted_class
)
rownames(comparison_test) = NULL

# Keep only the numeric part of the document ids
comparison_test = comparison_test %>%
  mutate(id = str_extract(id, '\\d{1,}$'))

comparison_test

# Extract the same test documents from the original dataset (same row order)
data_test = data[-id_train, ]

# Recreate the Bing and UDPipe binary labels on this test subset (as factors)
data_test = data_test %>%
  mutate(
    udbing_lab        = factor(ifelse(udbing > 0,         'positive', 'negative')),
    udpipe_lab        = factor(ifelse(udpipe > 0,         'positive', 'negative')),
    bing_nostop_lab   = factor(ifelse(bing_no_stop > 0,   'positive', 'negative')),
    bing_withstop_lab = factor(ifelse(bing_with_stop > 0, 'positive', 'negative'))
  )

# Bind the dictionary-based labels to the comparison data frame
comparison_test = comparison_test %>%
  bind_cols(
    data_test %>%
      select(udbing_lab, udpipe_lab, bing_nostop_lab, bing_withstop_lab)
  )

# The true sentiment must be a factor for confusionMatrix
comparison_test$true_sent = as.factor(comparison_test$true_sent)

# CONFUSION MATRICES ON THE COMMON TEST SET
cm_test_withbing = confusionMatrix(comparison_test$bing_withstop_lab, comparison_test$true_sent, mode = 'everything')
cm_test_withbing

cm_test_nobing = confusionMatrix(comparison_test$bing_nostop_lab, comparison_test$true_sent, mode = 'everything')
cm_test_nobing

cm_test_udbing = confusionMatrix(comparison_test$udbing_lab, comparison_test$true_sent, mode = 'everything')
cm_test_udbing

cm_test_udpipe = confusionMatrix(comparison_test$udpipe_lab, comparison_test$true_sent, mode = 'everything')
cm_test_udpipe

cm_nb

# SUMMARY: ACCURACY OF ALL METHODS ON THE SAME TEST SET
# Collect the accuracy of each method to make the comparison explicit
accuracy_comparison = tibble(
  method   = c('Naive Bayes', 'Bing (with stopwords)', 'Bing (no stopwords)', 'UDbing', 'UDpipe'),
  accuracy = c(
    cm_nb$overall['Accuracy'],
    cm_test_withbing$overall['Accuracy'],
    cm_test_nobing$overall['Accuracy'],
    cm_test_udbing$overall['Accuracy'],
    cm_test_udpipe$overall['Accuracy']
  )
) %>%
  arrange(desc(accuracy))

accuracy_comparison

# Visual comparison of the accuracies
accuracy_comparison %>%
  mutate(method = reorder(method, accuracy)) %>%
  ggplot(aes(method, accuracy, fill = method)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = round(accuracy, 2)), hjust = -0.1, size = 4) +
  coord_flip() +
  ylim(0, 1) +
  labs(
    title = 'Sentiment analysis: accuracy comparison on the common test set',
    x     = NULL,
    y     = 'Accuracy'
  ) +
  theme_bw() +
  theme(plot.title = element_text(color = "steelblue", size = 13, face = "bold"))

