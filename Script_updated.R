library(tidyverse)
library(rvest)
library(RSelenium)
library(netstat)
library(cld2)
library(tidytext)
library(wordcloud)

# PART 1: WEB SCRAPING - AMAZON REVIEWS
# "Atomic Habits" by James Clear - https://www.amazon.co.uk/product-reviews/1847941834/
# RSelenium loads the reviews dynamically, then we save the HTML.

rD = rsDriver(browser = "firefox",
               verbose = FALSE,
               port = free_port(),
               chromever = NULL,
               phantomver = NULL)
remDr = rD[["client"]]

url = "https://www.amazon.co.uk/product-reviews/1847941834/"
remDr$navigate(url)

# Login
field = remDr$findElement(using = "css", "#ap_email")
email = "INSERT THE EMAil HERE"
field$sendKeysToElement(list(email))

click = remDr$findElement(using = "css", "#continue")
click$clickElement()

field = remDr$findElement(using = "css", "#ap_password")
pwd = "INSER THE PASSWORD HERE"
field$sendKeysToElement(list(pwd))

click = remDr$findElement(using = "css", "#signInSubmit")
click$clickElement()

folder = "C:/Users/Marco Zubani/Documents/MAGISTRALE/SECONDO ANNO/SECONDO SEMESTRE/TEXT MINING/ASSIGNMENT/Amazon"
dir.create(folder)

pages = 11

# Click "show more" until ~100 reviews are loaded
for (i in 2:pages) {
  button = remDr$findElement(using = "css",
                              value = "[data-hook='show-more-button']")
  button$clickElement()
  Sys.sleep(3)
}

output = remDr$getPageSource(header = TRUE)
write(output[[1]], file = str_c(folder, "Amazon_reviews_", pages, ".html"))

remDr$close()
rD$server$stop()


# PART 2: HTML SCRAPING - EXTRACT TITLE, TEXT, STARS

html = read_html(str_c(folder, "Amazon_reviews_", pages, ".html"),
                  encoding = "utf-8")

# Titles: UK and non-UK reviews use different classes, so we grab both
title = html %>%
  html_elements("[class='a-size-base a-link-normal review-title a-color-base review-title-content a-text-bold']") %>%
  html_elements("span:nth-child(3)") %>%
  html_text2()

title = title %>% c(
  html %>%
    html_elements("[class = 'a-size-base review-title a-color-base review-title-content a-text-bold']") %>%
    html_text(trim = TRUE)
)

text = html %>%
  html_elements("[class='a-size-base review-text review-text-content']") %>%
  html_text(trim = TRUE)

# Stars: same UK / non-UK split as the titles
star = html %>%
  html_elements("[data-hook='review-star-rating']") %>%
  html_text2()

star = star %>% c(
  html %>%
    html_elements("[data-hook='cmps-review-star-rating']") %>%
    html_text2()
)

data = tibble(title, text, star)
data = data %>%
  mutate(id = seq_along(text))

View(data)
dim(data)


# PART 3: DATA CLEANING

# Detect language and keep only English reviews
data$title_language = detect_language(data$title)
data$text_language  = detect_language(data$text)

table(text = data$text_language, title = data$title_language, useNA = "always")

data = data %>%
  filter(text_language == "en")

# Star column looks like "5.0 out of 5 stars"; keep the first digit
data = data %>%
  mutate(score = as.numeric(str_sub(star, 1, 1)))

dim(data)
View(data)

#saveRDS(data, file = "C:/Users/Marco Zubani/Documents/MAGISTRALE/SECONDO ANNO/SECONDO SEMESTRE/TEXT MINING/ASSIGNMENT/data.rds")
data = readRDS("C:/Users/Marco Zubani/Documents/MAGISTRALE/SECONDO ANNO/SECONDO SEMESTRE/TEXT MINING/ASSIGNMENT/data.rds")

# Binary label: positive = 4-5 stars, negative = 1-3 stars
data = data %>%
  mutate(star_sent = ifelse(score >= 4, "positive", "negative"))

table(data$star_sent)

data %>%
  summarise(
    mean   = mean(score, na.rm = TRUE),
    median = median(score, na.rm = TRUE),
    sd     = sd(score, na.rm = TRUE),
    min    = min(score, na.rm = TRUE),
    max    = max(score, na.rm = TRUE)
  )

data %>%
  count(score) %>%
  mutate(p = round(n / sum(n) * 100, 1))

clean_text = function(text) {
  text = str_replace_all(text, '([!?]){2,}', '\\1')          # repeated ! or ?
  text = str_replace_all(text, '([\\.,!?=])(\\S)', '\\1 \\2') # missing space after punctuation
  text = str_replace_all(text, '(\\.\\s?){2,}', '... ')      # ellipses
  text = str_replace_all(text, '([a-z])([A-Z])', '\\1 \\2')  # merged words
  trimws(text)
}

data = data %>%
  mutate(text = clean_text(text))

# Tokens with and without stopwords (the latter for comparison)
tidy_text = data %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  filter(!str_detect(word, '[0-9]'))

tidy_text_long = data %>%
  unnest_tokens(word, text)


# PART 4: VISUALIZATIONS

# Distribution of star ratings
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

# Binary sentiment distribution
data %>%
  count(star_sent, sort = TRUE) %>%
  ggplot(aes(star_sent, n, fill = star_sent)) +
  geom_col(show.legend = FALSE) +
  ggtitle('True sentiment distribution with \n           two categories') +
  labs(x = "Sentiment", y = "Number of Reviews") +
  theme_minimal() +
  theme(plot.title = element_text(color = "steelblue", size = 13, face = "bold"))

# Top 20 most frequent words
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

bing = get_sentiments(lexicon = "bing")
bing %>% count(sentiment)

# Document sentiment, stopwords removed
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

# Document sentiment, stopwords retained
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

# Removing stopwords drops reviews with no remaining lexicon match
dim(data)
dim(data_bing)
dim(data_withbing)

# Combine both pipelines and join back the original features
bing_all_wide = bind_rows(data_bing, data_withbing) %>%
  pivot_wider(
    names_from  = method,
    values_from = sentiment,
    values_fill = 0
  ) %>%
  inner_join(data, join_by('doc_id' == 'id'))

dim(bing_all_wide)

# Binary labels; a score of 0 is treated as negative (no positive signal)
bing_all_wide = bing_all_wide %>%
  mutate(
    bing_nostop_lab  = as.factor(ifelse(bing_no_stop  > 0, 'positive', 'negative')),
    bing_withstop_lab = as.factor(ifelse(bing_with_stop > 0, 'positive', 'negative')),
    star_sent         = as.factor(star_sent)
  )

# Sentiment words with stopwords
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

# Sentiment words without stopwords
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

# Predicted vs true label distribution
bing_all_wide %>%
  select(bing_nostop_lab, bing_withstop_lab, star_sent) %>%
  pivot_longer(everything(), names_to = 'method', values_to = 'sentiment') %>%
  count(method, sentiment) %>%
  ggplot(aes(sentiment, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ method) +
  scale_fill_manual(values = c('positive' = 'green4', 'negative' = 'firebrick3')) +
  theme_minimal()

library(caret)

cm_bing_true  = confusionMatrix(bing_all_wide$bing_nostop_lab,  bing_all_wide$star_sent)
cm_bing2_true = confusionMatrix(bing_all_wide$bing_withstop_lab, bing_all_wide$star_sent)

cm_bing_true
cm_bing2_true

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

output = data %>%
  rename(doc_id = id) %>%
  udpipe('english-gum')

# Bing dictionary in UDPipe format (1 positive, -1 negative)
bing_dict = get_sentiments('bing') %>%
  mutate(sentiment = ifelse(sentiment == 'negative', -1, 1)) %>%
  rename(term = word, polarity = sentiment)

# UDbing: lexicon only, no negators/amplifiers
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

# Most frequent adverbs, to pick amplifiers/deamplifiers
as.data.frame(udbing$data) %>%
  filter(upos == 'ADV') %>%
  count(lemma, sort = TRUE)%>%
  head(50)

# UDpipe: with negators, amplifiers and deamplifiers
udpipe_sent = txt_sentiment(
  x              = output,
  term           = 'lemma',
  polarity_terms = bing_dict,
  polarity_negators     = c('no', 'not'),
  polarity_amplifiers   = c('very', 'so', 'really', 'highly', 'extremely', 'completely', 'absolutely', 'much'),
  polarity_deamplifiers = c('quite', 'rather', 'almost', 'merely'),
  amplifier_weight = 0.8,
  n_before  = 3,
  n_after   = 3,
  constrain = FALSE
)

data$udbing  = udbing$overall$sentiment_polarity
data$udpipe  = udpipe_sent$overall$sentiment_polarity

glimpse(data)

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

# Full-dataset table for the report: Accuracy + Sensitivity (positive class = negative)
full_dataset_results = tibble(
  method = c('Bing, no stopwords', 'Bing, stopwords', 'UDbing', 'UDpipe'),
  accuracy = c(
    cm_bing_true$overall['Accuracy'],
    cm_bing2_true$overall['Accuracy'],
    cm_udbing_true$overall['Accuracy'],
    cm_udpipe_true$overall['Accuracy']
  ),
  sensitivity = c(
    cm_bing_true$byClass['Sensitivity'],
    cm_bing2_true$byClass['Sensitivity'],
    cm_udbing_true$byClass['Sensitivity'],
    cm_udpipe_true$byClass['Sensitivity']
  )
)
full_dataset_results

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

# Add the tidy Bing scores back to the main dataset
data = data %>%
  left_join(
    bing_all_wide %>% select(doc_id, bing_no_stop, bing_with_stop),
    join_by('id' == 'doc_id')
  ) %>%
  mutate(
    bing_no_stop   = ifelse(is.na(bing_no_stop),   0, bing_no_stop),
    bing_with_stop = ifelse(is.na(bing_with_stop), 0, bing_with_stop)
  )

# Normalised distributions across the four methods
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
# Supervised classifier trained on star_sent, under the bag-of-words and
# conditional-independence assumptions.
require(quanteda)
require(quanteda.textmodels)
library(caret)

data_q = data %>%
  select(id, text, star_sent)

corpus = corpus(data_q, text_field = 'text')

# Tokenise: remove punctuation, numbers, stopwords, then stem
corpus = tokens(corpus,
                remove_punct  = TRUE,
                remove_number = TRUE) %>%
  tokens_remove(pattern = stopwords('en')) %>%
  tokens_wordstem()

dfm = dfm(corpus)

# Train / test split (~70% / ~30%)
set.seed(6272)
n_docs   = nrow(data)
id_train = sample(seq_len(n_docs), round(0.7 * n_docs), replace = FALSE)

dfm_train = dfm[id_train, ]
dfm_test  = dfm[-id_train, ]

tmod_nb = textmodel_nb(dfm_train, dfm_train$star_sent)
summary(tmod_nb)

# dfm_match aligns test features to the training vocabulary
dfm_matched     = dfm_match(dfm_test, features = featnames(dfm_train))
predicted_class = predict(tmod_nb, newdata = dfm_matched)

actual_class = dfm_matched$star_sent
tab_class    = table(predicted_class, actual_class)

cm_nb = confusionMatrix(tab_class, mode = 'everything')
cm_nb


# PART 8b: COMPARE NAIVE BAYES WITH BING AND UDPIPE
# All methods are evaluated on the same documents: the NB test set.
comparison_test = data.frame(
  id        = docnames(dfm_matched),
  true_sent = dfm_matched$star_sent,
  pred_NB   = predicted_class
)
rownames(comparison_test) = NULL

comparison_test = comparison_test %>%
  mutate(id = str_extract(id, '\\d{1,}$'))

# Same test documents from the original dataset, with the dictionary labels
data_test = data[-id_train, ] %>%
  mutate(
    udbing_lab        = factor(ifelse(udbing > 0,         'positive', 'negative')),
    udpipe_lab        = factor(ifelse(udpipe > 0,         'positive', 'negative')),
    bing_nostop_lab   = factor(ifelse(bing_no_stop > 0,   'positive', 'negative')),
    bing_withstop_lab = factor(ifelse(bing_with_stop > 0, 'positive', 'negative'))
  )

comparison_test = comparison_test %>%
  bind_cols(
    data_test %>%
      select(udbing_lab, udpipe_lab, bing_nostop_lab, bing_withstop_lab)
  )

comparison_test$true_sent = as.factor(comparison_test$true_sent)

cm_test_withbing = confusionMatrix(comparison_test$bing_withstop_lab, comparison_test$true_sent, mode = 'everything')
cm_test_withbing

cm_test_nobing = confusionMatrix(comparison_test$bing_nostop_lab, comparison_test$true_sent, mode = 'everything')
cm_test_nobing

cm_test_udbing = confusionMatrix(comparison_test$udbing_lab, comparison_test$true_sent, mode = 'everything')
cm_test_udbing

cm_test_udpipe = confusionMatrix(comparison_test$udpipe_lab, comparison_test$true_sent, mode = 'everything')
cm_test_udpipe

cm_nb

# Accuracy of all methods on the common test set
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
