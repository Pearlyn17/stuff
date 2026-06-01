install.packages("quanteda.textplots")
library(readtext)
library(quanteda)
library(quanteda.textplots)

quanteda_options("threads" = 2)

# Use *.txt for multiple text files
SG_Budget_data <- readtext("/Users/Pearlyn/Downloads/Data/*.txt",
                 docvarsfrom = "filenames", 
                 docvarnames = c("Year", "Singapore_Budget"),
                 dvsep = "_",
                 encoding = "UTF-8")

# Remove "Page X of Y"
SG_Budget_data$text <- gsub("(?i)page \\d+ of \\d+", "", SG_Budget_data$text)

# Remove section letters such as "A. ", "B. ", "a. ", "b. " etc
SG_Budget_data$text <- gsub("(?i)\\b[a-z]\\.\\s", "", SG_Budget_data$text)

SG_Budget_corpus <- corpus(SG_Budget_data)
summary(SG_Budget_corpus)

# --- Text Preprocessing for Text Mining --- #
# Remove punctuation and numbers
tokens1 <- tokens(SG_Budget_corpus, remove_punct = T, remove_numbers = T)
sum(ntoken(tokens1))

# Apply word stemming
tokens2 <- tokens_wordstem(tokens1)
sum(ntoken(tokens2))

# Remove stopwords
tokens3 <- tokens_remove(tokens2, pattern = stopwords("en"))
sum(ntoken(tokens3))

# Apply Lexicoder Dictionary and adjust for negated words
# Step 1 - Create Document-Feature Matrix (DFM)
dfm <- dfm(tokens3)

# Step 2 - Apply the Lexicoder-Sentiment Dictionary (LSD2015)
dfm.lsd <- dfm_lookup(dfm, dictionary = data_dictionary_LSD2015)
dfm.lsd

# --- Word Cloud --- #
# Visualise the most informative words used in recent Singapore Budget Speeches from Year 2020 onwards
recent.corpus <- corpus_subset(SG_Budget_corpus)

# Remove stopwords from DFM & trim away low frequency words
recent.dfm <- tokens(recent.corpus, remove_punc = T, remove_numbers = T)
recent.dfm <- dfm(recent.dfm)
recent.dfm <- dfm_remove(recent.dfm, 
                         pattern = "\\p{Han}|\\$", 
                         valuetype = "regex")
recent.dfm <- dfm_remove(recent.dfm, pattern = stopwords("en"))
recent.dfm <- dfm_trim(recent.dfm, min_termfreq = 100, verbose = FALSE)

set.seed(2014)
textplot_wordcloud(recent.dfm, max_words = 300, min_size = 1, max_size = 5)
grid::grid.text("Figure: Frequently Used Words in Singapore Budget Statement",
                x = grid::unit(0.90, "npc"),
                y = grid::unit(0.90, "npc"), 
                gp = grid::gpar(col = "salmon", fontsize = 14))

# --- Tokenisation --- #
sentences <- c(one = "ILLIT's Almond Chocolate is very beautiful.. mesmerising chocolate sweets and soundtrack!",
          two = "My favourite kpop artistes are fromis9's Lee Nagyung, Song Hayoung, Baek Jiheon, Aespa's Winter and Karina and IVE's Leeseo - too many kpop fandom")
words <- tokens(sentences)
words
