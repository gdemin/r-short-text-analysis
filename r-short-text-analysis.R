################################
### SHORT TEXT ANALYSIS IN R ###
### A PRACTICAL INTRODUCTION ###
################################

############################################
## Import survey data with text responses ##
############################################

## Read in the 2015 CCES data with Haven
library(haven)
cc15 <- read_dta("~/Dropbox/GitHub/r-short-text-analysis/CCES15_Common_OUTPUT_Jan2016.dta")

## Create a list of the variables we want to keep
# V101 is the respondent identifier, which we can use to merge back into the full dataset
# CC15_300 is the open-ended response
sd_identifiers <- c("V101","CC15_300")
sd <- as.data.frame(cc15[sd_identifiers])

# We can now remove the full dataset to focus on the text analysis
rm(cc15)

#########################
## Clean the text data ##
#########################

## Load the tidytext, dplyr, and tm libraries for manipulation and analysis
library(tidytext)
library(dplyr)
library(tm)
## Load the hunspell library to spellcheck responses
library(hunspell)

## Remove blank rows
sd <- subset(sd, sd$CC15_300!="")

## Rename the columns to "line" and "text" for compatibility with tm and tidytext
# "line" indicates a discrete observation, in this case a respondent
colnames(sd)[1] <- "line"
colnames(sd)[2] <- "text"

## Tokenize the text stored in the dataframe with tidytext
# This separates each word in a response into its own row while retaining the line identifier
# This facilitates cleaning and manipulation, and also removes all capital letters
sd_t <- unnest_tokens(sd, word, text)

## Remove stopwords
sd_t_sw <- sd_t %>%
  filter(!word %in% stop_words$word)

## Stem the words to their base forms
# This reduces variations to their common stems
# First, define any words that you do not want to stem
# These are words that seemingly have a common stem but are substantively different
retain1 <- c("libertarian")
retain2 <- c("progressive")
retain3 <- c("liberal")
retain4 <- c("liberty")
replace1 <- paste(seq_len(length(retain1)), "SPECIAL_WORD1", sep = "_")
replace2 <- paste(seq_len(length(retain2)), "SPECIAL_WORD2", sep = "_")
replace3 <- paste(seq_len(length(retain3)), "SPECIAL_WORD3", sep = "_")
replace4 <- paste(seq_len(length(retain4)), "SPECIAL_WORD4", sep = "_")

## Replace all instances of the word to be retained
library(mgsub)
sd_t_sw$word[seq_len(length(sd_t_sw$word))] <- lapply(sd_t_sw$word, mgsub, 
                                                      pattern=retain1, replacement=replace1)
sd_t_sw$word[seq_len(length(sd_t_sw$word))] <- lapply(sd_t_sw$word, mgsub, 
                                                      pattern=retain2, replacement=replace2)
sd_t_sw$word[seq_len(length(sd_t_sw$word))] <- lapply(sd_t_sw$word, mgsub, 
                                                      pattern=retain3, replacement=replace3)
sd_t_sw$word[seq_len(length(sd_t_sw$word))] <- lapply(sd_t_sw$word, mgsub, 
                                                      pattern=retain4, replacement=replace4)

## Stem the text
library(SnowballC)
sd_t_sw <- sd_t_sw %>%
  mutate(word = wordStem(word))

## Replace the placeholders
sd_t_sw[seq_len(length(sd_t_sw))] <- lapply(sd_t_sw, mgsub, 
                                            pattern=replace1, replacement=retain1)
sd_t_sw[seq_len(length(sd_t_sw))] <- lapply(sd_t_sw, mgsub, 
                                            pattern=replace2, replacement=retain2)
sd_t_sw[seq_len(length(sd_t_sw))] <- lapply(sd_t_sw, mgsub, 
                                            pattern=replace3, replacement=retain3)
sd_t_sw[seq_len(length(sd_t_sw))] <- lapply(sd_t_sw, mgsub, 
                                            pattern=replace4, replacement=retain4)

## Unify after stopwords removed
# This de-tokenizes the text such that each respondent only takes up 1 line
sd_clean <- aggregate(sd_t_sw$word, list(sd_t_sw$line), paste, collapse = " ")
colnames(sd_clean)[1] <- "line"
colnames(sd_clean)[2] <- "text"

###################################
## Explore the cleaned text data ##
###################################

## Get a count of words per response
# First, we will define a wordcount function
wordcount <- function(str) {
  sapply(gregexpr("\\b\\W+\\b", str, perl=TRUE), function(x) sum(x>0) ) + 1 
}

# Count the words per respondent
sd_wordcount <- data.frame(wordcount(sd_clean$text))
summary(sd_wordcount)

# Create a histogram of words per response
library(ggplot2)
ggplot(data = sd_wordcount, aes(sd_wordcount)) +
  geom_histogram(binwidth = 1) +
  xlim(0, 55) +
  ylim(0, 8000) +
  xlab("Words") +
  ylab("Frequency")


## Get a count of characters per response
# We'll use the nchar function, similar to our use of our user-defined wordcount above
sd_charcount <- data.frame(nchar(sd_clean$text, 
                       type = "chars", allowNA = TRUE, keepNA = NA))
summary(sd_charcount)

# Create a histogram of characters per response
ggplot(data = sd_charcount, aes(x=sd_charcount)) +
  geom_histogram(binwidth = 5) +
  xlim(0, 250) +
  ylim(0, 5000) +
  xlab("Characters") +
  ylab("Frequency")

## Examine single word frequencies
# Re-tokenize the cleaned text
sd_t_clean <- unnest_tokens(sd_clean, word, text)

# Pass the cleaned text through the tidytext count function
sd_counts <- sd_t_clean %>%
  count(word, sort = TRUE)
print(sd_counts)

## Create a bar plot of the most common single words
# First, subset the data because there are too many unique words
wf <- subset(sd_counts, n>250)
# Next, define factor levels for the words to order the plot
wf$word <- factor(wf$word, levels = wf$word[order(-wf$n)])
# Now plot with ggplot
ggplot(wf, aes(word, n)) +
  geom_bar(stat="identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1))

## Create a wordcloud of single word frequencies
library(wordcloud)
# The following steps are required to format the text for the wordcloud package
sd_corpus <- Corpus(VectorSource(sd_clean))
inspect(sd_corpus)
sd_dtm <- TermDocumentMatrix(sd_corpus)
sd_m <- as.matrix(sd_dtm)
sd_v <- sort(rowSums(sd_m),decreasing=TRUE)
sd_d <- data.frame(word = names(sd_v),freq=sd_v)
head(sd_d, 10)
set.seed(1234)
wordcloud(words = sd_d$word, freq = sd_d$freq, min.freq = 2,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))  

## Examine bigram (co-occurring words) frequencies
sd_bg <- sd_clean %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

library(tidyr)
bg_sep <- sd_bg %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bg_counts <- bg_sep %>%
  count(word1, word2, sort = TRUE) %>%
  na.omit()

print(bg_counts)

bg_counts_united <- bg_counts %>%
  unite(bigram, word1, word2, sep = " ")

## Create a bar plot of the most common bigrams
bf <- subset(bg_counts_united, n>40)
# Next, define factor levels for the words to order the plot
bf$bigram <- factor(bf$bigram, levels = bf$bigram[order(-bf$n)])
# Now plot with ggplot
ggplot(bf, aes(bigram, n)) +
  geom_bar(stat="identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1))
