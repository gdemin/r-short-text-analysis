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

## Load the tidytext and tm libraries for manipulation and analysis
library(tidytext)
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




#############################
## END AS OF 9-5-18        ##  
## MUST FIX THE CODE BELOW ##
#############################


## Spellcheck the tokenized text using hunspell
# hunspell_check creates a logical vector indicating whether a word is misspelled
correct_before <- hunspell_check(sd_t$word)
summary(correct_before)
# Replace misspelled words with hunspell's suggestions
# Duplicate the tokenized dataframe so we can compare the results
sd_t_sc <- sd_t
# Use gsub to replace the misspelled words
sd_t_sc$word <- gsub(pattern = correct_before, 
                hunspell_suggest(sd_t$word[!correct_before]), 
                sd_t_sc$word)
# Note: this approach defaults to the first word suggested by hunspell, which may introduce error
correct_after <- hunspell_check(sd_t_sc$word)
summary(correct)








