#######################################
## INCOMPLETE CODE FOR SPELLCHECKING ##  
#######################################


## Spellcheck the tokenized text using hunspell
# hunspell_check creates a logical vector indicating whether a word is misspelled
correct_before <- hunspell_check(sd_t_sw$word)
summary(correct_before)
# Replace misspelled words with hunspell's suggestions
# Duplicate the tokenized dataframe so we can compare the results
sd_t_sw_sc <- sd_t_sw

#Pull all of hunspell's initial suggested corrections


#Try to replace with stringr
library(stringr)
str_replace_all(string = sd_t_sw_sc$word,
                pattern = sd_t_sw_sc$word[!correct_before],
                replacement = hunspell_suggest(sd_t_sw_sc$word[[1]])[[1]][1])

correct_after <- hunspell_check(sd_t_sw_sc$word)
summary(correct_after)

# Use gsub to replace the misspelled words
gsub(pattern = sd_t_sw_sc$word[!sd_t_sw_sc$correct_before], 
     replacement = hunspell_suggest(sd_t_sw_sc$word[[1]]), 
     x = sd_t_sw_sc$word)