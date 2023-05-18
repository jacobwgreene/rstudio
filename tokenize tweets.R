#will need to install the packages below if you have not already done so. 

library(rtweet)
library(dbplyr)
library(dendroTools)
library(tidytext)
library(tidyverse)

#run the saved authorization command. This is a custom command. You will need to create your own auth variable using the rtweet_app() function. See the commented portions below. 
auth_as("time2-tweet")

#more extensive authorization command if the saved version is not working. use the rtweet_app authentication method, which is most appropriate for data collection. Put the bearer token from your twitter developer account here. A window should pop up to enter the token.
#auth <- rtweet_app()

#use auth_save to specify a unique name (e.g. "time2tweet") that can be called across later sessions. just use the auth_as("time2tweet")
#auth_save(auth, "time2-tweet")

#search for and download tweets containing certain words. Spaces function as boolean AND, use OR to search for different terms. Default count is 100, set n to Inf to get as many as possible
chatgpt_writing <- search_tweets("chatgpt writing", lang = "en", n = Inf, include_rts = FALSE)

#tokenize the data and remove all numbers
chatpt_writing_clean <- chatgpt_writing %>%
  select(full_text) %>%
  unnest_tokens(word, full_text) %>%
  filter(!grepl('[0-9]', word)) 

#remove default stop words
chatpt_writing_clean <- chatpt_writing_clean %>%
  anti_join(stop_words)

#custom stop words. good to add the words you used to download the data.
my_twitter_stopwords <- tibble(word = c("https",
                                        "t.co",
                                        "rt", 
                                        "chatgpt",
                                        "writing",
                                        "&amp",
                                        "ai",
                                        "write"))

#remove the custom stopwords
chatpt_writing_clean <- chatpt_writing_clean %>%
  anti_join(my_twitter_stopwords)

#visualize the top words in the data set
chatpt_writing_clean %>% 
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Most frequent words found in tweets containing 'chatgpt' and 'writing'",
       subtitle = "Stop words removed from the list")
