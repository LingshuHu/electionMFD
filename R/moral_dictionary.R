
#https://stackoverflow.com/questions/59569227/count-number-of-words-in-a-dictionary-file-in-r
install.packages("quanteda")
install.packages("devtools")
library(quanteda)
devtools::install_github("kbenoit/quanteda.dictionaries")

data(data_dictionary_MFD, package = "quanteda.dictionaries")
lengths(data_dictionary_MFD)
# first 5 values in each dictionary key
lapply(data_dictionary_MFD, head, 5)
# number of words in a text matching the MFD dictionary
dfm(data_corpus_inaugural) %>%
  dfm_lookup(dictionary = data_dictionary_MFD) %>%
  tail()

# remake the dictionary into nested catetgory of foundation and valence
data_dictionary_MFDnested <-
  dictionary(list(
    care = list(
      virtue = data_dictionary_MFD[["care.virtue"]],
      vice = data_dictionary_MFD[["care.vice"]]
    ),
    fairness = list(
      virtue = data_dictionary_MFD[["fairness.virtue"]],
      vice = data_dictionary_MFD[["fairness.vice"]]
    ),
    loyalty = list(
      virtue = data_dictionary_MFD[["loyalty.virtue"]],
      vice = data_dictionary_MFD[["loyalty.vice"]]
    ),
    authority = list(
      virtue = data_dictionary_MFD[["authority.virtue"]],
      vice = data_dictionary_MFD[["authority.vice"]]
    ),
    sanctity = list(
      virtue = data_dictionary_MFD[["sanctity.virtue"]],
      vice = data_dictionary_MFD[["sanctity.vice"]]
    )
  ))

lengths(data_dictionary_MFDnested)

lapply(data_dictionary_MFDnested, lengths)

## match texts
dfm(data_corpus_inaugural) %>%
  dfm_lookup(dictionary = data_dictionary_MFDnested, levels = 1:2) %>%
  tail()


mdf <- dfm(dfcc$text) %>%
  dfm_lookup(dictionary = data_dictionary_MFDnested, levels = 1:2)

x <-dfm(dfcc$text)

x[1,10]


df <- read.csv("metoo/MeTooMA.csv")
library(rtweet)

rt <- lookup_statuses(statuses = df$TweetId)

saveRDS(rt, "metoo/MeTooMA_tweets.rds")

mdf <- dfm(rt$text) %>%
  dfm_lookup(dictionary = data_dictionary_MFD) 

mdf <- as.data.frame(mdf)

st <- psych::describe(mdf[, 2:11])
st$vars <- rownames(st)
library(ggplot2)
ggplot(st, aes(x = vars, y = mean)) + geom_bar(stat = "identity")

