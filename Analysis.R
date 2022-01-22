library(dplyr)
library(stringr)
#-------------------------------------------------------------------------------

#1
beyonce <- read.csv("beyonce.csv", header = T, stringsAsFactors = F)
stoplist <- scan("stoplist_HW.txt", what = character(), sep = "\n")


#2
beyonce_freq <- beyonce %>% count(Word, name = "Freq") %>% arrange(desc(Freq))


#3
types <- length(beyonce_freq$Word)
token <- length(beyonce$Word)
types/token



#4
stoplist_clean <- beyonce_freq %>% filter(!(Word %in% stoplist))
head(stoplist_clean, 5)


#5
bigrams <- beyonce %>% mutate(Bigram = str_c(lag(Word), Word, sep = " "))
beyonce_bigram <- bigrams %>% 
  count(Bigram, name = "Freq") %>% 
  filter(!is.na(Bigram)) %>% 
  arrange(desc(Freq))

head(beyonce_bigram, 5)


#6
beyonce_conc <- beyonce %>% 
  mutate(Left = str_c(lag(Word, n = 2),
                      lag(Word, n = 1),
                      sep = " "),
         Right = str_c(lead(Word, n = 1),
                       lead(Word, n = 2),
                       sep = " ")) %>%
  filter(str_detect(Word, pattern = "^un")) %>% 
  select(Left, Word, Right)


#6
beyonce_solo <- beyonce %>% filter(is.na(Featuring))
beyonce_collab <- beyonce %>% filter(!is.na(Featuring))


#7
solo_freq_oh <- beyonce_solo %>% 
  filter(Word == "oh") %>% count(Word)

solo_freq_baby <- beyonce_solo %>% 
  filter(Word == "baby") %>% count(Word)

collab_freq_oh <- beyonce_collab %>% 
  filter(Word == "oh") %>% count(Word)

collab_freq_baby <- beyonce_collab %>% 
  filter(Word == "baby") %>% count(Word)


#8
bmat <- matrix(c(solo_freq_oh$n, collab_freq_oh$n,
                 solo_freq_baby$n, collab_freq_baby$n), 2, 2,
               dimnames = list(c("Solo", "Collab"), c("oh", "baby")))


#9
chisq.test(bmat, correct = FALSE)
# X-squared = 15.299, df = 1, p-value = 9.176e-05
# # 'oh' and 'baby' are different when she sings alone and with another person.


#10
# # numerator
num <- chisq.test(bmat, correct = FALSE)$statistic

# # denominator
denom <- sum(bmat)*(min(dim(bmat))-1)

# # phi
phi <- sqrt(num/denom)

# # plot
assocplot(bmat)


#11
solo_me <- beyonce_solo %>% 
  filter(Word == "me") %>% count(Word)

collab_me <- beyonce_collab %>% 
  filter(Word == "me") %>% count(Word)

solo_total <- length(beyonce_solo$Word)

collab_total <- length(beyonce_collab$Word)

solo_expected <- solo_total*(solo_me$n+collab_me$n) / (solo_total+collab_total)

collab_expected <- collab_total*(solo_me$n+collab_me$n) / (solo_total+collab_total)

2*((solo_me$n*log(solo_me$n/solo_expected)) + (collab_me$n*log(collab_me$n/collab_expected)))


#12
a1 <- beyonce_freq[beyonce_freq$Word == "got", "Freq"]

a2 <- beyonce_freq[beyonce_freq$Word == "me", "Freq"]

a3 <- beyonce_bigram[beyonce_bigram$Bigram == "got me","Freq"]

t1 <- length(beyonce$Word)


pmi <- log2((a3/t1)/((a1/t1)*(a2/t1)))


#13
ranking <- beyonce %>%
  group_by(Rank, Song) %>%
  count(Word, name = "Freq") %>%
  filter(Word == "me") %>%
  arrange(Rank)

plot(Freq~Rank, data = ranking)


cor.test(ranking$Freq, ranking$Rank, method = "spearman")
