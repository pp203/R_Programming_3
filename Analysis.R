library(dplyr)
library(stringr)
#-------------------------------------------------------------------------------
# Part 1
#1
beyonce <- read.csv("beyonce.csv", header = T, stringsAsFactors = F)
stoplist <- scan("stoplist_HW.txt", what = character(), sep = "\n")


#2
beyonce_freq <- beyonce %>% count(Word, name = "Freq") %>% arrange(desc(Freq))


#3
types <- length(beyonce_freq$Word)
token <- length(beyonce$Word)
types/token
# [1] 0.1245963


#4
stoplist_clean <- beyonce_freq %>% filter(!(Word %in% stoplist))
head(stoplist_clean, 5)
#1   oh  198
#2 baby   92
#3 like   87
#4  got   76
#5  can   70


#5
bigrams <- beyonce %>% mutate(Bigram = str_c(lag(Word), Word, sep = " "))
beyonce_bigram <- bigrams %>% 
  count(Bigram, name = "Freq") %>% 
  filter(!is.na(Bigram)) %>% 
  arrange(desc(Freq))

head(beyonce_bigram, 5)
#1     oh oh  119
#2    got me   51
#3    in the   45
#4 halo halo   38
#5     on it   37


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


#-------------------------------------------------------------------------------
# Part 2
#1
beyonce_solo <- beyonce %>% filter(is.na(Featuring))
beyonce_collab <- beyonce %>% filter(!is.na(Featuring))


#2
solo_freq_oh <- beyonce_solo %>% 
  filter(Word == "oh") %>% count(Word)
# oh for solo 126

solo_freq_baby <- beyonce_solo %>% 
  filter(Word == "baby") %>% count(Word)
# baby for solo 36

collab_freq_oh <- beyonce_collab %>% 
  filter(Word == "oh") %>% count(Word)
# oh for collabration 72

collab_freq_baby <- beyonce_collab %>% 
  filter(Word == "baby") %>% count(Word)
# baby for collaboration 56


#3
bmat <- matrix(c(solo_freq_oh$n, collab_freq_oh$n,
                 solo_freq_baby$n, collab_freq_baby$n), 2, 2,
               dimnames = list(c("Solo", "Collab"), c("oh", "baby")))


#4
chisq.test(bmat, correct = FALSE)
# X-squared = 15.299, df = 1, p-value = 9.176e-05
# # 'oh' and 'baby' are different when she sings alone and with another person.


#5
# # numerator
num <- chisq.test(bmat, correct = FALSE)$statistic

# # denominator
denom <- sum(bmat)*(min(dim(bmat))-1)

# # phi
phi <- sqrt(num/denom)
# 0.2296863

# # plot
assocplot(bmat)
# when she sings alone, she more likely use "oh"
# # while when she sings with another person, she more likely use "baby".


#6
solo_me <- beyonce_solo %>% 
  filter(Word == "me") %>% count(Word)
# me for solo 103

collab_me <- beyonce_collab %>% 
  filter(Word == "me") %>% count(Word)
# me for collabration 106

solo_total <- length(beyonce_solo$Word)
# [1] 5801
collab_total <- length(beyonce_collab$Word)
# [1] 3798

solo_expected <- solo_total*(solo_me$n+collab_me$n) / (solo_total+collab_total)
# [1] 126.3058
collab_expected <- collab_total*(solo_me$n+collab_me$n) / (solo_total+collab_total)
# [1] 82.69424

2*((solo_me$n*log(solo_me$n/solo_expected)) + (collab_me$n*log(collab_me$n/collab_expected)))
# [1] 10.61811
# # (p < 0.001; critical value = 10.83)
# The word "me" in her solo is strong enough higher than her collabration.


#7
a1 <- beyonce_freq[beyonce_freq$Word == "got", "Freq"]
# [1] "got" = 76
a2 <- beyonce_freq[beyonce_freq$Word == "me", "Freq"]
# [1] "me" = 209
a3 <- beyonce_bigram[beyonce_bigram$Bigram == "got me","Freq"]
# [1] "got me" = 51
t1 <- length(beyonce$Word)
# [1] 9599

pmi <- log2((a3/t1)/((a1/t1)*(a2/t1)))
# [1] 4.945807
# # "got" and "me" has highly relationship in Beyonce's lyrics


#8
ranking <- beyonce %>%
  group_by(Rank, Song) %>%
  count(Word, name = "Freq") %>%
  filter(Word == "me") %>%
  arrange(Rank)

plot(Freq~Rank, data = ranking)
# this is non-parametric --> use spearman

cor.test(ranking$Freq, ranking$Rank, method = "spearman")
# -0.4914257
# # her rank and the word "me" are the strong negative correlation.