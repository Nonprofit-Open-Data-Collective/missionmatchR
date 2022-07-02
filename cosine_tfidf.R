library(quanteda)
library(readtext)
library(dplyr)
library(quanteda.textstats)

#loading the test datasets
black <- read.csv("blackCommunities.csv")
disadvantaged <- read.csv("disadvantagedCommunities.csv")

#filtering out mission statements containing the words -black, african-american, people of color, black communities
black <- black %>% filter(BLACK == 1)

#keeping only the required columns in both datasets
black <- select(black,mission,orgname)
disadvantaged <- select(disadvantaged,Mission,Name)

#Renaming columns
black <- rename(black, Mission = mission, OrgName=orgname)
disadvantaged <- rename(disadvantaged, OrgName=Name)

#converting the datasets to quanteda corpuses
black_corpus <- corpus(black, text_field = "Mission")
disadvantaged_corpus <- corpus(disadvantaged, text_field = "Mission")
print(black_corpus)
summary(black_corpus)

#creating meaningful docid
docnames(black_corpus) <- black_corpus$OrgName
print(black_corpus)

docnames(disadvantaged_corpus) <- disadvantaged_corpus$OrgName

#########BLACK CORPUS#########
#pre-processing the data
inaugural_tokens <- quanteda::tokens(black_corpus,
                                     what = "word",
                                     remove_punct = TRUE, # default FALSE
                                     remove_symbols = TRUE, # default FALSE
                                     remove_numbers = FALSE,
                                     remove_url = TRUE, # default FALSE
                                     remove_separators = TRUE,
                                     split_hyphens = FALSE,
                                     include_docvars = TRUE,
                                     padding = FALSE,
                                     verbose = quanteda_options("verbose")
)

#generating a document-term matrix 
dtm_black <- quanteda::dfm(inaugural_tokens,
                     tolower = TRUE    # casefold
)

#look at frequencies
dtm_black %>%
  textstat_frequency() %>% 
  head(10)
#a lot of words are just stop words and others are related to belonging to black communities

#let's remove the stop words
inaugural_tokens_nostop <- inaugural_tokens %>%
  tokens_tolower() %>%
  tokens_remove(stopwords('en'))
dtm_nostop_black <- dfm(inaugural_tokens_nostop)
dtm_nostop_black %>%
  textstat_frequency() %>% 
  head(10)
#now the doc-term matrix makes more sense

#lets also male a document incidence matrix (more useful for shorter texts 
#like tweets and in this case mission statements)
dim_black <- dfm_weight(dtm_black, scheme="boolean")

dim_black %>%
  textstat_frequency() %>%
  head(10)

#TF-IDF
dtm_w_black <- dfm_tfidf(dtm_black)

#cosine similarity
cos_dtm_black <- textstat_simil(dtm_black, method="cosine")
dim(cos_dtm_black)

#lets look at the similarity matrix
sort(cos_dtm_black[,"5 CENTS FOUNDATION "],dec=T)%>%
  head(10)
#the scores don't make sense as it has a high similarity score with mission statements about black 
#communities which shouldn't be the case

#let's try without stop words
cos_nonstop_dtm_black <- textstat_simil(dtm_nostop_black, method="cosine")
dim(cos_nonstop_dtm_black)

sort(cos_nonstop_dtm_black[,"5 CENTS FOUNDATION "],dec=T)%>%
  head(10)
#did not make much of a difference

#let's try TF-IDF
cos_dtm_w_black <- textstat_simil(dtm_w_black, method="cosine")
sort(cos_dtm_w_black[,"5 CENTS FOUNDATION "],dec=T)%>%
  head(10)
#all the results from all the methods do not make a clear distinction between black community related NGOs and otherwise



