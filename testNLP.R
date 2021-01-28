
# load library ------------------------------------------------------------

library("openNLP")
library("magrittr")
library("NLP")
library("topicmodels")
library("tm")
library("SnowballC")
library("readtext")
library("quanteda")
library("quanteda.textmodels")
library("quanteda.dictionaries")
library("quanteda.corpora")
library("corpustools")
library("stm")
library("torch")
library("torchaudio")

# testing -----------------------------------------------------------------

## Some text.
s <-
  paste(
    c(
      "Pierre Vinken, 61 years old, will join the board as a ",
      "nonexecutive director Nov. 29.\n",
      "Mr. Vinken is chairman of Elsevier N.V., ",
      "the Dutch publishing group."
    ),
    collapse = ""
  )

s <- as.String(s)

## Chunking needs word token annotations with POS tags.
sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
pos_tag_annotator <- Maxent_POS_Tag_Annotator()
a3 <- annotate(
  s,
  list(
    sent_token_annotator,
    word_token_annotator,
    pos_tag_annotator
  )
)

annotate(s, Maxent_Chunk_Annotator(), a3)
annotate(s, Maxent_Chunk_Annotator(probs = TRUE), a3)

NLP::sents()

NLP::ngrams(
  x = paste0(letters, "asda", 1:26), 3
)

quanteda::char_ngrams(
  x = paste0(letters, "asda", 1:26), 3
)

myDfm <- dfm(
  c("1780 wemmel", "2015 schlemmel"),
  what = "character",
  ngram = 1:5,
  concatenator = "",
  removePunct = FALSE,
  removeNumbers = FALSE,
  removeSeparators = FALSE,
  verbose = FALSE
)

text <-
  "In 1804, after several months of profound spiritual anxiety, Jarena Lee moved from New Jersey to Philadelphia. There she labored as a domestic and worshiped among white congregations of Roman Catholics and mixed congregations of Methodists. On hearing an inspired sermon by the Reverend Richard Allen, founder of the Bethel African Methodist Episcopal Church, Lee joined the Methodists. She was baptized in 1807. Prior to her baptism, she experienced the various physical and emotional stages of conversion: terrifying visions of demons and eternal perdition; extreme feelings of ecstasy and depression; protracted periods of meditation, fasting, and prayer; ennui and fever; energy and vigor. In 1811 she married Joseph Lee, who pastored an African-American church in Snow Hill, New Jersey. They had six children, four of whom died in infancy."

bio <- tokenize_sentence(text)[[1]]

bio <- as.String(bio)

word_ann <- Maxent_Word_Token_Annotator()
sent_ann <- Maxent_Sent_Token_Annotator()
person_ann <- Maxent_Entity_Annotator(kind = "person")
location_ann <- Maxent_Entity_Annotator(kind = "location")
organization_ann <- Maxent_Entity_Annotator(kind = "organization")

bio_ann <- annotate(
  bio,
  list(
    sent_ann,
    word_ann,
    person_ann,
    location_ann,
    organization_ann
  )
)

class(bio_ann)

head(bio_ann)

bio_doc <- AnnotatedPlainTextDocument(bio, bio_ann)

bio_doc %>%
  sents()

bio_doc %>%
  words()

a <- annotation(bio_doc)
k <- lapply(
  a$features,
  `[[`,
  "kind"
)

bio_doc$content[a[a$type == "entity"]]
bio_doc$content[a[k == "person"]]
bio_doc$content[a[k == "location"]]
bio_doc$content[a[k == "organization"]]

# quanteda ----------------------------------------------------------------

text <- "An example of preprocessing techniques"
toks <- tokens(text)
toks %>% class()
toks %>%
  unlist() %>%
  .[2]

sw <- stopwords(language = "en")

toks <- tokens_remove(toks, sw)

toks <- toks %>%
  tokens_tolower() %>%
  tokens_wordstem()

text <- c(
  d1 = "An example of preprocessing techniques",
  d2 = "An additional example of example",
  d3 = "A third example"
)

dtm <- dfm(text,
  # input text
  tolower = TRUE,
  stem = TRUE,
  # set lowercasing and stemming to TRUE
  remove = stopwords("english")
)

dtm
docfreq(dtm)
dfm_weight(dtm, "tfidf")
data_corpus_inaugural
dtm <- dfm(
  x = data_corpus_inaugural,
  tolower = TRUE,
  stem = TRUE,
  remove = stopwords("en"),
  remove_punct = TRUE
)

dtm %>%
  docfreq()

myDict <- dictionary(list(
  terror = c("terror*"),
  economy = c("job*", "business*", "econom*")
))

dict_dtm <- dfm_lookup(dtm,
  myDict,
  nomatch = "_unmatched"
)

tail(dict_dtm)

set.seed(2)
# create a document variable indicating pre or post war
docvars(dtm, "is_prewar") <- docvars(dtm, "Year") < 1945
# sample 40 documents for the training set and use remaining (18) for testing
train_dtm <- dfm_sample(dtm, size = 40)
test_dtm <- dtm[setdiff(docnames(dtm), docnames(train_dtm)), ]
# fit a Naive Bayes multinomial model and use it to predict the test data
nb_model <-
  textmodel_nb(train_dtm,
    y = docvars(train_dtm, "is_prewar")
  )
pred_nb <- predict(nb_model,
  newdata = test_dtm
)
# compare prediction (rows) and actual is_prewar value (columns) in a table
table(
  prediction = pred_nb,
  is_prewar = docvars(test_dtm, "is_prewar")
)

texts <- corpus_reshape(data_corpus_inaugural, to = "paragraphs")
par_dtm <- dfm(
  texts,
  stem = TRUE,
  # create a document-term matrix
  remove_punct = TRUE,
  remove = stopwords("english")
)
par_dtm <- dfm_trim(par_dtm, min_count = 5) # remove rare terms
par_dtm <-
  convert(par_dtm, to = "topicmodels") # convert to topicmodels format
set.seed(1)
lda_model <- topicmodels::LDA(par_dtm,
  method = "Gibbs",
  k = 5
)

terms(lda_model, 5)

corpus_pres <- corpus_subset(
  data_corpus_inaugural,
  President %in% c("Obama", "Trump")
)

dtm_pres <- dfm(
  corpus_pres,
  groups = "President",
  remove = stopwords("english"),
  remove_punct = TRUE
)
# compare target (in this case Trump) to rest of DTM (in this case only Obama).
keyness <- textstat_keyness(dtm_pres,
  target = "Trump"
)
textplot_keyness(keyness)

tc <- create_tcorpus(sotu_texts, doc_column = "id")
hits <- tc$search_features('"freedom americ*"~5')
## created index for "token" column
kwic <- tc$kwic(hits, ntokens = 3)
head(kwic$kwic, 3)

# quanteda documentations -------------------------------------------------

corp_uk <-
  corpus(data_char_ukimmig2010) # build a new corpus from the texts
summary(corp_uk)

docvars(corp_uk, "Party") <- names(data_char_ukimmig2010)
docvars(corp_uk, "Year") <- 2010
docvars(corp_uk, "auther") <- "Vikram"
summary(corp_uk)

corp_uk %>%
  texts()

tokeninfo <- summary(data_corpus_inaugural)
tokeninfo$Year <- docvars(data_corpus_inaugural, "Year")
if (require(ggplot2)) {
  ggplot(
    data = tokeninfo,
    aes(
      x = Year,
      y = Tokens,
      group = 1
    )
  ) +
    geom_line() +
    geom_point() +
    scale_x_continuous(
      labels = c(seq(1789, 2017, 12)),
      breaks = seq(1789, 2017, 12)
    ) +
    theme_bw()
}

# Longest inaugural address: William Henry Harrison
tokeninfo[which.max(tokeninfo$Tokens), ]

corp1 <- corpus(data_corpus_inaugural[1:5])
corp2 <- corpus(data_corpus_inaugural[53:58])
corp3 <- corp1 + corp2
summary(corp3)

data_corpus_inaugural %>%
  corpus_subset(Year > 1990) %>%
  summary()

data_corpus_inaugural %>%
  corpus_subset(President == "Adams") %>%
  summary()

kwic(data_corpus_inaugural,
  pattern = "terror"
)

kwic(data_corpus_inaugural,
  pattern = "terror",
  valuetype = "regex"
)

kwic(data_corpus_inaugural,
  pattern = "friends"
)

txt <- c(
  text1 = "This is $10 in 999 different ways,\n up and down; left and right!",
  text2 = "@kenbenoit working: on #quanteda 2day\t4ever, http://textasdata.com?page=123."
)
tokens(txt)
tokens(txt,
  remove_punct = TRUE,
  remove_numbers = TRUE,
  remove_symbols = TRUE,
  what = "character"
)

tokens(txt,
  remove_punct = TRUE,
  remove_numbers = TRUE,
  remove_symbols = TRUE,
  what = "word1"
)

dfmat_uk <- dfm(data_char_ukimmig2010, remove = stopwords("english"), remove_punct = TRUE)
dfmat_uk
textplot_wordcloud(
  x = dfmat_uk,
  min_count = 6,
  random_order = FALSE,
  rotation = 0.25,
  color = RColorBrewer::brewer.pal(8, "Dark2")
)

# quanteda book -----------------------------------------------------------

chat_data %>%
  corpus(
    text_field = "user",
    docid_field = "chat_no"
  ) %>%
  corpus_reshape(to = "sentence") %>%
  corpus_subset(user_sentiment_score > 1)

# ndoc(user_tokens)
# prd$docs_newspace
# methods("corpus")
# getAnywhere("phrase.character")
# selectMethod(f = "corpus",
#              signature = "data.frame")
# getMethod("corpus.data.frame")
# getMethod("phrase")

phrase("asdfasd asdaa")

kw_asylum <- kwic(user_tokens, pattern = phrase("dank"))
head(kw_asylum)

# user_tokens %>%
#   textstat_collocations(size = 4) %>%
#   as.data.table() %>%
#   .[order(-count)]

kwic(
  tokens_remove(user_tokens, letters),
  pattern = "dank"
) %>%
  head()

kwic(user_tokens, ger_emo_dict) %>%
  head()

tokens_lookup(user_tokens, ger_emo_dict) %>%
  head()

user_tokens %>%
  tokens_compound(pattern = phrase("hab *")) %>%
  dfm() %>%
  topfeatures()

user_tokens %>%
  tokens_compound(pattern = phrase("hab *")) %>%
  dfm() %>%
  dfm_weight(scheme = "prop")

user_tokens %>%
  tokens_compound(pattern = phrase("hab *")) %>%
  dfm() %>%
  textstat_frequency()

user_tokens %>%
  tokens_compound(pattern = phrase("hab *")) %>%
  dfm() %>%
  dfm_tfidf() %>%
  topfeatures()

corpus.un.sample <- corpus_sample(mycorpus, size = 500)

corp_news <- download("data_corpus_guardian")

dfmat_news <- dfm(corp_news, remove = stopwords("en"), remove_punct = TRUE)
dfmat_news <- dfm_remove(dfmat_news, pattern = c("*-time", "updated-*", "gmt", "bst", "|"))
dfmat_news <- dfm_trim(dfmat_news, min_termfreq = 100)

topfeatures(dfmat_news)
nfeat(dfmat_news)
fcmat_news <- fcm(dfmat_news)
dim(fcmat_news)
topfeatures(fcmat_news)
feat <- names(topfeatures(fcmat_news, 50))
fcmat_news_select <- fcm_select(fcmat_news, pattern = feat, selection = "keep")
dim(fcmat_news_select)

size <- log(colSums(dfm_select(dfmat_news, feat, selection = "keep")))

set.seed(144)
textplot_network(fcmat_news_select, min_freq = 0.8, vertex_size = size / max(size) * 3)
