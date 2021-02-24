## Sentiment Analysis --- IMDB movie review 



##
result <- c()
for(i in c(1, seq(10, 290, 10))) {
  link <- paste0("https://www.imdb.com/title/tt0111161/reviews?start=",i)
  #https://www.imdb.com/title/tt0111161/reviews?ref_=tt_ql_3
  HouseofCards_IMDb <- read_html(link)
  
  # Used SelectorGadget as the CSS Selector
  reviews <- HouseofCards_IMDb %>% html_nodes("#pagecontent") %>%
    html_nodes(".text") %>%
    html_text()
  
  # perfrom data cleaning on user reviews
  reviews <- gsub("\r?\n|\r", " ", reviews) 
  reviews <- tolower(gsub("[^[:alnum:] ]", " ", reviews))
  sapply(reviews, function(x){})
  result <- c(result, reviews)
}

write.table(result,"IMDB.txt")
getwd()

#### Sentiment Analysis ####
txt <- result

str(txt)
length(txt)
View(txt)

# install.packages("tm")
library(tm)

# Convert the character data to corpus type
x <- Corpus(VectorSource(txt))

inspect(x[1])

x <- tm_map(x, function(x) iconv(enc2utf8(x), sub='byte'))
#?tm_map

# Data Cleansing
x1 <- tm_map(x, tolower)
#inspect(x1[1])

x1 <- tm_map(x1, removePunctuation)
#inspect(x1[1])

#inspect(x1[5])
x1 <- tm_map(x1, removeNumbers)
#inspect(x1[1])

x1 <- tm_map(x1, removeWords, stopwords('english'))
#inspect(x1[1])

# striping white spaces 
x1 <- tm_map(x1, stripWhitespace)
#inspect(x1[1])

# Term document matrix 
# converting unstructured data to structured format using TDM

tdm <- TermDocumentMatrix(x1)
tdm
dtm <- t(tdm) # transpose
dtm <- DocumentTermMatrix(x1)

# To remove sparse entries upon a specific value
corpus.dtm.frequent <- removeSparseTerms(tdm, 0.99) 
#?removeSparseTerms

tdm <- as.matrix(tdm)
dim(tdm)

tdm[1:20, 1:20]

inspect(x[1])

# Bar plot
w <- rowSums(tdm)
w

w_sub <- subset(w, w >= 400)
w_sub

barplot(w_sub, las=2, col = rainbow(30))

# Terms laptop, computer repeat maximum number of times
x1 <- tm_map(x1, removeWords, c('can','every', 'film','shawshank', 'movie','redemption'))
x1 <- tm_map(x1, stripWhitespace)




tdm <- TermDocumentMatrix(x1)
tdm

tdm <- as.matrix(tdm)
tdm[1:20, 1:20]

# Bar plot after removal of the terms
w <- rowSums(tdm)
w

w_sub <- subset(w, w >= 50)
w_sub

barplot(w_sub, las=2, col = rainbow(2))

##### Word cloud #####
#install.packages("wordcloud")
library(wordcloud)

wordcloud(words = names(w_sub), freq = w_sub)

w_sub1 <- sort(rowSums(tdm), decreasing = TRUE)
head(w_sub1)



# better visualization
wordcloud(words = names(w_sub1), freq = w_sub1, random.order=F, colors=rainbow(30), scale = c(2,0.5), rot.per = 0.4)
windows()


############# Wordcloud2 ###############

#installed.packages("wordcloud2")
library(wordcloud2)

w1 <- data.frame(names(w_sub), w_sub)
colnames(w1) <- c('word', 'freq')




