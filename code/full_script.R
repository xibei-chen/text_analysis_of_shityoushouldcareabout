#############################
###    Data Collection    ###
#############################

# Load packeges
library(rvest)
library(data.table)

# Scraper for one page
get_one_page <- function(url) {
        t <- read_html(url)
        rel_link <- t %>% html_nodes('.BlogList-item-title') %>% html_attr('href')
        title <- t %>% html_nodes('.BlogList-item-title') %>% html_text()
        date <- t %>% html_nodes('.Blog-meta-item--date') %>% html_text()
        link <- paste0('https://shityoushouldcareabout.com', rel_link)
        return(data.frame('title'= title, 'date'= date, 'link'=link))
}

# Scrape all pages
url <- 'https://shityoushouldcareabout.com/article'
urls <- paste0(url,'?page=',1:12)
list_of_dfs<- lapply(urls, get_one_page)
df <- rbindlist(list_of_dfs)

# Get the content for each article link
for (i in 1:nrow(df)){
        content <- read_html(df$link[i]) %>% html_nodes('p') %>% html_text()
        content <- paste(content, collapse = '')
        df$content[i]=content
}

# Save the df
saveRDS(df, file = "sysca_data.rds")






#############################
###   Data Preparation    ###
#############################

# Load packages
library(dplyr)
library(tidytext)
library(stringr)
library(textstem)
library(lubridate)
library(ggplot2)

###  Data Cleaning
#############################
# Read the data
df <- readRDS(file = "sysca_data.rds")

# Drop the link column
df <- df %>% select(-link)

# Convert character to date
df$date <- as_date(df$date, format = "%B %e, %Y")

# Add number of articles column by date
df <- df %>% merge(df %>% group_by(date) %>% summarize(n_articles=n()), by="date")

# Combine together titles and bodies into text
df$text <- paste(df$title, df$content, sep = " ")
df <- df %>% select(-content)


###  Exploratory Data Analysis
###############################
# Check number of articles
nrow(df)
# 240 articles

# Check time range
min(df$date)
# "2018-09-24"
max(df$date)
# "2021-08-20"

# Visualize number of articles published over time
df_n_articles <- df %>% group_by(date) %>% 
        summarise(n_articles=mean(n_articles)) 

df_n_articles %>% ggplot(aes(x=date, y=n_articles, fill=n_articles))+
        geom_col(width= 10, alpha=0.6, stat = "identity")+
        scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y") +
        scale_fill_viridis_c(direction = -1) +
        geom_segment( aes(y = mean( df_n_articles$n_articles, na.rm = T ), 
                          yend = mean( df_n_articles$n_articles, na.rm = T),
                          x = as.Date('2018-09-24'),
                          xend = as.Date('2021-08-20')) , color = '#f8776c',linetype="dashed")+
        annotate( "text" , y = mean( df_n_articles$n_articles, na.rm = T )+1, x = as.Date('2021-08-20') , 
                  label =  round(mean( df_n_articles$n_articles, na.rm = T )) , color = '#f8776c')+
        annotate( "text" , y = max( df_n_articles$n_articles)+1, x = as.Date('2019-02-06') , 
                  label =  max( df_n_articles$n_articles), color = '#f8776c')+
        labs(x=NULL,y=NULL,
             title = "Number of Articles Published over Time",
             subtitle = "- On average 1 article per publication date, 6th Feb 2019 is an exception with 20 articles published.\n- There is a surge in terms of the frequency of publication between April 2020 and December 2020.")+
        theme_minimal()+
        theme(legend.position="none",
              plot.title=element_text(size=25, face="bold", colour="#184e77", vjust=1),
              plot.subtitle=element_text(size=18, face="italic", color="#1a759f",vjust=.8))



###  Text Preprocessing
#############################
# Import stopwords corpus
data(stop_words)

# Text-preprocessing
text_preprocessor <- function(text){
        
        # remove non-alphanumeric
        text <- str_replace_all(text, "[^[:alnum:]]", "")
        # lowercase every letter
        text <- tolower(text)
        # tokenize text
        tokens <- unlist(strsplit(text, "\\s+"))
        # remove stopwords
        tokens <- tokens[!(tokens %in% stop_words$word)]
        # apply lemmatization
        tokens <- lemmatize_words(tokens)
        # remove words with less than 3 letters
        tokens <- tokens[length(tokens) >= 3]
        # join tokens back to text
        preprocessed_text <- paste(tokens, collapse = " ")
        
        return(preprocessed_text)
}

df$text_clean <- lapply(df$text, text_preprocesser)
df <- df %>% select(-text)





######################################
###   Word Frequency and TF-IDF    ###
######################################

# Word frequency top words
word_freq <- df  %>% unnest_tokens(word, text_clean) %>% 
        count(word, sort = TRUE) 

customized_stopwords <- c('people','time','feel','life','day','start','call','leave','live',
                          'talk','read','bad','happen','stop','month', 'experience','continue',
                          'have', 'don', 'didn','doesn','medium','mean','tell','person','news')

word_freq[!(word %in% customized_stopwords)] %>% 
        head(20) %>% 
        mutate(word=reorder(word,n)) %>% 
        ggplot(aes(word,n,fill=n)) + geom_col() + 
        geom_text(aes(label=n), hjust=-0.5, size =3, color="#1a759f") +
        scale_fill_viridis_c(direction = -1) +
        labs(x=NULL,y=NULL,
             title = "Top 20 Most Frequently Used Words",
             subtitle = "Gender, sexuality, mental health, love, black rights are potential hot topics")+
        coord_flip()+
        theme_minimal()+
        theme(legend.position="none",
              plot.title=element_text(size=25, face="bold", colour="#184e77", vjust=1),
              plot.subtitle=element_text(size=18, face="italic", color="#1a759f",vjust=.8))

# Top20 tf-idf words
df_tfidf <- df  %>% unnest_tokens(word, text_clean) %>% 
        count(word, title,sort = TRUE)  %>% 
        ungroup() %>% 
        mutate(total = sum(n))

total_words <- df_tfidf %>% 
        group_by(title)  %>% 
        summarize(total = sum(n))

df_tfidf <- left_join(df_tfidf, total_words)

df_tfidf <- df_tfidf  %>% bind_tf_idf(word, title, n)

df_tfidf %>% arrange(desc(tf_idf)) %>% 
        head(20) %>% 
        mutate(word=reorder(word,tf_idf)) %>% 
        ggplot(aes(word,tf_idf,fill=tf_idf)) + geom_col() + 
        geom_text(aes(label=round(tf_idf,2)), hjust=-0.5, size =3) +
        scale_fill_viridis_c(direction = -1) +
        labs(x=NULL,y=NULL,
             title = "Top 20 TF-IDF Words")+
        coord_flip()+
        theme_minimal()+
        theme(legend.position="none",
              plot.title=element_text(size=25, face="bold", colour="#184e77", vjust=1),
              plot.subtitle=element_text(size=18, face="italic", color="#1a759f",vjust=.8))






##############################
###  Sentiment Analysis    ###
##############################

# Load package
library(textdata)

# Unnest tokens in the text column
df_token <- df %>% unnest_tokens(word, text_clean)

# Create dataframe using Bing lexicon
bing <- df_token %>% 
        inner_join(get_sentiments("bing")) %>%
        mutate(method = "bing",score = ifelse(sentiment=='positive',1,-1)) %>% 
        group_by(date, n_articles) %>% 
        summarise(sentiment = sum(score)/n_articles)
bing <- bing[!duplicated(bing), ]

# Visualize sentiment over time
bing %>% ggplot(aes(x=date, y=sentiment))+
        geom_col(aes(fill=sentiment),width= 10, alpha=0.6, stat = "identity",position = position_nudge(x = 2))+
        scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y") +
        scale_fill_viridis_c(direction = -1) +
        labs(x=NULL,y=NULL,
             title = "Sentiment Over Time Using Bing Lexicon",
             subtitle = "Negative sentiment appears much more often and stronger than positive sentiment in general.")+
        theme_minimal()+
        theme(legend.position="none",
              plot.title=element_text(size=25, face="bold", colour="#184e77", vjust=1),
              plot.subtitle=element_text(size=18, face="italic", color="#1a759f",vjust=.8))

# Most common positive and negative words using bing dictionary
df_token %>% inner_join(get_sentiments("bing")) %>%
        count(word, sentiment, sort=TRUE) %>% 
        group_by(sentiment) %>% 
        top_n(10) %>% ungroup() %>% 
        mutate(word=reorder(word,n)) %>% 
        ggplot(aes(word,n,fill=sentiment))+
        geom_col()+
        facet_wrap(~sentiment, scales="free_y")+
        labs(x=NULL,y=NULL,
             title = "Top 10 Words for Negative and Positive Sentiment")+
        coord_flip()+
        theme_minimal()+
        theme(legend.position="none",
              plot.title=element_text(size=25, face="bold", colour="#184e77", vjust=1))


# Create dataframe using Loughran lexicon
loughran <- df_token %>% 
        inner_join(get_sentiments("loughran")) %>%
        mutate(method = "loughran") %>% 
        group_by(sentiment) %>% 
        summarise(score=n()/240)

# Visualize proportion of different sentiments
loughran %>% ggplot(aes(x=sentiment,y=score))+
        geom_col(aes(fill=score))+
        scale_fill_viridis_c(direction = -1) +
        labs(x=NULL,y=NULL,
             title = "Sentiment Analysis Using Loughran-McDonald Lexicon",
             subtitle = "Negative sentiment is the highest, far ahead of positive and then ligitious sentiment.")+
        theme_minimal()+
        theme(legend.position="none",
              plot.title=element_text(size=25, face="bold", colour="#184e77", vjust=1),
              plot.subtitle=element_text(size=18, face="italic", color="#1a759f",vjust=.8))

# Wordcloud for negative and positive sentiments
library(wordcloud)

negative_wc <- df_token %>% inner_join(get_sentiments('loughran'), by = "word")  %>% 
        filter(sentiment=='negative') %>% 
        count(word, sort = TRUE) 

wordcloud(words = negative_wc$word, 
          freq = negative_wc$n, 
          min.freq = 1,max.words=200, 
          random.order=FALSE, 
          rot.per=0.35, 
          scale=c(1.8, .5),
          colors=brewer.pal(8, "Dark2"))

positive_wc <- df_token %>% inner_join(get_sentiments('loughran'), by = "word")  %>% 
        filter(sentiment=='positive') %>% 
        count(word, sort = TRUE) 

wordcloud(words = positive_wc$word, 
          freq = positive_wc$n, 
          min.freq = 1,max.words=200, 
          random.order=FALSE, 
          rot.per=0.35, 
          scale=c(2, .5),
          colors=brewer.pal(8, "Dark2"))





##################################
###  Bigram Network Analysis   ###
##################################

# Load package
library(igraph)
library(ggraph)

# Tokenize words specifying using bigrams
df_bigrams <- df %>% unnest_tokens(bigram, text_clean, token = "ngrams", n = 2)

# Separate bigram words
bigrams_separated <- df_bigrams  %>% separate(bigram, c("word1", "word2"), sep = " ")

# Count bigrams
bigram_counts <- bigrams_separated  %>% count(word1, word2, sort = TRUE)

# Generate bigram network graph
bigram_graph <- bigram_count  %>% 
        select(from = word1, to = word2, n = n) %>% 
        filter(n > 20)  %>% 
        graph_from_data_frame()

set.seed(2022)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
        geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                       arrow = a, end_cap = circle(.07, 'inches')) +
        geom_node_point(color = "lightblue", size = 5) +
        geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
        labs(x=NULL,y=NULL,
             title = "Network of Bigrams")+
        theme_void()+
        theme(legend.position="none",
              plot.title=element_text(size=25, hjust=0.1, face="bold", colour="#184e77", vjust=1))






################################
###  Topic Modelling (LDA)   ###
################################

# Load packages
library(tm)
library(ldatuning)
library(topicmodels)

# Create corpus and document term matrix
corpus <- Corpus(VectorSource(df$text_clean))
dtm <- DocumentTermMatrix(corpus)

# Find the optimal number of topics
result <- FindTopicsNumber(
        dtm,
        topics = seq(from = 5, to = 30, by = 5),
        metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010"),
        method = "Gibbs",
        control = list(seed = 2022),
        verbose = TRUE
)

FindTopicsNumber_plot(result)

# Generate graph for 20 topics
lda <- LDA(dtm, k = 20, control = list(seed = 2022))
topics <- tidy(lda, matrix = "beta")
top_terms <- topics  %>% 
        group_by(topic)  %>% 
        top_n(10, beta)  %>% 
        ungroup()  %>% 
        arrange(topic, -beta)
top_terms  %>% 
        mutate(term = reorder(term, beta)) |>
        ggplot(aes(term, beta, fill = factor(topic))) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~ topic, scales = "free") +
        coord_flip()+
        labs(x=NULL,y=NULL,
             title = "20 Topics Using LDA")+
        theme_minimal()+
        theme(legend.position="none",
              plot.title=element_text(size=25, face="bold", colour="#184e77", vjust=1))






