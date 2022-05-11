rm(list=ls())
library(rvest)
library(data.table)

# scraper for one page
get_one_page <- function(url) {
        
        t <- read_html(url)
        
        rel_link <- t %>% html_nodes('.BlogList-item-title') %>% html_attr('href')
        
        title <- t %>% html_nodes('.BlogList-item-title') %>% html_text()
        date <- t %>% html_nodes('.Blog-meta-item--date') %>% html_text()
        link <- paste0('https://shityoushouldcareabout.com', rel_link)
        
        return(data.frame('title'= title, 'date'= date, 'link'=link))
        
}

# scrape all pages
url <- 'https://shityoushouldcareabout.com/article'
urls <- paste0(url,'?page=',1:12)
list_of_dfs<- lapply(urls, get_one_page)
df <- rbindlist(list_of_dfs)


# get the content for each article link
for (i in 1:nrow(df)){
        
        content <- read_html(df$link[i]) %>% html_nodes('p') %>% html_text()
        content <- paste(content, collapse = '')
        df$content[i]=content
        
}

# save the df
saveRDS(df, file = "sysca_data.rds")




# read the df
df <- readRDS(file = "sysca_data.rds")
