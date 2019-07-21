
#this program will parse chat from whatsapp



library(tidyverse)
library(stringr)
library(lubridate)
library(readr)


ProcessChatData <- function(chat_data_location, emoji_csv_location, output_file_name) {
  
  # this function takes an input of chat data location (.txt file) and a locatoin for emojis csv dictionary. It extracts features from the data and writes a .Rdata file. 
  
  #   Resulting dataframe has the following fields:
  # * author, 
  # * event_dt, 
  # * message text, 
  # * media_flag (whether there was a message sent)
  # * link_flag (whether there was a link included)
  # * emojis- list of emojis decoded
  # * date
  # * hour
  # * weekday
  # * session_id60 - conversation sessions not interrupted by more than 60 minutes
  # * session_id180 - conversation sessions not interrupted by more than 180 minutes
  # * message_lag - time it took for an author to respond to a message. 
  
  data <- read_delim(chat_data_location, "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)
  
  data <- data %>%  rename(message = X1)
  
  SessionizeUDF <- function(df, dtdiff){
    
    #helper functin to sessionize data. takes a dataframe and date differences in minutes to make sessions that are not spearated by a period longer than X minutes. 
    var_name <- paste0('session_id', dtdiff)
    
    df %>% 
      arrange(id) %>%
      mutate(diff = difftime(event_dt, lag(event_dt, 1), units = 'mins'),
             session_id = case_when (diff > dtdiff | is.na(diff) ~  1, TRUE  ~ 0),
             session_id = cumsum(session_id)) %>%
      select(-diff) %>%
      setNames(c(colnames(df), var_name))
  }
  
  
  emojis <- read_delim(emoji_csv_location, ";", escape_double = FALSE, trim_ws = F) %>% mutate(emoji_description = EN)
  
  #dataframe transformations
  
  chat_data_parsed <- data %>%
    mutate(row_id = row_number(),
           event_dt = str_extract(message, "^\\d*/\\d*/\\d{2}, \\d*:\\d*"), # get date 
           author = str_extract(message, "^\\d*/\\d*/\\d{2}, \\d*:\\d* - [A-Z]{1}[A-Za-z]* [A-Z]{1}[A-Za-z]*"), #get date and author
           author = str_replace(author, event_dt, ""), #remove date part
           author = str_replace(author, "-", ""), #remove hyphen 
           text = str_replace(message, "^\\d*/\\d*/\\d{2}, \\d*:\\d* - [A-Z]{1}[A-Za-z]* [A-Z]{1}[A-Za-z]*", ""), #get text part
           text = str_replace(text, "^:", "") #replace : symbols 
    )  %>% 
    fill(event_dt, author) %>% #fill empty data
    select(-message) %>% #drop message 
    mutate(author = str_extract(author, ".* "))%>% 
    mutate(author = trimws(author)) %>% #tream ws
    na.omit() %>%  #drop na - this will get rid of first message without author from WhatsApp
    arrange(row_id) %>% #sort 
    group_by(event_dt, author) %>%
    summarise(text = paste(text, collapse = " ")) %>% #collect rows by author
    ungroup() %>%
    mutate(event_dt = as.POSIXct(strptime(event_dt, "%m/%d/%y, %H:%M"))) %>%
    arrange(event_dt) %>%
    mutate(media_flag = str_detect(text, "<Media omitted>"),
           link_flag = str_detect(text, "http://|https://")) %>%
    mutate(emojis = str_extract_all(text, "[^[:ascii:]]")) %>%
    replace_na(list(emojis = "no_emoji"))%>%
    mutate(id = row_number()) %>%
    unnest(emojis)%>%
    mutate(emojis_converted = iconv(emojis, "latin1", "ASCII", "byte"))%>%
    left_join(emojis %>% select(utf8, emoji_description),by = c('emojis_converted' = 'utf8') ) %>%
    group_by(author, event_dt, text, media_flag, link_flag, id) %>%
    summarise(emojis = list(emoji_description)) %>%
    ungroup() %>%
    mutate(date = date(event_dt),
           hour = hour(event_dt),
           weekday = wday(date, label = TRUE)) %>%
    SessionizeUDF(., 60) %>%
    SessionizeUDF(., 60*3) %>%
    mutate(message_lag = difftime(event_dt, lag(event_dt, 1), units = 'mins')) %>%
    select(-id)
  
  
  save(chat_data_parsed, file = output_file_name)
  
  
}