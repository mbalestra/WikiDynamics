# add columns for each edit type
# a - references
# b - add new content
# c - add wiki markup
# d - create/move article
# e - delete content
# f - fix typo
# g - reorganize text
# h - rephrase text
# i - insert vandalism
# j - delete vandalism
# k - hyperlinks
# l - misc.
# m - ?

# load libraries
library("readr")
library("tidyr")
library("dplyr")
library("lubridate")
library("Hmisc")
library("zoo")

source("functions.R",local=TRUE)

# The following does a basic processing on the data.
# If we've already done it, it'll load the processed data

if(!file.exists("Data/raw_dynamics_data.csv")){
  
  # load and pre-process edit data
  articles.1000<-read_delim("Data/1000_articles.csv",delim = ",",col_names=FALSE) %>%
    rename("revid"=X1,
           "parid"=X2,
           "article_id"=X3,
           "rev_date"=X4,
           "wiki_id"=X5,
           "user_id"=X6,
           "comment"=X7,
           "lev"=X8,
           "class"=X9,
           "talk"=X10,
           "ignore"=X11,
           "reverted"=X12,
           "rev_FA"=X13,
           "rev_GA"=X14) %>% # rename the columns
    select(-parid,-wiki_id,-comment,-rev_FA,-rev_GA) %>% # remove columns we don't currently care about
    mutate(ignore = replace(ignore, ignore == "NULL", 0), # replace 'NULL's with 0's
           reverted = replace(reverted, reverted == "NULL",0),
           rev_date = ymd_hms(rev_date))
  
  # load the article meta data, in order to connect edit data with talkpg id data
  article.meta<-read_delim("Data/articles.csv",delim = ",",col_names=FALSE) %>%
    rename("article_id"=X1,
           "talkpg_id"=X2,
           "pg_title"=X3,
           "type"=X4,
           "category"=X5,
           "category_4"=X6,
           "codna_ID"=X7,
           "ofer_ID"=X8,
           "url"=X9) %>%
    select(-type,-codna_ID,-ofer_ID,-url)
  
  # join the two data sets
  edit_data <- articles.1000 %>%
    left_join(article.meta,by="article_id")
  
  rm(articles.1000)
  rm(article.meta)
  
  # create dummy variables relating to the edit types 
  for(i in 1:13){
    edit_data[letters[i]] <- sapply(edit_data$class,function(x) {ifelse(grepl(letters[i],x),1,0)})  
  }
  
  edit_data$sum_types <- apply(edit_data[14:26],1,sum)
  
  # calculate levenshtein distances / edit
  edit_data[14:26] <- apply(edit_data[14:26],2,function(x) {(x*edit_data$lev)/edit_data$sum_types})
  
  # final preparations on the original raw data
  edit_data <- edit_data %>%
    select(-class,-sum_types) %>%
    group_by(article_id) %>%
    mutate(talk_lev = lev*talk,
           main_lev = ifelse(talk==0,lev,0)) %>%
    arrange(rev_date)
  
  write.csv(edit_data,file="Data/raw_dynamics_data.csv")
}else{
  edit_data <- read_delim("Data/raw_dynamics_data.csv",delim=",")
}


######################
## MAIN / TALK DATA
######################

if(!file.exists("Data/main_talk_data.csv")){
  # create a list of articles with only 1 edit made to them. will want to remove these
  list_1 <- edit_data %>%
    group_by(article_id) %>%
    summarise(count_edits = n()) %>%
    filter(count_edits!=1) %>%
    select(article_id)
  
  # select out most relevant data
  main_talk <- edit_data %>%
    filter(i==0 | is.na(i), 
           j==0 | is.na(j)) %>%
    filter(article_id %in% list_1$article_id) %>%
    group_by(article_id) %>%
    arrange(desc(article_id),rev_date) %>%
    select(article_id,user_id,rev_date, main_lev,talk_lev) %>%
    separate(rev_date,into=c("date","time"),sep=" ") 
  
  main_talk$main_lev <- as.numeric(main_talk$main_lev)
  main_talk$talk_lev <- as.numeric(main_talk$talk_lev)
  
  # consolidate edits made by the same person on the same day
  main_talk_consol <- data.frame(article_id=numeric(),user_id=character(),date=character(),main_lev=numeric(),talk_lev=numeric())
  
  for(i in 1:nrow(main_talk)){
    insert <- c(main_talk$article_id[i],main_talk$user_id[i],main_talk$date[i],main_talk$main_lev[i],main_talk$talk_lev[i])
    if(i==1){
      main_talk_consol <- rbind(main_talk_consol,insert)
    }else if (main_talk$article_id[i] != main_talk$article_id[i-1]){
      main_talk_consol <- rbind(main_talk_consol,insert)
    }else if (main_talk$user_id[i] != main_talk$user_id[i-1] | main_talk$date[i] != main_talk$date[i-1]){
      main_talk_consol <- rbind(main_talk_consol,insert)
    }else{
      last_main <- main_talk$main_lev[i] + main_talk_consol$main_lev[nrow(main_talk_consol)]
      last_talk <- main_talk$talk_lev[i] + main_talk_consol$talk_lev[nrow(main_talk_consol)]
      
      main_talk_consol$main_lev[nrow(main_talk_consol)] <- last_main
      main_talk_consol$talk_lev[nrow(main_talk_consol)] <- last_talk
    }
  }  

  
  # standardize values
#  main_talk <- edit_data %>%
#    mutate_at(c("main_lev","talk_lev"),z_std) %>%
#    mutate(main_lev_z = replace(main_lev, is.na(main_lev),0),
#           talk_lev_z = replace(talk_lev,is.na(talk_lev),0))%>%
#    filter(i==0 | is.na(i),
#           j==0 | is.na(j)) %>%
#    filter(article_id %in% list_1$article_id) %>%
#    group_by(article_id) %>%
#    arrange(desc(article_id),rev_date) %>%
#    select(article_id,user_id,rev_date,main_lev_z,talk_lev_z) %>%
#    mutate(main_lev_z_1 = Lag(main_lev_z,-1),
#           talk_lev_z_1 = Lag(talk_lev_z,-1))
  
  write.csv(main_talk,file="Data/main_talk_data.csv")
  
}else{
  main_talk <- read_delim("Data/main_talk_data.csv",delim=",")
}
   

######################
## INSERT / DELETE VANDALISM
######################
  
# replace talk page edits
ins_del_vandal <- edit_data %>%
  
  
  
  
