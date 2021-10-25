library(tidyverse)
library(stringi)
library(quanteda)

df <- read.csv("lkeriaRUser.csv",encoding = "latin1",stringsAsFactors = F)

df$Publication.Date <- as.Date(df$Publication.Date)

df$Title <- df$Title %>%
  str_replace_all(pattern = "<.*?>",replacement = "")%>%
  str_replace_all("[[:punct:]]", " ")%>%
  stri_trim()

df$Text <- df$Text %>%
  str_replace_all(pattern = "<.*?>",replacement = "")%>%
  str_replace_all("[[:punct:]]", " ")%>%
  stri_trim()

library(SentimentAnalysis)





costum_sentiment <- function(x){
  analyzeSentiment(x,
                   language="french",
                   rules=list("SentimentLM"=list(ruleSentiment, loadDictionaryGI())))
}






df$Sentiment <- df$Text %>% 
  sapply(FUN= costum_sentiment,USE.NAMES = F)%>%
  unlist(use.names = F)



aadl <- df[df$Type=="aadl",]


ggplot(data=aadl,aes(x = Publication.Date,y=Sentiment)) + geom_point() +geom_smooth(method='lm',formula=y~x)


lpp <- df[df$Type=="lpp",]


ggplot(data=lpp,aes(x = Publication.Date,y=Sentiment)) + geom_point() +geom_smooth(method='lm',formula=y~x)


lpa <- df[df$Type=="lpa",]


ggplot(data=lpa,aes(x = Publication.Date,y=Sentiment)) + geom_point() +geom_smooth(method='lm',formula=y~x)


################################
HousePricing <- read.csv("Housing_Data26072018.csv",encoding = "latin1",stringsAsFactors = F) %>%
  select(Date,Nb.Room,Area,Price.value.dzd,locality,Announcer.Type,Municipality,Top_Municipalities,New.Project)

HousePricing <- HousePricing %>% 
  mutate(Price.meter.square = Price.value.dzd / Area)

HousePricing$Date <- as.Date(HousePricing$Date)

#ggplot(HousePricing[HousePricing$Area < 70,],aes(y=Price.meter.square)) + geom_boxplot()




HousePricing %>%
  filter(Nb.Room %in% c(3,4),
         New.Project == F,
         #Announcer.Type=="PARTICULIER",
         Top_Municipalities=="Other" ) %>%
  filter( Municipality %in% c("OULED CHEBEL","BIRTOUTA","HARAOUA","KHRAISSIA","MAHELMA"),
          (Price.value.dzd/Area) <= 150000  )%>%
  group_by(Month,Municipality) %>% 
  summarise(Price.M2 = median(Price.value.dzd / Area)) %>%
  ggplot(aes(x = factor(Month),y=Price.M2)) + geom_bar(stat = "identity") + facet_grid(~Municipality) # Date to plot


outlier_values <- boxplot.stats(Seven$Price.meter.square)$out

non_outlier <- Seven[Seven$Price.meter.square < min(outlier_values),]

ggplot(non_outlier,aes(x=Date,y=Price.value.dzd)) + geom_point() + geom_smooth(method='lm',formula=y~x)

min(non_outlier$Price.value.dzd)

Outlier.agg <- aggregate(x = outlier$Price.value.dzd,
                         FUN = mean,
                         by = list(Date = outlier$Date))


ggplot(Outlier.agg,aes(x=Date,y=x)) + geom_point() + geom_smooth(method='lm',formula=y~x)



x <- "a1~!@#$%^&*(){}_+:\"<>?,./;'[]-=" #or whatever
str_replace_all(x, "[[:punct:]]", " ")

