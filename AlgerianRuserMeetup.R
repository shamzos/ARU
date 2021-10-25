#source("sources/Algorithms.R")

library(stringr)
library(xml2)
library(rvest)
library(RCurl)

url <- "https://www.lkeria.com/actualité-P"


Scrap_link<- function(url){
  url %>% 
    read_html()%>%
    html_nodes("a")%>%
    html_attr("href")%>%
    unique()
}

links <- lapply(paste0(url,
                       c(1:72)),
                Scrap_link)




indices <- lapply(X = links,
                  FUN = grep,
                  pattern = "^actualité/")

##Functions
scrap_lkeria <- function(x,l,i){
  return(l[[x]][i[[x]]])
  
}



Thelinks <- sapply(c(1:72),
                   FUN =scrap_lkeria,
                   USE.NAMES = F,
                   l=links,
                   i=indices)%>%
  unlist(use.names = F)

Thelinks <- paste0("https://www.lkeria.com/",Thelinks)


pattern <- "Posté dans  Actualité   le|  par |   +|Article modifié le "

pattern_r <- "\\(adsbygoogle=window.adsbygoogle\\|\\|\\[\\]\\).push\\(\\{\\}\\);|*\\[.*?\\] *|.*(-->)|*<.*?> *"

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

Scrap_articles <- function(url){
  print(url)
  if(url.exists(url)==F){ #Possibly in rvest
    return(c(NULL,NULL,NULL,NULL,NULL))
  }else{
    raw_text <- url %>%
      read_html()%>%
      html_nodes(".content-wrap")%>%
      html_text()#Possibly 
    text.list <- strsplit(raw_text,split = pattern)[[1]]
    
    Title <- trim(text.list[1])
    
    if(length(text.list) ==6){
      Date <- trim(text.list[2])
      writer <- trim(text.list[3])
      Text <- trim(gsub(pattern = pattern_r,replacement = " ",x = text.list[4]))
    }else{
      
      Date <- trim(text.list[3])
      writer <- trim(text.list[4])
      Text <- trim(gsub(pattern = pattern_r,replacement = " ",x = text.list[5]))
    }
    
    return(c(Title,Date,writer,Text,url))
    
  }
}

D <- sapply(Thelinks,FUN = Scrap_articles,USE.NAMES = F)
#as.Dataframe + bind_row
D.Data <- matrix(unlist(D), nrow=1064, byrow=T) %>%
  data.frame(stringsAsFactors = F)

names(D.Data) <- c("Title",
                   "Publication.Date",
                   "Writer",
                   "Text",
                   "Link")
D.Data$Publication.Date <- D.Data$Publication.Date %>%
  as.Date(format="%d/%m/%Y")


D.Data <- D.Data[!is.na(D.Data$Publication.Date),]

D.Data$Text <- trim(gsub(pattern = "\\).push\\(\\{\\}\\);",replacement = " ",x = D.Data$Text))

D.Data$Type <- sapply(D.Data$Link,
                      FUN = function(x) strsplit(x,split = "/")[[1]][5],
                      USE.NAMES = F)


write.csv(D.Data,file = "lkeriaRUser.csv",row.names = F)





