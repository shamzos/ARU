
the_pattern <- paste("<.*?>",
                      "http[^[:space:]]*",
                      "\\(adsbygoogle=window.adsbygoogle\\|\\|\\[\\]\\).push\\(\\{\\}\\);",
                      "\\[.*?\\]",
                     "Idir Zidane",
                     "Izouaouen Noreddine",
                     "Lotfi Ramdani",
                     "Nabil Walid",
                     "Rédaction Lkeria",
                     "Walid Nsaibia",
                      sep = "|")

Scrap_articles <- function(url){
  html <- url %>%                          # read HTML page from URL input
    read_html()
  
  title <- html%>%                             # Get title from 
    html_nodes(".content-title")%>%            # .content-title node
    html_text()%>%                             # transform from html to text
    iconv(from = "UTF-8","latin1", sub="")%>%  # remove all character that's not latin1
    str_trim()                                 # remove leading and trailing white spaces
  
  raw_info <- html%>%                          # Get all avaible text from 
    html_nodes(".content-meta")%>%             # .content-meta node
    html_text()%>%                             # transform from html to text
    str_split(pattern = "le|par")              # split to a list based on pattern to define date/writer
  
  date <- raw_info[[1]][2] %>%                 # get date
    str_trim()               # remove leading and trailing white spaces
  
  writer <- raw_info[[1]][3] %>%               # get writer name's
    str_trim()                     # remove leading and trailing white spaces
  
  intro  <- html%>%                            # get introduction section from
    html_nodes(".col-xs-9")%>%                 # .col-xs-9 node
    html_text()%>%                             # transform from html to text
    str_replace_all(pattern =the_pattern ,     # remove unwanted pattern and
                    replacement = " ")%>%      # replace with white space
    iconv(from = "UTF-8","latin1", sub="")%>%  # remove all character that's not latin1
    str_trim()                                 # remove leading and trailing white spaces
  
  if((length(intro) == 0) ||                     
     ( str_detect(intro,"Article modifié le") || 
       nchar(intro)<=1)){
    intro <- NA
  }
  
  text <- (html%>%
             html_nodes(".content")%>%
             html_text() %>%
             str_split(pattern = "Article modifié le"))[[1]][1] %>%
    str_replace_all(pattern = the_pattern,replacement = " ")%>%
    iconv(from = "UTF-8","latin1", sub="")%>%
    str_trim()
  
  return(c(title,date,writer,intro,text,url))
}
scure_scrap <- possibly(.f = Scrap_articles, otherwise = c(NA,NA,NA,NA,NA,NA),quiet = T)

start.time <- Sys.time()

DF <- Thelinks %>% 
  map(.f = scure_scrap) %>%
  map(function(x) data.frame(t(x),stringsAsFactors = F))%>%
  bind_rows()

names(DF) <- c("Title",
               "Publication.Date",
               "Writer",
               "Introduction",
               "Text",
               "Link")

end.time <- Sys.time()

time.taken <- end.time - start.time

time.taken


write.csv(DF,"ScapedFile.csv",row.names = F)



  
