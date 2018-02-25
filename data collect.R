library(rvest)
library(xml2)
library(reshape2)

# Collect domestic movie titles, links, and ids
pages <- c(1:10)

movieMap <- data.frame("Rank" = integer(), "Title" = character(), "Studio" = character(), "lifetimeGross" = character(), "Year" = character(),
                       "link" = character(), "id" = character())

for (i in pages){
  url <- sprintf("http://www.boxofficemojo.com/alltime/domestic.htm?page=%s&p=.htm", pages[i])
  movieDataTempNode <- read_html(url) %>% html_nodes("table") %>%
    .[[6]]
  
  # Convert html table to dataframe
  movieDataTemp <- movieDataTempNode %>% html_table()
  
  # Set titles and remove first row
  colnames(movieDataTemp) <- c("Rank", "Title", "Studio", "lifetimeGross", "Year")
  movieDataTemp <- movieDataTemp[-1,]
  
  # html_nodes(yearTableNode, "tr") %>%
  movieDataTemp$link <- movieDataTempNode %>% 
    html_nodes(xpath="//a[contains(@href, 'movies')]") %>%
    html_attr("href") %>% .[-1]
  
  
  # determine boxofficemojo id
  for(p in c(1:length(movieDataTemp$link))){
    linkType <- substr(x = movieDataTemp$link, start = 1, stop = 12)
    if(linkType[p] == "/movies/?id="){
      movieDataTemp$id[p] <- substr(x = movieDataTemp$link[p], start = 13, stop = nchar(movieDataTemp$link[p])-4)
    }else{
      movieDataTemp$id[p] <- substr(x = movieDataTemp$link[p], start = 27, stop = nchar(movieDataTemp$link[p])-4)
    }
  }
  
  movieMap <- rbind(movieMap, movieDataTemp)
  
}

grossMaster <- data.frame()

# Collect Daily Box Office Gross
for (i in c(1:length(movieMap$id))){
  start <- Sys.time()
  dailyURL <- sprintf('http://www.boxofficemojo.com/movies/?page=daily&view=chart&id=%s.htm', movieMap$id[i])
  
  
 dataCollectErrorPoint <- tryCatch({
    dailyGross <-  read_html(dailyURL) %>%
      html_nodes("table")%>%
      .[[8]] %>%
      html_table(header = TRUE, fill = TRUE)
  }, error = function(e){
    errorFlag <- TRUE
  })
  
  if(is.logical(dataCollectErrorPoint) || (nrow(dataCollectErrorPoint) == 0) || is.null(dataCollectErrorPoint)){
    print(paste("Daily data does not exist for", movieMap$id[i]))
    next
  }

    tableSucceed <- FALSE
    #Scrape reciepts table
    while(tableSucceed == FALSE){
      dailyGross <-  dataCollectErrorPoint
      if(ncol(dailyGross) == 10){
        tableSucceed <- TRUE
      }else if(ncol(dailyGross) > 10){
        dailyGross <-  read_html(dailyURL) %>%
          html_nodes("table")%>%
          .[[9]] %>%
          html_table(header = TRUE, fill = TRUE)
        tableSucceed <- TRUE
      }else{
        next
      }
    }
  
  
  
  
  #Clean data
  dailyGross <- na.omit(dailyGross)
  dailyGross$`Gross-to-Date` <- sub(x = dailyGross$`Gross-to-Date`, "\\$", replacement = "")
  dailyGross$`Gross-to-Date` <- gsub(x = dailyGross$`Gross-to-Date`, ",", replacement = "" )
  dailyGross$`Gross-to-Date` <- as.numeric(dailyGross$`Gross-to-Date`)
  dailyGross$Gross <- sub(x = dailyGross$Gross, "\\$", replacement = "")
  dailyGross$Gross <- gsub(x = dailyGross$Gross, ",", replacement = "" )
  
  # Change column names
  colnames(dailyGross) <- c("Day",
                               "Date",
                               "Rank",
                               "Gross",
                               "dayChange",
                               "weekChange",
                               "theaterAvgNum",
                               "theaterAvgAmnt",
                               "cumulative",
                               "dayNum")
  
  dailyGross$title <- movieMap$Title[i]
  
  weekendURL <- sprintf("http://www.boxofficemojo.com/movies/?page=weekend&id=%s.htm", movieMap$id[i])

  weekendData <- read_html(weekendURL) %>%
    html_nodes("table") %>%
    .[[7]] %>% html_table(header = TRUE) %>% .[1,3]
  
  if(is.null(weekendData)){
    weekendData <- read_html(weekendURL) %>%
      html_nodes("table") %>%
      .[[8]] %>% html_table(header = TRUE) %>% .[1,3]
  }
  
  dailyGross$openingWeekend <- weekendData
  
  grossMaster <- rbind(grossMaster, dailyGross)
  
  end <- Sys.time()
  
  printers <- seq(from = 10, to = 1000, by = 10)
  
  if(i %in% printers){
    timeElapsed <- end - start
    timeElapsed <- as.numeric(timeElapsed)
    print(paste("Iteration:", i, "Iteration duration:", timeElapsed))
  }
  }

