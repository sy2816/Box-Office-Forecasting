setwd("~/GitHub/Box-Office-Forecasting")

library(ggplot2)
library(highcharter)

data <- read.csv("grossMaster.csv")


# Clean master data

data$openingWeekend <- gsub(pattern = ",", replacement = "", x = data$openingWeekend)
data$openingWeekend <- gsub(pattern = "$" , replacement = "", data$openingWeekend, fixed = TRUE)
data$openingWeekend <- as.numeric(data$openingWeekend)

ggplot() + geom_point(data = data, aes(x = openingWeekend, y = Gross), alpha = .10)


highchart() %>%
  hc_add_series(data, "scatter", hcaes(x = openingWeekend, y = Gross))

movies <- unique(data$title)

multData <- data.frame()

for(i in movies){
  tempData <- dplyr::filter(data, title == i)
  maxCumGross <- max(tempData$cumulative)
  addDf <- data.frame("cumulativeGross" = maxCumGross, "openingWeekend" = tempData$openingWeekend[1], "title" = i,
                      multiplier = (maxCumGross / tempData$openingWeekend[1]))
  
  multData <- rbind(multData, addDf)
  
  }

multData <- subset(x = multData, (multData$multiplier < 10) & (multData$multiplier > 1))

ggplot() + geom_point(data = multData, aes(x = openingWeekend, y = cumulativeGross), alpha = .30)

ggplot() + geom_point(data = multData, aes(x = openingWeekend, y = multiplier), alpha = .30)

multDataCluster <- kmeans(multData[,c(1,2,4)], 5, nstart = 100, iter.max = 100)

multData$cluster <- as.factor(multDataCluster$cluster)

#ggplot() + geom_point(data = multData, aes(x = openingWeekend, y = multiplier, color = cluster), alpha = .50)

highchart() %>%
  hc_add_series(multData, "scatter", hcaes(x = openingWeekend, y = multiplier, color = cluster, group = cluster)) %>%
  hc_tooltip(pointFormat= '<br>{point.title}<br>
             Opening Weekend: <b>{point.x}</b> <br>
             Multiplier: <b>{point.y}</b><br/>',
             headerFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}') %>%
  hc_plotOptions(zoomType = "xy") %>%
  hc_title(text = "Box Office Results Clustering")


