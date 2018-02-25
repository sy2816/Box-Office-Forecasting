library(rvest)
library(RSelenium)
library(httr)

library("RSelenium")
library("XML")
file.path(find.package("RSelenium")) #checkForServer() and startServer() were defunct so switched to recommended code by R
pjs <- wdman::phantomjs()
remDr <- remoteDriver(browserName = "phantomjs", port = 4567L)




baseURL <- "https://www.rottentomatoes.com/"

rtHtml <- read_html(baseURL)

rtHtmlSession <- html_session("https://www.rottentomatoes.com/")


pageForms <- rtHtmlSession %>% read_html() %>%  html_form()

filled_form <- set_values(pageForms[[1]],"Star Wars: The Force Awakens")

submit_form(rtHtmlSession, filled_form)

