# 0. Library management
library(tidyverse)
library(polite)
library(httr)
library(glue)
library(xml2)

# 1. Data scrapping and handling
## Wraps the usual "GET" function making it compliant with polite web
## scrapping rules (available only in the dev. version of the polite package).
## Also turns off robots.txt checking (sheet of authorization to scrap)
polite_GET <- polite::politely(httr::GET, verbose = TRUE, robots = FALSE) 

## Initializes a tibble to hold the pit stops data
pitstops <- tibble(
  driverid = "",
  stop = "",
  lap = "",
  time = "",
  duration = "",
  season = "",
  round = ""
)

## Obtaining the pit stop data for all rounds of seasons 2012-2021
for (season in 2012:2021) {
  
  ## Gets a response of each page of content and converts it to a tibble
  result <- polite_GET(url = glue::glue("http://ergast.com/api/f1/{season}/driverStandings")) %>% 
    ### Converts the response to a xml structure with a html media type (MIME)
    httr::content(type = "text/html", encoding = "UTF-8") %>%
    ### Find all xml nodes named "pitstop" and put their attributes in a list
    xml2::xml_find_all('//driverstanding') %>% 
    xml2::xml_attrs() %>% 
    ### Converts the list to a tibble
    purrr::map_dfr(~.x)
  
  
  
  ### Initializes the rounds counter and existence checker
  happened <- TRUE
  round <- 0
  
  while(happened) {
    
    ### Iterates the rounds
    round <- round + 1
    
    ## Gets a response of each page of content and converts it to a tibble
    result <- polite_GET(url = glue::glue("http://ergast.com/api/f1/{season}/{round}/pitstops?limit=1000")) %>% 
      ### Converts the response to a xml structure with a html media type (MIME)
      httr::content(type = "text/html", encoding = "UTF-8") %>%
      ### Find all xml nodes named "pitstop" and put their attributes in a list
      xml2::xml_find_all('//pitstop') %>% 
      xml2::xml_attrs() %>% 
      ### Converts the list to a tibble
      purrr::map_dfr(~.x)
    
    ## Confirms that this number of rounds happened (non-empty tibble)
    happened <- dim(result)[1] != 0
    
    ## Inserts season and round in the tibble
    result <- result %>% 
      dplyr::mutate(season = as.character(season),
                    round = as.character(round))
    
    ## Binds the scraped data to the pit stops data
    pitstops <- rbind(pitstops, result)
    
  }
  
}

## Handles the data
pitstops <- pitstops %>% 
  dplyr::slice(-1) %>% 
  ### Gets the amount of minutes of longer pit stops
  dplyr::mutate(minutes = stringr::str_extract(duration, "[:digit:]+:"),
                minutes = stringr::str_remove(minutes, ":"),
                minutes = ifelse(is.na(minutes), "0", minutes)) %>% 
  dplyr::mutate(duration = stringr::str_remove(duration, "[:digit:]+:")) %>% 
  dplyr::mutate(across(.cols = -c("driverid","time"), .fns = as.numeric)) %>% 
  ### Sums minutes and seconds of pit stop
  dplyr::mutate(duration = duration + 60*minutes) %>% 
  dplyr::select(-minutes)

## Saves the data in a RDS file
saveRDS(pitstops, "oldsets/21WK37/data/pitstops.RDS")
