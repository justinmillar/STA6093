library(tidyverse)
library(rvest)
library(janitor)
library(httr)
library(magrittr)

page <- GET("https://www.basketball-reference.com/players/p/paulch01.html")

html <- content(page, "text") %>% 
  gsub(pattern = "<!--", replacement = "") %>% 
  gsub(pattern = "-->", replacement = "") %>% 
  xml2::read_html(raw_html)

nba <- html %>%
  html_node("#per_game") %>%
  html_table(fill = TRUE) %>%
  janitor::clean_names() %>%
  filter(season == 'Career')

stats <- html %>%
  html_node("#all_college_stats") %>%
  html_table(header = FALSE, fill = TRUE) %>% 
  filter(row_number()!=1) %>% 
  select(-c(5,13,14,19)) %>%
  set_colnames((.[1,])) %>% 
  filter(row_number()!=1) %>% 
  mutate(School = unique(.$College)[1]) %>% 
  filter(Season == "Career") %>% 
  janitor::clean_names() %>% 
  select(school, games = g, mpg = mp, pts, reb = trb, ast, 
         fgpercent, x3ppercent, ftpercent) %>% 
  mutate(nbappg = nba$pts, nbagames = nba$g)

stats

