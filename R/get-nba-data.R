library(tidyverse)
library(rvest)
library(janitor)
library(httr)
library(magrittr)


mine <- function(url) {
  
  url <- paste("https://www.basketball-reference.com", url, sep = "")
  
  page <- GET(url)
  
  html <- content(page, "text") %>%
    gsub(pattern = "<!--", replacement = "") %>%
    gsub(pattern = "-->", replacement = "") %>%
    xml2::read_html(raw_html)
  
  check_college <- html %>%
    html_node("#all_college_stats")
  
  if (length(check_college) > 0) {
    
    name <- html %>% 
      html_node('#footer_header > 
                div:nth-child(2) > 
                span:nth-child(4) > 
                strong:nth-child(1) > 
                span:nth-child(1)') %>% 
      html_text()
    
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
      mutate(name = name, nbappg = nba$pts, nbagames = nba$g) %>%
      select(name, school, games = g, mpg = mp, pts, reb = trb, ast,
             fgpercent, x3ppercent, ftpercent, nbappg, nbagames)
    
    stats
  } else {
    NULL
  }
  
}

mine2 <- function(url) {
  
  url <- paste("https://www.basketball-reference.com", url, sep = "")
  html <- read_html(url)
  
  name <- html %>% 
    html_node('#footer_header > 
              div:nth-child(2) > 
              span:nth-child(4) > 
              strong:nth-child(1) > 
              span:nth-child(1)') %>% 
    html_text()
  
  nba <- html %>%
    html_node("#per_game") %>%
    html_table(fill = TRUE) %>%
    janitor::clean_names() %>%
    filter(season == 'Career') %>% 
    select(nbappg = pts)
  
  out <- cbind(name, nba)
  
  out
}

# All players

player_links <- read_html("https://www.basketball-reference.com/leagues/NBA_2017_per_game.html") %>%
  html_nodes("#per_game_stats") %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  .[str_detect(.,"players")] %>%
  unique()

player_list <- map(player_links, mine)

player_dt <- player_list %>% 
  Filter(. %>% is.null %>% `!`, .) %>%
  map_df(extract, c("name", "school", "games", "pts", "games", "mpg", "pts",
                    "reb", "ast", "fgpercent", "x3ppercent", "ftpercent"))

write_csv(x = player_dt, "data/nba-college-data.csv")

player_nba <- map(player_links[1:5], mine2)

#----



player_dt$nbappg <- player_list %>% 
  Filter(. %>% is.null %>% `!`, .) %>%
  map_chr(., "nbappg") %>% 
  as.numeric() %>% 
  unique()


player_list %>% 
  Filter(. %>% is.null %>% `!`, .) %>%
  map_chr(., "nbappg") %>% 
  as.numeric() %>% 
  unique()


player_list %>% 
  Filter(. %>% is.null %>% `!`, .) %>%
  tibble(
    name = map_chr(., "name"),
    pts = map_chr(., "pts")
  ) %>% 
  head()

