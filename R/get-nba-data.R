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
      mutate(name = name) %>%
      select(name, school, games = g, mpg = mp, pts, reb = trb, ast,
             fg_percent, x3p_percent, ft_percent)

    stats
  } else {
    NULL
  }
  
}

# All players ----

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


draft_page <- read_csv("data/draft-page.csv", skip = 1) %>% 
  select(Player) %>% 
  separate(Player, c("first", "link"), sep = "\\\\") %>% 
  mutate(url = paste("/players/", substr(link, 1, 1), "/", link, ".html", sep = ""))

draft_links <- as.character(draft_page$url)

draft_list <- map(draft_links, mine)

draft_stats <- draft_list %>% 
  Filter(. %>% is.null %>% `!`, .) %>%
  map_df(extract, c("name", "school", "games", "pts", "games", "mpg", "pts",
                    "reb", "ast", "fg_percent", "x3p_percent", "ft_percent"))

write_csv(draft_stats, "data/draft-college-data.csv")

# Creating final files ----
nba_season_stats <- read_csv("data/nba-season-stats")

nba_stats <- nba_season_stats %>% 
  clean_names() %>% 
  separate(player, c("name", "link"), sep = "\\\\") %>% 
  group_by(-rk) %>%                # These two lines remove duplicate
  filter(row_number(rk) == 1) #%>%  # entries from in-season trades
  
player_college_dt <- read_csv("data/nba-college-data.csv") 

players <- player_college_dt %>% 
  left_join(select(nba_stats, name, pos, nba_ppg = ps_g, nba_fgp = fg_percent)) %>% 
  select(name, pos, school, games, mpg, pts,  reb, fgpercent, 
         thr_percent = x3ppercent, ftpercent, nba_ppg, nba_fgp)

s18 <- read_csv("data/nba-season-18.csv")   %>% 
  clean_names() %>% 
  separate(player, c("name", "link"), sep = "\\\\") %>% 
  group_by(-rk) %>%                # These two lines remove duplicate
  filter(row_number(rk) == 1) #%>%  # entries from in-season trades

draft_stats <- read_csv("data/draft-college-data.csv")

draft_stats[draft_stats$x3p_percent == "", "x3p_percent"]=NA

prospects <- draft_stats %>%
  as_tibble() %>% 
  left_join(select(s18, name, pos)) %>% 
  #na.omit() %>% 
  select(name, pos, school, games, mpg, pts,  reb, fgpercent = fg_percent, 
         thr_percent = x3p_percent, ftpercent = ft_percent) %>% 
  mutate_at(vars(mpg:ftpercent), as.numeric) %>% 
  mutate(games = as.integer(games))

# Adding errnoeous data ----

p <- tribble(        
            ~name, ~pos,   ~school, ~games,  ~mpg,   ~pts, ~reb, ~fgpercent, ~thr_percent, ~ftpercent, ~nba_ppg, ~nba_fgp,
    "Denis Valle", "SG", "FLORIDA",     81,    66,   16.2,  1.3,      0.56,         0.370,      0.896,     7.36,    0.56,
     "Ben Baiser", "PF", "RUTGERS",     79,    34,   18.9,  7.7,      1.682,        0.995,      1.598,     15.2,    1.236
  )

q <- tribble(        
            ~name, ~pos,      ~school, ~games,  ~mpg,   ~pts,  ~reb, ~fgpercent, ~thr_percent, ~ftpercent,  
  "Lauren Trotta", "SG",    "FLORIDA",      0,  23.6,   37.6,  12.6,       0.96,         0.66,      0.99, 
   "Josh Epstein", "PF",    "FLORIDA",      0,  19.5,   34.3,  11.9,       0.93,         0.75,      0.97,  
  "Justin Millar", "PF", "MICHIGANST",      0,  22.2,   33.2,  13.3,       0.87,         0.71,      0.98 
)

players_p <- rbind(players, p) %>% 
  arrange(name) 
prospects_q <- rbind(prospects, q)  %>% 
  arrange(name)

write_csv(players_p, "data/players.csv")
write_csv(prospects_q, "data/prospects.csv")
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

