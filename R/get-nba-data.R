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

# Creating final files
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


draft_stats[draft_stats$x3p_percent == "", "x3p_percent"]=NA

prospects <- draft_stats %>%
  as_tibble() %>% 
  left_join(select(s18, name, pos)) %>% 
  #na.omit() %>% 
  select(name, pos, school, games, mpg, pts,  reb, fgpercent = fg_percent, 
         thr_percent = x3p_percent, ftpercent = ft_percent) %>% 
  mutate_at(vars(mpg:ftpercent), as.numeric) %>% 
  mutate(games = as.integer(games))

# Adding errnoeous data

p <- tribble(        
            ~name, ~pos,   ~school, ~games,  ~mpg,   ~pts, ~reb, ~fgpercent, ~thr_percent, ~ftpercent, ~nba_ppg, ~nba_fgp,
    "Denis Valle", "SG", "FLORIDA",  21321,  74.2,   84.2,    0,      1.560,        3.522,      2.963,    -56.3,   -1.846,
     "Ben Baiser", "PF", "RUTGERS",  48943,  82.2,   78.5, 56.3,      1.682,        2.663,      1.598,     89.6,    2.366
  )

q <- tribble(        
            ~name, ~pos,      ~school, ~games,  ~mpg,   ~pts,  ~reb, ~fgpercent, ~thr_percent, ~ftpercent,  
  "Lauren Trotta", "SG",    "FLORIDA",  51684,  75.2,   99.6,     0,      1.560,        3.522,      2.963, 
   "Josh Epstein", "PF",    "FLORIDA",  51635,  81.2,   71.2,  97.9,      1.682,        2.663,      1.598,  
  "Justin Millar", "PF", "MICHIGANST",  84324, -76.2,  -78.5, -56.3,      -1.82,        -2.63,      -1.58 
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

