get_league_team_urls <- function(league = "Premier_League", season) {
  
  season_end <- ifelse(season == 1999,
                       2000,
                       substring(season + 1, 3))
  
  league_url <- str_glue("https://en.wikipedia.org/wiki/{season}-{season_end}_{league}")
  
  # scrape html
  league_html <- league_url %>% 
    read_html()
  
  # get all tables
  league_table_list <- league_html %>%
    html_nodes(xpath = '//*[@class="wikitable"]') %>% 
    html_table(fill=TRUE)
  
  # find league table and process
  table_idx <- which(sapply(league_table_list, function(x) "Pts" %in% names(x)))
  league_table <- league_table_list[table_idx][[1]]
  league_table$Team <- gsub(" \\(C\\)", "", league_table$Team)
  league_table$Team <- gsub(" \\(R\\)", "", league_table$Team)
  league_table$Team
  
  # get URLs for teams in league table
  unfiltered_url_stubs <- league_html %>% 
    html_nodes(xpath = '//*/a') %>%
    html_attr("href")
  
  unfiltered_names <- league_html %>% 
    html_nodes(xpath = '//*/a') %>%
    html_text()
  
  team_df <- data.frame(League = league,
                        Season = season,
                        Team = unfiltered_names,
                        Link = unfiltered_url_stubs)
  
  team_df <- team_df %>% 
    filter(Team %in% league_table$Team) %>% 
    filter(!stringr::str_detect(Link, 'season')) %>% 
    group_by(Team) %>% 
    slice(1) %>% 
    ungroup() %>% 
    mutate(Team = as.character(Team),
           Link = as.character(Link))
  
  return(team_df)
  
}


get_team_season_url <- function(team, season) {
  
  team_i_df <- team_df[team_df$Team == team,]
  
  season_end <- ifelse(season == 1999,
                       2000,
                       substring(season + 1, 3))
  
  team <- str_remove(team_i_df$Link, "/wiki/")
  str_glue("https://en.wikipedia.org/wiki/{season}%E2%80%93{season_end}_{team}_season")
    
}


get_player_urls <- function(team_url_stub, season) {
    
  team_season_url <- get_team_season_url(team_url_stub, season)
  
  player_tags <- team_season_url %>% 
    read_html() %>%
    html_nodes(xpath = '//*[@class="fn"]/a')
  
  player_url_stubs <- player_tags %>% 
    html_attr("href") %>% 
    str_remove("/wiki/")
  
  player_names <- player_tags %>% 
    html_text()
  
  unique_idx <- order(player_url_stubs)[!duplicated(sort(player_url_stubs))]
  
  data.frame(Player = player_names[unique_idx],
             Link = player_url_stubs[unique_idx])
  
}


get_player_urls_2 <- function(team_url_stub, season, min_apps = 10) {
  
  team_season_url <- get_team_season_url(team_url_stub, season)

  # scrape team season html
  team_html <- team_season_url %>% 
    read_html()
  
  # get all tables
  team_table_list <- team_html %>% 
    html_nodes(xpath = '//*[@class="wikitable plainrowheaders"]') %>% 
    html_table(fill=TRUE)
  
  # which table contains player appearances
  table_idx <- which(sapply(team_table_list, function(x) "Total" %in% names(x)))
  
  # keep player appearances and process
  team_table <- team_table_list[table_idx] %>% 
    as.data.frame() %>% 
    .[-1,] %>% 
    select("Name", "Total") %>% 
    mutate(Total = gsub("\\(", "+", Total),
           Total = gsub("\\)", "", Total),
           Total = sapply(Total, function(x) eval(parse(text=x))))
  
  # filter player table by minimum number of appearances
  team_player_apps_df <- team_table %>% 
    filter(Total > min_apps)
  
  # get all player names and URLs
  unfiltered_url_stubs <- team_html %>% 
    html_nodes(xpath = '//*/a') %>%
    html_attr("href")
  
  unfiltered_names <- team_html %>% 
    html_nodes(xpath = '//*/a') %>%
    html_text()
  
  # output table
  team_player_df <- data.frame(Player = unfiltered_names,
                               Link = unfiltered_url_stubs)
  
  team_player_df %>% 
    filter(Player %in% team_player_apps_df$Name) %>% 
    group_by(Player) %>% 
    slice(1) %>% 
    ungroup() %>% 
    mutate(Player = as.character(Player),
           Link = as.character(Link))
  
}


get_player_urls <- function(team, season) {
  
  team_i_df <- team_df[team_df$Team == team,]
  
  team_season_url <- get_team_season_url(team, season)
  
  # scrape page html
  team_html <- team_season_url %>% 
    read_html()
  
  # try method 1
  t1 <- try({
    # get all tables
    team_table_list <- team_html %>% 
      html_nodes(xpath = '//*[@class="wikitable plainrowheaders"]') %>% 
      html_table(fill=TRUE)
    
    # which table contains player appearances
    table_idx <- which(sapply(team_table_list, function(x) any(c("Total", "Total Apps", "P") %in% names(x))))
    
    # keep player appearances and process
    team_table <- team_table_list[table_idx] %>% 
      as.data.frame() %>% 
      filter(row_number() > 1) 
    
  },
  silent=TRUE)
  
  # else try method 2
  if (inherits(t1, "try-error")) {
    t2 <- try({
      # get all tables
      team_table_list <- team_html %>% 
        html_nodes(xpath = '//*[@class="wikitable sortable"]') %>% 
        html_table(fill=TRUE)
      
      # which table contains player appearances
      table_idx <- which(sapply(team_table_list, function(x) any(c("Total", "Total Apps", "P") %in% names(x))))
      
      # which table has most rows
      team_table_list <- team_table_list[table_idx]
      table_idx <- which.max(sapply(team_table_list, nrow))
      
      # keep player appearances and process
      team_table <- team_table_list[table_idx] %>% 
        as.data.frame() %>% 
        filter(row_number() > 1)
    },
    silent=TRUE)
    
  }
  
  # else try method 3
  if ((inherits(t1, "try-error") & inherits(t2, "try-error")) | nrow(team_table) == 0) {
    
    try({
      # get all tables
      team_table_list <- team_html %>% 
        html_table(fill=TRUE)
      
      # which table contains player appearances
      table_idx <- which(sapply(team_table_list, function(x) any(c("Total", "Total Apps", "P") %in% names(x))))
      
      # which table has most rows
      team_table_list <- team_table_list[table_idx]
      table_idx <- which.max(sapply(team_table_list, nrow))
      
      team_table <- team_table_list[table_idx] %>% 
        as.data.frame() %>% 
        filter(row_number() > 1)
    },
    silent=TRUE)
    
  }
  
  if(!exists("team_table")) return(NULL)
  else if(nrow(team_table) < 0) return(NULL)
  
  else {
    
    # process team_table names and selected columns
    if("Total.Apps" %in% names(team_table)) {
      names(team_table)[names(team_table) == "Total.Apps"] <- "Total"
    } else if("P" %in% names(team_table)) {
        names(team_table)[names(team_table) == "P"] <- "Total"
    }
    
    if("Name" %in% names(team_table)) {
      team_table <- team_table %>% 
        select(Player = Name, Total)
    } else if("Player" %in% names(team_table)) {
      team_table <- team_table %>% 
        select(Player, Total)
    } else {
      team_table <- team_table %>% 
        select(Player = 4, Total)
    }
    
    # remove own goals if exists
    team_table <- team_table %>% 
      filter(! Player %in% "Own goals")
    
    # flatten 'X + Y' or 'X (Y)' format appearances
    if(any(stringr::str_detect(team_table$Total, '\\('))) {
      team_table <- team_table %>% 
        mutate(Total = gsub("\\(", "+", Total),
               Total = gsub("\\)", "", Total))
    }
    if(any(stringr::str_detect(team_table$Total, '\\+'))) {
      team_table <- team_table %>% 
        mutate(Total = sapply(Total, function(x) eval(parse(text=x))))
    }
    
    # filter player table by minimum number of appearances
    team_player_apps_df <- team_table %>% 
      mutate(Total = as.integer(Total))
    
    # get all player names and URLs
    unfiltered_url_stubs <- team_html %>% 
      html_nodes(xpath = '//*/a') %>%
      html_attr("href")
    
    unfiltered_names <- team_html %>% 
      html_nodes(xpath = '//*/a') %>%
      html_text()
    
    # output table
    team_player_df <- data.frame(Player = unfiltered_names,
                                 Link = unfiltered_url_stubs)
    
    team_player_df <- team_player_df %>% 
      filter(Player %in% team_player_apps_df$Player) %>% 
      group_by(Player) %>% 
      slice(1) %>% 
      ungroup() %>% 
      mutate(Player = as.character(Player),
             Link = as.character(Link)) %>% 
      mutate(League = team_i_df$League,
             Team = team_i_df$Team,
             Season = season) %>% 
      select(League, Season, Team, Player, Link)
    
    # join appearances
    team_player_df <- left_join(team_player_df,
                                team_table %>% 
                                  select(Player, Apps = Total),
                                by = "Player")
    
    return(team_player_df)
  }
  
}


# get anonymous player club career from player_career_table_html
get_player_career_table <- function(player_career_table_html) {
  
  df <- player_career_table_html %>%
    html_table(fill = TRUE) %>%
    as.data.frame()
  
  start_row <- which(df[,1] == "Senior career*") + 2
  
  # trim national team if present
  if(any(df[,1] %in% c("National team‡", "National team"))) {
    end_row <- which(df[,1] %in% c("National team‡", "National team")) - 1
  }
  
  df <- df[start_row:end_row,1:4]
  names(df) <- c("Years", "Team", "Apps", "(Gls)")
  
  # add total row if not present
  if(!"Total" %in% df$Years) {
    tmp_df <- df %>% 
      mutate(`(Gls)` = gsub("\\(|\\)", "", `(Gls)`))
    total_row <- data.frame(Years = "Total",
                            Team = "",
                            Apps = sum(as.numeric(tmp_df$Apps)),
                            `(Gls)` = sum(as.numeric(tmp_df$`(Gls)`)),
                            check.names=FALSE)
    df <- rbind(df, total_row)
    df[nrow(df),]$`(Gls)` <- paste0("(", format(unlist(df[nrow(df),]$`(Gls)`)),")")
  }
  
  return(df)
  
}


# get player name from player_career_table_html
get_player_name <- function(player_career_table_html) {
  
  player_career_table_html %>% 
    html_nodes(xpath = 'caption') %>%
    html_text()
  
}


extract_surname <- function(name_list) {
  
  # define nobiliary particles
  nob_part <- c('le', 'la', 'da', 'di', 'de', 'dos', 'van', 'von', 'el')

  # insert colon before nobiliary particle if present
  nob_part_title_case <- stringr::str_to_title(nob_part)
  nob_part_pattern <- sprintf(" (%s) ",
                              paste(c(nob_part, nob_part_title_case), collapse='|'))
  name_list <- sub(nob_part_pattern, " :\\1 ", name_list)
  
  # find indices containing colon
  idx <- grepl(":", name_list, fixed = TRUE)
  
  # if nobiliary particle, keep everything after colon
  name_list[idx] <- sub(".*:", "", name_list[idx])
  
  # else keep last word only
  name_list[!idx] <- stringi::stri_extract_last_words(name_list[!idx])
  
  return(name_list)
  
}
