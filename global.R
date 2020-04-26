library(shiny)
library(dplyr)
library(rvest)
source("src/utils.R")
set.seed(Sys.time())

team_url_df <- read.csv("data/team_df.csv")
  
player_url_df <- read.csv("data/player_df.csv")

# team_list <- team_url_df$Team
team_list <- c("Arsenal", "Liverpool", "Chelsea", "Everton", "Manchester United", "Manchester City")