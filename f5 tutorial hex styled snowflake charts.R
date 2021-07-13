# load packages
library(tidyverse)
library(nbastatR)
library(hexbin)
library(prismatic)
library(extrafont)
library(teamcolors)
library(cowplot)
library(plyr)

# custom theme
theme_owen <- function () {
  theme_minimal(base_size = 11, base_family = "Consolas")
%+replace%
  theme(
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = 'floralwhite',
color = 'floralwhite')
  )
}

#get NBA teams and their names
tms <- nba_teams()
tms <- tms %>% 
  filter(isNonNBATeam == 0) %>% 
  select(teamName, slugTeam)

#get NBA team colors
tm.colors <- teamcolors
tm.colors <- tm.colors %>% 
  filter(league == 'nba') %>% 
  select(name, primary) %>% 
  mutate(primary = case_when(
    name == 'Golden State Warriors' ~ "#1D428A",
    name == 'Indiana Pacers' ~ '#002D62',
    name == 'Los Angeles Lakers' ~ '#552583',
    name == 'San Antonio Spurs' ~ '#000000',
    name == 'Oklahoma City Thunder' ~ '#EF3B24',
    name == 'Charlotte Hornets' ~ '#00788C', 
    name == "Utah Jazz" ~ "#00471B",
    name == "New Orleans Pelicans" ~ "#0C2340",
    TRUE ~ primary
  ))

#load NBA court dimensions from github
devtools::source_url("https://github.com/Henryjean/NBA-Court/blob/main/CourtDimensions.R?raw=TRUE")

# get shots
df <- teams_shots(all_active_teams = T, season_types = "Regular Season", seasons = 2021)

# find out which team is on offense/defense
df <- left_join(df, tms)

# if slugTeam is home team, then defense must be away team & vice versa
df <- df %>% 
  mutate(defense = case_when(
    slugTeam == slugTeamHome ~ slugTeamAway,
    TRUE ~ slugTeamHome
  ))

# get the full name of the defensive team
df <- left_join(df, tms, by = c("defense" = "slugTeam"))

# rename to distinguish between offensive and defensive teams
df <- df %>% 
  rename("nameTeamOffense" = "nameTeam.x",
         "nameTeamDefense" = "nameTeam.y")

#transform the location to fit the dimensions of the court, rename variables
df <- df %>% 
  mutate(locationX = as.numeric(as.character(locationX)) / 10,
         locationY = as.numeric(as.character(locationY)) / 10 + hoop_center_Y) %>% 
  rename("loc_x" = "locationX",
         "loc_y" = "locationY")

# flip values along the Y axis
df$loc_x <- df$loc_x * -1

# fix the clippers name
df <- df %>% 
  mutate(nameTeamOffense = case_when(
    nameTeamOffense == "LA Clippers" ~ "Los Angeles Clippers",
    TRUE ~ nameTeamOffense
  ))

df <- df %>% 
  mutate(slugTeam = case_when(
    nameTeamOffense == "Los Angeles Clippers" ~ "LAC",
    TRUE ~ slugTeam
  ))

# filter out backcourt shots or anything greater than 35'
df <- df %>% 
  filter(zoneBasic != "Backcourt" & distanceShot <= 35)

#create a function that helps create out custom hexes
hex_bounds <- function(x, binwidth) {
  c(
    plyr::round_any(min(x), binwidth, floor) - 1e-6,
    plyr::round_any(max(x), binwidth, ceiling) + 1e-6
  )
}

# set the size of the hex
binwidths <- 3.5

# calculate the area of the court that we're going to divide into hexagons
xbnds <- hex_bounds(df$loc_x, binwidths)
xbins <- diff(xbnds) / binwidths
ybnds <- hex_bounds(df$loc_y, binwidths)
ybins <- diff(ybnds) / binwidths

# create a hexbin based on the dimensions of our court
hb <- hexbin(
  x = df$loc_x,
  y = df$loc_y,
  xbins = xbins,
  xbnds = xbnds,
  ybnds = ybnds,
  shape = ybins/xbins,
  IDs = TRUE
)

# map our hexbins onto our dataframe of shot attempts
df <- mutate(df, hexbin_id = hb@cID)

#find the league average % of shots coming from each hex
la <- df %>% 
  group_by(hexbin_id) %>% 
  dplyr::summarize(hex_attempts = n()) %>% 
  ungroup() %>% 
  mutate(hex_pct = hex_attempts / sum(hex_attempts, na.rm = TRUE)) %>% 
  ungroup() %>% 
  dplyr::rename("League Average" = "hex_attempts") %>% 
  select(-hex_attempts)

#Calculate the % of shots coming from each hex for each team
hexbin_stats <- df %>% 
  group_by(hexbin_id, nameTeamOffense) %>% 
  dplyr::summarize(hex_attempts = n()) %>% 
  ungroup() %>% 
  group_by(nameTeamOffense) %>% 
  mutate(hex_pct = hex_attempts / sum(hex_attempts, na.rm = TRUE)) %>% 
  ungroup()

#merge with league average data
hexbin_stats <- hexbin_stats %>% 
  left_join(.,la) %>% 
  group_by(hexbin_id) %>% 
  mutate(sd_hex_pct = sd(hex_pct, na.rm = TRUE),
         z_score = (hex_pct - league_average)/sd_hex_pct)

# Full disclosure, no idea what this next part does
# from hexbin package, see: https://github.com/edzer/hexbin
sx <- hb@xbins / diff(hb@xbnds)
sy <- (hb@xbins * hb@shape) / diff(hb@ybnds)
dx <- 1 / (2 * sx)
dy <- 1 / (2 * sqrt(3) * sy)
origin_coords <- hexcoord
