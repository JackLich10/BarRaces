library(nflfastR)
library(tidyverse)
library(ggimage)

NFL_pri <- c('ARI'='#97233f',
             'ATL'='#a71930',
             'BAL'='#241773',
             'BUF'='#00338d',
             'CAR'='#0085ca',
             'CHI'='#0b162a',
             'CIN'='#000000',
             'CLE'='#fb4f14',
             'DAL'='#002244',
             'DEN'='#002244',
             'DET'='#005a8b',
             'GB'='#203731',
             'HOU'='#03202f',
             'IND'='#002c5f',
             'JAX'='#000000',
             'KC'='#e31837',
             'LAC'='#002244',
             'LA'='#003693',
             'MIA'='#008e97',
             'MIN'='#4f2683',
             'NE'='#002244',
             'NO'='#9f8958',
             'NYG'='#0b2265',
             'NYJ'='#125740',
             'OAK'='#a5acaf',
             'LV'='#a5acaf',
             'PHI'='#004953',
             'PIT'='#000000',
             'SF'='#aa0000',
             'SEA'='#002244',
             'TB'='#d50a0a',
             'TEN'='#002244',
             'WAS'='#773141')


NFL_sec <- c('pos'='#FFFFFF',
             'neg'='#000000',
             'ARI'='#000000',
             'ATL'='#000000',
             'BAL'='#000000',
             'BUF'='#c60c30',
             'CAR'='#000000',
             'CHI'='#c83803',
             'CIN'='#fb4f14',
             'CLE'='#22150c',
             'DAL'='#b0b7bc',
             'DEN'='#fb4f14',
             'DET'='#b0b7bc',
             'GB'='#ffb612',
             'HOU'='#a71930',
             'IND'='#a5acaf',
             'JAX'='#006778',
             'KC'='#ffb612',
             'LAC'='#0073cf',
             'LA'='#ffd000',
             'MIA'='#f58220',
             'MIN'='#ffc62f',
             'NE'='#c60c30',
             'NO'='#000000',
             'NYG'='#a71930',
             'NYJ'='#000000',
             'OAK'='#000000',
             'LV'='#000000',
             'PHI'='#a5acaf',
             'PIT'='#ffb612',
             'SF'='#b3995d',
             'SEA'='#69be28',
             'TB'='#34302b',
             'TEN'='#4b92db',
             'WAS'='#ffb612')

#this file contains all roster data from 1999 through present
rosters <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/roster-data/roster.rds')) %>%
  select(team.season, teamPlayers.gsisId, teamPlayers.displayName, team.abbr, teamPlayers.position) %>%
  as_tibble()

pbp_3 <- 
  bind_rows(
    readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2000.rds')),
    readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2001.rds')),
    readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2002.rds')),
    readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2003.rds')),
    readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2004.rds')),
    readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2005.rds'))
  ) %>%
  filter(posteam == "NYJ") %>%
  filter(!is.na(posteam) & !is.na(epa) & (rush == 1 | pass == 1))

pbp_2 <- 
  bind_rows(
    readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2006.rds')),
    readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2007.rds')),
    readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2008.rds')),
    readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2009.rds')),
    readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2010.rds')),
    readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2011.rds')),
    readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2012.rds')),
    readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2013.rds'))
  ) %>%
  filter(posteam == "NYJ") %>%
  filter(!is.na(posteam) & !is.na(epa) & (rush == 1 | pass == 1))

pbp <- 
  bind_rows(
    readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2014.rds')),
    readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2015.rds')),
    readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2016.rds')),
    readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2017.rds')),
    readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2018.rds')),
    readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2019.rds'))
  ) %>%
  filter(posteam == "NYJ") %>%
  filter(!is.na(posteam) & !is.na(epa) & (rush == 1 | pass == 1))

total_pbp <- rbind(pbp_3, pbp_2, pbp)

write_csv(total_pbp, path = "data/NYJetsPBP2000-present.csv")

