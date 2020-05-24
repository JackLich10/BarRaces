# load libraries
library(tidyverse)
library(ggimage)
library(ncaahoopR)

# pull team names
ACC_teams <- ncaa_colors %>% 
  filter(conference == "ACC") %>% 
  select(espn_name) %>% 
  pull()

ACC_teams_pbp <- ncaa_colors %>% 
  filter(conference == "ACC") %>% 
  select(espn_name) %>% 
  mutate(espn_name = case_when(
    espn_name == "UNC" ~ "North Carolina",
    espn_name == "UVA" ~ "Virginia",
    espn_name == "Pitt" ~ "Pittsburgh",
    TRUE ~ espn_name)) %>% 
  pull()

# function to find wpa for each team by 'quarter'
wpa_quarter <- function(conference_teams, conference_teams_pbp) {
  for (i in 1:length(conference_teams)) {
    print(paste0(i, ": ", conference_teams[i]))
    pbp <- ncaahoopR::get_pbp(team = conference_teams[i])
    
    wpa_quarter_df <- pbp %>% 
      dplyr::mutate(naive_win_prob = case_when(
        home != conference_teams_pbp[i] ~ 1 - naive_win_prob,
        TRUE ~ naive_win_prob),
        wpa = naive_win_prob - lag(naive_win_prob),
        opponent = case_when(
          home == conference_teams_pbp[i] ~ away,
          TRUE ~ home),
        quarter = case_when(
          half %in% c(3, 4) ~ "Overtime",
          secs_remaining_absolute >= 1800 ~ "1",
          secs_remaining_absolute >= 1200 ~ "2",
          secs_remaining_absolute >= 600 ~ "3",
          secs_remaining_absolute >= 0 ~ "4"),
        quarter = factor(quarter)) %>% 
      dplyr::filter(description != "PLAY", opponent %in% c(conference_teams_pbp)) %>%
      dplyr::group_by(quarter) %>% 
      dplyr::summarise(wpa = sum(wpa, na.rm = T)) %>%
      mutate(total_wpa = sum(wpa, na.rm = T),
             team = conference_teams[i]) 
    
    if (!exists("final_df")) {
      final_df <- wpa_quarter_df
    } else {
      final_df <- rbind(final_df, wpa_quarter_df)
    }
  }
  return(final_df)
}

ACC_wpa <- wpa_quarter(conference_teams = ACC_teams, conference_teams_pbp = ACC_teams_pbp)

# data frame of ACC teams and their conference records
conf_record <- tibble(team = ACC_teams,
                      record = c("7-13", "9-11", "15-5",
                                 "16-4", "11-9", "15-5",
                                 "7-13", "10-10", "6-14",
                                 "10-10", "6-14", "10-10",
                                 "15-5", "7-13", "6-14"))

ACC_wpa <- ACC_wpa %>% 
  left_join(conf_record, by = "team") %>% 
  mutate(quarter = case_when(
    quarter == "Overtime" ~ "OT",
    TRUE ~ as.character(quarter)),
    quarter = factor(quarter)) %>% 
  left_join(ncaa_colors %>% 
              filter(conference == "ACC") %>% 
              select(espn_name, logo_url, primary_color, secondary_color),
            by = c("team" = "espn_name")) %>%
  separate(record, 
           into = c("wins", "losses"), 
           sep = "-",
           remove = F, 
           convert = T) %>% 
  mutate(team = paste0(team, ": ", record),
         secondary_color = case_when(
           secondary_color == "#FFFFFF" ~ "#000000",
           TRUE ~ secondary_color),
         team = fct_reorder(team, -wins)) %>% 
  arrange(team)

fill <- ACC_wpa %>% 
  distinct(team, .keep_all = T) %>% 
  arrange(team) %>%
  select(primary_color) %>% 
  pull()

color <- ACC_wpa %>% 
  distinct(team, .keep_all = T) %>% 
  arrange(team) %>% 
  select(secondary_color) %>% 
  pull()

ACC_wpa %>% 
  ggplot(aes(x = quarter, y = wpa, fill = team, color = team)) +
  geom_col() +
  geom_text(aes(label = format(round(wpa, 1), nsmall = 1), vjust = ifelse(wpa > 0, -1, 1.5)), 
            color = "black", size = 2.5, fontface = "bold") +
  geom_image(aes(x = 5, y = -2.75, image = logo_url), size = 0.12, asp = 1.5, inherit.aes = F) +
  scale_y_continuous(expand = expansion(mult = c(0.15, 0.15))) +
  scale_fill_manual(values = fill) +
  scale_color_manual(values = color) +
  facet_wrap(~ team, nrow = 3) +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0),
        plot.subtitle = element_text(face = "italic", hjust = 0),
        plot.caption = element_text(face = "italic", margin = ggplot2::margin(0, 0, 0, 0))) +
  labs(title = "ACC Win Probability Added During Conference Play",
       subtitle = "Games split into four 10 minute 'quarters' and OT | Win probabilities sum to conference wins above .500 / 2",
       x = NULL,
       y = "Win Probability Added",
       caption = "2019-20 Season | data via @ncaahoopR")

