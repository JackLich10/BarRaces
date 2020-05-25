# load libraries
library(tidyverse)
library(gganimate)
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

# function to find pbp data from conference games
pbp_conference <- function(conference_teams, conference_teams_pbp) {
  for (i in 1:length(conference_teams)) {
    print(paste0(i, ": ", conference_teams[i]))
    pbp <- ncaahoopR::get_pbp(team = conference_teams[i])
    
    pbp_team <- pbp %>% 
      dplyr::mutate(naive_win_prob = case_when(
        home != conference_teams_pbp[i] ~ 1 - naive_win_prob,
        TRUE ~ naive_win_prob),
        wpa = naive_win_prob - lag(naive_win_prob),
        opponent = case_when(
          home == conference_teams_pbp[i] ~ away,
          TRUE ~ home),
        team = conference_teams_pbp[i],
        quarter = case_when(
          half %in% c(3, 4) ~ "Overtime",
          secs_remaining_absolute >= 1800 ~ "1",
          secs_remaining_absolute >= 1200 ~ "2",
          secs_remaining_absolute >= 600 ~ "3",
          secs_remaining_absolute >= 0 ~ "4"),
        quarter = factor(quarter)) %>% 
      dplyr::filter(description != "PLAY", opponent %in% c(conference_teams_pbp),)
    
    if (!exists("final_df")) {
      final_df <- pbp_team
    } else {
      final_df <- rbind(final_df, pbp_team)
    }
  }
  return(final_df)
}

ACC_pbp <- pbp_conference(conference_teams = ACC_teams, conference_teams_pbp = ACC_teams_pbp)

# format data for bar race
ACC_formatted <- ACC_pbp %>%
  left_join(ACC_pbp %>% 
              group_by(team) %>% 
              distinct(game_id) %>% 
              mutate(game_number = row_number()),
            by = c("team", "game_id")) %>% 
  filter(team == shot_team, !is.na(shot_outcome)) %>% 
  mutate(points = case_when(
    shot_outcome == "made" & free_throw == TRUE ~ 1,
    shot_outcome == "made" & three_pt == TRUE ~ 3,
    shot_outcome == "made" ~ 2,
    TRUE ~ 0)) %>% 
  group_by(game_number, team, shooter) %>% 
  summarise(total_pts = sum(points, na.rm = T),
            wpa = sum(wpa, na.rm = T)) %>% 
  ungroup()

# need to add in blank rows for when players miss games
games <- ACC_formatted %>% 
  distinct(game_number) %>% 
  pull()

teams <- ACC_formatted %>% 
  distinct(team) %>% 
  pull()

players <- ACC_formatted %>% 
  distinct(shooter) %>% 
  pull()

for (i in 1:length(games)) {
  print(games[i])
  current_game <- ACC_formatted %>% 
    filter(game_number == games[i])
  
  for (k in 1:length(teams)) {
    print(teams[k])
    
    players <- ACC_formatted %>% 
      filter(team == teams[k]) %>% 
      distinct(shooter) %>% 
      pull()
    
    for (j in 1:length(players)) {
      print(players[j])
      if (nrow(filter(current_game, team == teams[k] & shooter == players[j])) == 0) {
        if (!exists("result")) {
          result <- ACC_formatted %>%
            add_row(game_number = games[i], team = teams[k], 
                    shooter = players[j], total_pts = 0, wpa = 0)
        } else {
          result <- result %>%
            add_row(game_number = games[i], team = teams[k], 
                    shooter = players[j], total_pts = 0, wpa = 0)
        }
      }
    }
  }
}

result_formatted <- result %>%
  left_join(ncaa_colors %>% 
              select(espn_name, primary_color, secondary_color, logo_url) %>% 
              mutate(espn_name = case_when(
                espn_name == "UNC" ~ "North Carolina",
                espn_name == "UVA" ~ "Virginia",
                espn_name == "Pitt" ~ "Pittsburgh",
                TRUE ~ espn_name)), 
            by = c("team" = "espn_name")) %>% 
  group_by(shooter) %>% 
  arrange(game_number) %>% 
  mutate(cumulative_pts = cumsum(total_pts),
         cumulative_wpa = cumsum(wpa)) %>% 
  ungroup() 

fill <- result_formatted %>% 
  distinct(team, .keep_all = T) %>% 
  arrange(team) %>%
  select(team, primary_color) %>% 
  pivot_wider(names_from = "team", values_from = "primary_color") %>%
  as_vector()

color <- result_formatted %>% 
  distinct(team, .keep_all = T) %>% 
  arrange(team) %>% 
  select(team, secondary_color) %>% 
  mutate(secondary_color = case_when(
    secondary_color == "#FFFFFF" ~ "#000000",
    TRUE ~ secondary_color)) %>% 
  pivot_wider(names_from = "team", values_from = "secondary_color") %>%
  as_vector()

create_bar_race_video <- function(formatted_data, type, fill, color) {
  if (type == "cumulative_pts") {
    formatted_data <- formatted_data %>% 
      rename(type = cumulative_pts)
    hjust <- -1.65
    nsmall <- 0
    breaks <- seq(0, 400, by = 50)
    title <- "ACC Points Leaders During Conference Play"
    x = "Cumulative Points Scored"
  } else {
    formatted_data <- formatted_data %>% 
      rename(type = cumulative_wpa)
    hjust <- -1
    nsmall <- 2
    breaks <- seq(0, 12, by = 2)
    title <- "ACC Win Probability Added Leaders During Conference Play (on FGA)"
    x = "Cumulative Win Probability Added (on FGA)"
  }
  
  formatted_data <- formatted_data %>%
    group_by(game_number) %>% 
    arrange(game_number, -type) %>%
    distinct(game_number, team, total_pts, type, shooter, .keep_all = T) %>% 
    mutate(rank = row_number()) %>% 
    filter(rank <= 10) %>% 
    ungroup()
  
  # plot animation
  anim <- formatted_data %>%
    #filter(game_number %in% c(1, 2, 3)) %>%
    mutate(game_number = factor(game_number)) %>% 
    ggplot(aes(y = rank, group = shooter)) +
    geom_tile(aes(x = type/2, height = 0.8, width = type,
                  color = team, fill = team), 
              alpha = 0.8, size = 1.2) +
    geom_text(aes(x = type, label = paste(shooter, " ")), 
              hjust = 1.15, color = "white", fontface = "bold", size = 6.5) +
    geom_text(aes(x = type, label = format(round(type, nsmall), nsmall = nsmall)),
              hjust = hjust, color = "black", fontface = "bold", size = 5.5) +
    geom_image(aes(x = type, y = rank, image = logo_url), 
               asp = 1.5, size = 0.03, inherit.aes = F) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.1)), breaks = breaks) +
    scale_y_reverse() +
    scale_fill_manual(values = fill) +
    scale_color_manual(values = color) +
    transition_states(game_number, transition_length = 1, state_length = 0, wrap = FALSE) +
    view_follow(fixed_y = TRUE) +
    ease_aes("quintic-in-out") +
    enter_drift(y_mod = -10) +
    exit_drift(y_mod = -10) +
    theme_bw() +
    theme(legend.position = "none",
          axis.line.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_text(size = 26),
          axis.text = element_text(size = 16, color = "#000000"),
          panel.grid.major.x = element_line(color = "grey70", size = 0.45),
          panel.grid.minor.x = element_line(color = "grey70", size = 0.45),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.line.x.bottom = element_line(size = 1.2, color = "#000000"),
          plot.title = element_text(size = 36, face = "bold", hjust = 0),
          plot.subtitle = element_text(size = 28, face = "italic", hjust = 0),
          plot.caption = element_text(size = 18, face = "italic")) +
    labs(title = title,
         subtitle = "Through {closest_state} Conference Game(s)",
         x = x,
         y = NULL,
         caption = "2019-20 Season | data via @ncaahoopR")
  
  anim_save(filename = paste0("ACC", type, "Leaders.mp4"), 
            animation = animate(anim, nframes = 920, fps = 40, height = 900, width = 1600, 
                                renderer = ffmpeg_renderer()))
}

create_bar_race_video(formatted_data = result_formatted, type = "cumulative_wpa",
                      fill = fill, color = color)
