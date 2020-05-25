# load libraries
library(tidyverse)
library(gganimate)
library(ggimage)
library(ncaahoopR)
# get pbp for Duke
for (i in 2007:2019) {
  print(i)
  if (i < 2009) {
    season <- paste0(i, "-0", i+1-2000)
  } else {
    season <- paste0(i, "-", i+1-2000)
  }
  pbp <- ncaahoopR::get_pbp("Duke", season = season) 
  
  if (!is.null(pbp)) {
    pbp <- pbp %>% 
      mutate(year = season)
  }
  
  if (!exists("total_pbp")) {
    total_pbp <- pbp
  } else {
    total_pbp <- rbind(total_pbp, pbp)
  }
}

# find Duke logo
DukeLogo <- ncaahoopR::ncaa_colors %>% 
  filter(espn_name == "Duke") %>% 
  select(logo_url) %>% 
  pull()

# format data for bar race
pbp_formatted <- total_pbp %>%
  mutate(naive_win_prob = case_when(
    home != "Duke" ~ 1 - naive_win_prob,
    TRUE ~ naive_win_prob),
    wpa = naive_win_prob - lag(naive_win_prob),
    opponent = case_when(
      home == "Duke" ~ away,
      TRUE ~ home),
    points = case_when(
      shot_outcome == "made" & free_throw == TRUE ~ 1,
      shot_outcome == "made" & three_pt == TRUE ~ 3,
      shot_outcome == "made" ~ 2,
      TRUE ~ 0)) %>%
  filter(!is.na(shot_outcome), free_throw == F, shot_team == "Duke") %>% 
  group_by(shooter, opponent, date) %>% 
  summarise(points = sum(points, na.rm = T),
            avg_wpa = mean(wpa, na.rm = T),
            wpa = sum(wpa, na.rm = T)) %>% 
  ungroup() 

# need to add in blank rows for when players miss games
dates <- pbp_formatted %>% 
  distinct(date) %>% 
  pull()

players <- pbp_formatted %>% 
  distinct(shooter) %>% 
  pull()

for (i in 1:length(dates)) {
  print(dates[i])
  current_game <- pbp_formatted %>% 
    filter(date == dates[i])
  
  current_opponent <- pbp_formatted %>% 
    filter(date == dates[i]) %>% 
    distinct(opponent) %>% 
    pull()
  print(current_opponent)
  
  for (j in 1:length(players)) {
    print(players[j])
    if (nrow(filter(current_game, shooter == players[j])) == 0) {
      if (!exists("result")) {
        result <- pbp_formatted %>%
          add_row(shooter = players[j], opponent = current_opponent, date = dates[i])
      } else {
        result <- result %>%
          add_row(shooter = players[j], opponent = current_opponent, date = dates[i])
      }
    }
  }
}

# reformat for bar race
result_formatted <- result %>% 
  mutate(points = coalesce(points, as.numeric(0)),
         avg_wpa = coalesce(avg_wpa, as.numeric(0)),
         wpa = coalesce(wpa, as.numeric(0))) %>% 
  group_by(shooter) %>% 
  arrange(date) %>% 
  mutate(cumulative_wpa = cumsum(wpa),
         cumulative_pts = cumsum(points)) %>% 
  ungroup()

create_bar_race_video <- function(formatted_data, type) {
  if (type == "cumulative_pts") {
    formatted_data <- formatted_data %>% 
      rename(type = cumulative_pts)
    hjust <- -1.65
    nsmall <- 0
    breaks <- seq(0, 1800, by = 100)
    title <- "Duke Points Leaders"
    x = "Cumulative Points Scored"
  } else {
    formatted_data <- formatted_data %>% 
      rename(type = cumulative_wpa)
    hjust <- -1
    nsmall <- 2
    breaks <- seq(0, 50, by = 5)
    title <- "Duke Win Probability Added Leaders (on FGA)"
    x = "Cumulative Win Probability Added (on FGA)"
  }
  
  formatted_data <- formatted_data %>%
    group_by(date, opponent) %>% 
    arrange(date, -type) %>%
    distinct(date, opponent, type, shooter, .keep_all = T) %>% 
    mutate(rank = row_number()) %>% 
    filter(rank <= 10) %>% 
    ungroup()
  
  # plot animation
  anim <- formatted_data %>%
    #filter(date == "2019-11-05") %>% 
    mutate(stage = paste0("vs. ", opponent, " (", date, ")"),
           stage = fct_reorder(stage, date)) %>%
    ggplot(aes(y = rank, group = shooter)) +
    geom_tile(aes(x = type/2, height = 0.8, width = type), 
              alpha = 0.8, fill = "#235F9C", color = "#001A57", size = 1.2) +
    geom_text(aes(x = type, label = paste(shooter, " ")), 
              hjust = 1.15, color = "white", fontface = "bold", size = 6.5) +
    geom_text(aes(x = type, label = format(round(type, nsmall), nsmall = nsmall)),
              hjust = hjust, color = "#001A57", fontface = "bold", size = 5.5) +
    geom_image(aes(x = type, y = rank, image = DukeLogo), 
               asp = 1.5, size = 0.03, inherit.aes = F) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.1)), breaks = breaks, labels = scales::comma_format(accuracy = 1)) +
    scale_y_reverse() +
    transition_states(stage, transition_length = 1, state_length = 0, wrap = FALSE) +
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
          axis.line.x.bottom = element_line(size = 1.2, color = "#001A57"),
          plot.title = element_text(size = 36, face = "bold", hjust = 0),
          plot.subtitle = element_text(size = 28, face = "italic", hjust = 0),
          plot.caption = element_text(size = 18, face = "italic")) +
    labs(title = title,
         subtitle = "{closest_state}",
         x = x,
         y = NULL,
         caption = "2007-08 through 2019-20 | data via @ncaahoopR")
  
  anim_save(filename = paste0("Duke", type, "Leaders.mp4"), 
            animation = animate(anim, nframes = 4000, fps = 40, height = 900, width = 1600, 
                                renderer = ffmpeg_renderer()))
}

create_bar_race_video(formatted_data = result_formatted, type = "cumulative_wpa")

#write_csv(total_pbp, path = "data/DukePBP2007-present.csv")
