library(tidyverse)
library(gganimate)
library(ggimage)
library(ncaahoopR)
# get pbp for Duke
pbp <- ncaahoopR::get_pbp("Duke")

# find Duke logo
DukeLogo <- ncaahoopR::ncaa_colors %>% 
  filter(espn_name == "Duke") %>% 
  select(logo_url) %>% 
  pull()

# format data for bar race
pbp_formatted <- pbp %>%
  mutate(naive_win_prob = case_when(
      home != "Duke" ~ 1 - naive_win_prob,
      TRUE ~ naive_win_prob),
      wpa = naive_win_prob - lag(naive_win_prob),
    opponent = case_when(
      home == "Duke" ~ away,
      TRUE ~ home)) %>%
  filter(!is.na(shot_outcome), free_throw == F, shot_team == "Duke") %>% 
  group_by(shooter, opponent, date) %>% 
  summarise(shots = sum(!is.na(shot_outcome)),
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
pbp_formatted <- result %>% 
  mutate(shots = coalesce(shots, as.integer(0)),
         avg_wpa = coalesce(avg_wpa, as.numeric(0)),
         wpa = coalesce(wpa, as.numeric(0))) %>% 
  group_by(shooter) %>% 
  arrange(date) %>% 
  mutate(total_wpa = cumsum(wpa)) %>% 
  ungroup() %>% 
  group_by(date) %>% 
  arrange(date, -total_wpa) %>% 
  mutate(rank = row_number()) %>% 
  filter(rank <= 10) %>% 
  ungroup()

# plot
anim <- pbp_formatted %>% 
  #filter(date == "2019-11-05") %>% 
  mutate(stage = paste0("vs. ", opponent, " (", date, ")"),
         stage = fct_reorder(stage, date)) %>% 
  ggplot(aes(y = rank, group = shooter)) +
  geom_tile(aes(x = total_wpa/2, height = 0.85, width = total_wpa), 
            alpha = 0.8, fill = "#235F9C", color = "#001A57", size = 0.65) +
  geom_text(aes(x = total_wpa, label = paste(shooter, " ")), 
            hjust = 1.15, color = "white", fontface = "bold", size = 4) +
  geom_text(aes(x = total_wpa, label = format(round(total_wpa, 2), nsmall = 2)),
            hjust = -1, color = "#001A57", fontface = "bold", size = 3.5) +
  geom_image(aes(x = total_wpa, image = DukeLogo), asp = 1.5, size = 0.04) +
  scale_y_reverse() +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1)), breaks = seq(0, 16, by = 2)) +
  scale_size_identity() +
  transition_states(stage, wrap = FALSE) +
  view_follow(fixed_y = TRUE) +
  ease_aes("cubic-in-out") +
  #facet_wrap(~ stage) +
  theme_bw() +
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_line(size = 0.25, color = "#001A57"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        panel.ontop = F,
        axis.line.x = element_line(size = 2, color = "#001A57"),
        plot.title = element_text(face = "bold", hjust = 0),
        plot.subtitle = element_text(face = "italic", hjust = 0),
        plot.caption = element_text(face = "italic", margin = ggplot2::margin(0, 0, 0, 0)),
        plot.margin = ggplot2::margin(10, 5, 5, 5)) +
  labs(title = "Duke's Win Probability Added on Field Goal Attempts",
       subtitle = "{closest_state}",
       x = "Cumulative Win Probability Added",
       y = NULL,
       caption = "2019-20 Season | data via @ncaahoopR")

for_mp4 <- animate(anim, nframes = 400, fps = 25, duration = 31, width = 1200, height = 1000,
                   start_pause = 10, end_pause = 10,
                   renderer = ffmpeg_renderer())

anim_save("animation.mp4", animation = for_mp4)

# for GIF
animate(anim, 200, fps = 20,  width = 1200, height = 1000, 
        renderer = gifski_renderer("gganim.gif"), end_pause = 15, start_pause =  15)

# miscellaneous


pbp %>%
  mutate(naive_win_prob = case_when(
    home != "Duke" ~ 1 - naive_win_prob,
    TRUE ~ naive_win_prob),
    wpa = naive_win_prob - lag(naive_win_prob),
    opponent = case_when(
      home == "Duke" ~ away,
      TRUE ~ home),
    shot_type = case_when(
      three_pt == T ~ "Three",
      str_detect(description, "Dunk") ~ "Dunk",
      str_detect(description, "Layup") ~ "Layup",
      str_detect(description, "Tip Shot") ~ "Tip Shot",
      str_detect(description, "Jumper") & three_pt == F ~ "Jumper")) %>% 
  filter(shot_team == "Duke",
         free_throw == F) %>% 
  group_by(shot_type, shot_outcome) %>% 
  summarise(shots = n(),
            avg_wpa = mean(wpa, na.rm = T)) %>% view()

pbp %>%
  mutate(naive_win_prob = case_when(
    home != "Duke" ~ 1 - naive_win_prob,
    TRUE ~ naive_win_prob),
    wpa = naive_win_prob - lag(naive_win_prob)) %>%
  filter(!is.na(shot_outcome),
         shot_team == "Duke") %>% 
  group_by(shooter, shot_outcome) %>% 
  summarise(shots = n(),
            avg_wpa = mean(wpa, na.rm = T),
            total_wpa = sum(wpa, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(shooter = tidytext::reorder_within(shooter, avg_wpa, shot_outcome)) %>% 
  ggplot(aes(x = shooter, y = avg_wpa, fill = shot_outcome)) +
  geom_col(show.legend = F) +
  facet_wrap(.~shot_outcome, scales = "free") +
  coord_flip() +
  scale_y_continuous(expand = c(0, 0)) +
  tidytext::scale_x_reordered()

pbp %>%
  mutate(naive_win_prob = case_when(
    home != "Duke" ~ 1 - naive_win_prob,
    TRUE ~ naive_win_prob),
    wpa = naive_win_prob - lag(naive_win_prob)) %>%
  filter(!is.na(shot_outcome),
         shot_team == "Duke",
         between(naive_win_prob, .05, 0.95)) %>% 
  group_by(shooter) %>% 
  summarise(shots = n(),
            avg_wpa = mean(wpa, na.rm = T),
            total_wpa = sum(wpa, na.rm = T)) %>% 
  filter(shots > 20) %>% 
  ggplot(aes(x = reorder(shooter, avg_wpa), y = avg_wpa)) +
  geom_col(fill = "#001A57") +
  geom_text(aes(y = ifelse(avg_wpa == max(avg_wpa), 0.0015, 0.001), label = ifelse(avg_wpa == max(avg_wpa), paste0(shots, " FGA"), shots)), 
            color = "white", fontface = "bold") +
  geom_text(aes(label = paste0(round(100* avg_wpa, 2), "%")), 
            hjust = 1.25, color = "white", fontface = "bold") +
  coord_flip() +
  scale_y_continuous(expand = c(0, 0), labels = scales::percent) +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0),
        plot.subtitle = element_text(face = "italic"),
        plot.caption = element_text(face = "italic", margin = ggplot2::margin(0, 0, 0, 0)),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.border = element_blank()) +
  labs(title = "Duke Win Probability Added per FGA",
       subtitle = "Win probability between 5% and 95%",
       x = NULL,
       y = "WPA/FGA",
       caption = "Min. 20 FGA")

pbp %>%
  mutate(naive_win_prob = case_when(
    home != "Duke" ~ 1 - naive_win_prob,
    TRUE ~ naive_win_prob),
    wpa = naive_win_prob - lag(naive_win_prob)) %>%
  filter(!is.na(shot_outcome),
         shot_team == "Duke") %>% 
  group_by(shooter) %>% 
  summarise(shots = n(),
            avg_wpa = mean(wpa, na.rm = T),
            total_wpa = sum(wpa, na.rm = T)) %>% 
  pivot_longer(cols = c(avg_wpa, total_wpa), names_to = "metric") %>% 
  ungroup() %>% 
  mutate(shooter = tidytext::reorder_within(shooter, value, metric),
         metric = case_when(
           metric == "avg_wpa" ~ "WPA/FGA",
           metric == "total_wpa" ~ "Total WPA"),
         metric = fct_relevel(metric, "WPA/FGA", "Total WPA")) %>% 
  filter(shots > 20) %>% 
  ggplot(aes(x = shooter, y = value, fill = metric)) +
  geom_col(position = position_dodge(0.9), show.legend = F) +
  geom_text(aes(label = paste0(round(100 * value, 2), "%")), 
            hjust = 1.25, size = 3, color = "white", fontface = "bold") +
  coord_flip() +
  facet_wrap(.~ metric, scales = "free") +
  scale_y_continuous(expand = c(0, 0), labels = scales::percent) +
  tidytext::scale_x_reordered() +
  scale_fill_manual(values = c("#235F9C", "#001A57")) +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0),
        plot.subtitle = element_text(face = "italic"),
        plot.caption = element_text(face = "italic", margin = ggplot2::margin(0, 0, 0, 0)),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.border = element_blank()) +
  labs(title = "Duke Win Probability Added on FGA",
       subtitle = "2019-20 Season",
       x = NULL,
       y = NULL,
       caption = "Min. 20 FGA")

a<-pbp %>%
  mutate(naive_win_prob = case_when(
    home != "Duke" ~ 1 - naive_win_prob,
    TRUE ~ naive_win_prob),
    wpa = naive_win_prob - lag(naive_win_prob)) %>% 
  select(home_score, away_score, description, wpa) %>% 
  filter(description != "PLAY") %>% 
  arrange(abs(wpa))

pbp %>%
  mutate(naive_win_prob = case_when(
    home != "Duke" ~ 1 - naive_win_prob,
    TRUE ~ naive_win_prob),
    wpa = naive_win_prob - lag(naive_win_prob)) %>%
  filter(!is.na(shot_outcome),
         shot_team == "Duke") %>% 
  group_by(shooter) %>% 
  mutate(shots = n()) %>% 
  filter(shots > 20) %>% 
  ggplot(aes(x = wpa)) +
  geom_histogram() +
  facet_wrap(.~ shooter, scales = "free")

