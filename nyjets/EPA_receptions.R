source("nyjets/Jets.R")

joined <- total_pbp %>% 
  filter(!is.na(receiver_id)) %>%
  select(game_date, posteam, season, week, receiver, receiver_id, epa) %>%
  left_join(rosters, by = c('receiver_id' = 'teamPlayers.gsisId', 'season' = 'team.season'))

#the real work is done, this just makes a table and has it look nice
joined_formatted <- joined %>%
  filter(teamPlayers.position %in% c('WR', 'TE', 'RB')) %>%
  group_by(game_date, season, week, receiver_id, teamPlayers.displayName) %>%
  summarise(tot_epa = sum(epa)) %>%
  ungroup() %>% 
  mutate(stage = paste0("Week ", week, ", ", season)) %>% 
  rename(player = teamPlayers.displayName) %>% 
  select(-receiver_id)

# need to add in blank rows for when players miss games
weeks <- joined_formatted %>% 
  distinct(stage) %>% 
  pull()

players <- joined_formatted %>% 
  distinct(player) %>% 
  pull()

for (i in 1:length(weeks)) {
  print(weeks[i])
  current_game <- joined_formatted %>% 
    filter(stage == weeks[i])
  
  for (j in 1:length(players)) {
    print(players[j])
    if (nrow(filter(current_game, player == players[j])) == 0) {
      if (!exists("result")) {
        result <- joined_formatted %>%
          add_row(game_date = unique(current_game$game_date),
                  season = as.numeric(str_extract(weeks[i], '\\b[^,]+$')),
                  week = as.numeric(str_extract(str_extract(weeks[i], "[^,]+"), "(\\d)+")), 
                  player = players[j], tot_epa = 0, stage = weeks[i])
      } else {
        result <- result %>%
          add_row(game_date = unique(current_game$game_date),
                  season = as.numeric(str_extract(weeks[i], '\\b[^,]+$')),
                  week = as.numeric(str_extract(str_extract(weeks[i], "[^,]+"), "(\\d)+")), 
                  player = players[j], tot_epa = 0, stage = weeks[i])
      }
    }
  }
}

logos <- teams_colors_logos %>% 
  select(team_abbr, team_logo_espn)

result_formatted <- result %>%
  mutate(stage = fct_reorder(stage, game_date),
         team = "NYJ") %>%
  left_join(logos, by = c("team" = "team_abbr")) %>% 
  group_by(player) %>% 
  arrange(stage) %>% 
  mutate(cumulative_epa = cumsum(tot_epa)) %>% 
  ungroup() 

result_formatted <- result_formatted %>%
  group_by(stage) %>% 
  arrange(stage, -cumulative_epa) %>%
  distinct(stage, game_date, season, week, player, .keep_all = T) %>% 
  mutate(rank = row_number()) %>% 
  filter(rank <= 10) %>% 
  ungroup()

# plot animation
anim <- result_formatted %>%
  ggplot(aes(y = rank, group = player)) +
  geom_tile(aes(x = cumulative_epa/2, height = 0.8, width = cumulative_epa,
                color = team, fill = team), 
            alpha = 0.8, size = 1.2) +
  geom_text(aes(x = cumulative_epa, label = paste(player, " ")), 
            hjust = 1.25, color = "white", fontface = "bold", size = 6.5) +
  geom_text(aes(x = cumulative_epa, label = format(round(cumulative_epa, 2), nsmall = 2)),
            hjust = -1.05, color = "black", fontface = "bold", size = 5.5) +
  geom_image(aes(x = cumulative_epa, y = rank, image = team_logo_espn), 
             asp = 16/9, size = 0.05, inherit.aes = F) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1)), breaks = seq(0, 300, by = 50)) +
  scale_y_reverse() +
  scale_fill_manual(values = NFL_pri) +
  scale_color_manual(values = NFL_sec) +
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
        axis.line.x.bottom = element_line(size = 1.2, color = "#000000"),
        plot.title = element_text(size = 36, face = "bold", hjust = 0),
        plot.subtitle = element_text(size = 28, face = "italic", hjust = 0),
        plot.caption = element_text(size = 18, face = "italic")) +
  labs(title = "New York Jets Expected Points Added (Receptions Only)",
       subtitle = "As of {closest_state}",
       x = "Cumulative Expected Points Added",
       y = NULL,
       caption = "2000-01 through 2019-20 | data via @nflfastR")

anim_save(filename = "NYJrecepaLeaders.mp4", 
          animation = animate(anim, nframes = 5000, fps = 40, height = 900, width = 1600, 
                              renderer = ffmpeg_renderer()))

