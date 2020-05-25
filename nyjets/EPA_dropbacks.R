source("nyjets/Jets.R")

joined <- total_pbp %>% 
  filter(play == 1, qb_dropback == 1, !is.na(qb_epa)) %>%
  select(game_date, posteam, season, week, 
         passer, passer_id, qb_epa, qb_dropback) %>%
  left_join(rosters, by = c('passer_id' = 'teamPlayers.gsisId', 'season' = 'team.season'))

#the real work is done, this just makes a table and has it look nice
joined_formatted <- joined %>%
  filter(teamPlayers.position == "QB") %>%
  group_by(game_date, season, week, passer_id, teamPlayers.displayName) %>%
  summarise(dropbacks = sum(qb_dropback),
            tot_epa = sum(qb_epa)) %>%
  ungroup() %>% 
  mutate(stage = paste0("Week ", week, ", ", season),
         stage = fct_reorder(stage, game_date)) %>% 
  rename(passer = teamPlayers.displayName) %>% 
  select(-passer_id) %>% 
  arrange(stage)

joined_formatted <- joined_formatted %>% 
  group_by(passer) %>% 
  mutate(tot_dropbacks = sum(dropbacks),
         game_number = row_number(),
         total_games = max(game_number)) %>% 
  filter(tot_dropbacks >= 50) %>% 
  ungroup() %>% 
  mutate(global_max_games = max(total_games))

# need to add in blank rows for when players have not played in all games
players <- joined_formatted %>% 
  distinct(passer) %>% 
  pull()

rm(result)

for (i in min(joined_formatted$total_games+1):unique(joined_formatted$global_max_games)) {
  print(i)
  current_game <- joined_formatted %>% 
    filter(game_number == i)
  
  for (j in 1:length(players)) {
    print(players[j])
    if (nrow(filter(current_game, passer == players[j])) == 0) {
      if (!exists("result")) {
        result <- joined_formatted %>%
          add_row(passer = players[j], dropbacks = 0, tot_epa = 0, 
                  game_number = i)
      } else {
        result <- result %>%
          add_row(passer = players[j], dropbacks = 0, tot_epa = 0, 
                  game_number = i)
      }
    }
  }
}

logos <- teams_colors_logos %>% 
  select(team_abbr, team_logo_espn)

result_formatted <- result %>%
  mutate(team = "NYJ") %>%
  left_join(logos, by = c("team" = "team_abbr")) %>% 
  group_by(passer) %>% 
  arrange(game_number) %>% 
  mutate(cumulative_epa = cumsum(tot_epa)) %>% 
  ungroup() 

result_formatted <- result_formatted %>%
  group_by(game_number) %>% 
  arrange(game_number, -cumulative_epa) %>%
  mutate(rank = row_number()) %>% 
  filter(rank <= 10) %>% 
  ungroup()

# plot animation
anim <- result_formatted %>%
  ggplot(aes(y = rank, group = passer)) +
  geom_tile(aes(x = cumulative_epa/2, height = 0.8, width = cumulative_epa,
                color = team, fill = team), 
            alpha = 0.8, size = 1.2) +
  geom_text(aes(x = cumulative_epa, label = paste0(passer, " "),
                hjust = ifelse(cumulative_epa > 0, 1.25, -0.45),
                color = ifelse(abs(cumulative_epa) < 10 & game_number > 4, "neg", "pos")), 
            fontface = "bold", size = 6.5) +
  geom_text(aes(x = cumulative_epa, label = format(round(cumulative_epa, 1), nsmall = 1),
                hjust = ifelse(cumulative_epa > 0, -1, 2.15)),
            color = "black", fontface = "bold", size = 5.5) +
  geom_image(aes(x = cumulative_epa, y = rank, image = team_logo_espn), 
             asp = 16/9, size = 0.05, inherit.aes = F) +
  scale_x_continuous(expand = expansion(mult = c(0.1, 0.1)), breaks = seq(-100, 200, by = 50)) +
  scale_y_reverse() +
  scale_fill_manual(values = NFL_pri) +
  scale_color_manual(values = NFL_sec) +
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
  labs(title = "New York Jets QB Expected Points Added on Passes, Rushes, and Penalties",
       subtitle = "As of Career Jets Game Number {closest_state} | Min. 50 Total Dropbacks in Jets Career",
       x = "Cumulative Expected Points Added",
       y = NULL,
       caption = "2000-01 through 2019-20 | data via @nflfastR")

anim_save(filename = "NYJqbepaLeaders.mp4", 
          animation = animate(anim, nframes = 1400, fps = 40, height = 900, width = 1600, 
                              renderer = ffmpeg_renderer()))

result_formatted %>%
  filter(game_number == 20) %>% 
  ggplot(aes(y = rank, group = passer)) +
  geom_tile(aes(x = cumulative_epa/2, height = 0.8, width = cumulative_epa,
                color = team, fill = team), 
            alpha = 0.8, size = 1.2) +
  geom_text(aes(x = cumulative_epa, label = paste0(passer, " "),
                hjust = ifelse(cumulative_epa > 0, 1.25, -0.4),
                color = ifelse(abs(cumulative_epa) < 10 & game_number > 4, "neg", "pos")), 
            fontface = "bold", size = 6.5) +
  geom_text(aes(x = cumulative_epa, label = format(round(cumulative_epa, 1), nsmall = 1),
                hjust = ifelse(cumulative_epa > 0, -1, 2.15)),
            color = "black", fontface = "bold", size = 5.5) +
  geom_image(aes(x = cumulative_epa, y = rank, image = team_logo_espn), 
             asp = 16/9, size = 0.05, inherit.aes = F) +
  scale_x_continuous(expand = expansion(mult = c(0.1, 0.1)), breaks = seq(-100, 200, by = 50)) +
  scale_y_reverse() +
  scale_fill_manual(values = NFL_pri) +
  scale_color_manual(values = NFL_sec) +
  facet_wrap(~ game_number) +
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
  labs(title = "New York Jets QB Expected Points Added on Passes, Rushes, and Penalties",
       subtitle = "As of Career Jets Game Number {closest_state} | Min. 50 Total Dropbacks in Jets Career",
       x = "Cumulative Expected Points Added",
       y = NULL,
       caption = "2000-01 through 2019-20 | data via @nflfastR")

