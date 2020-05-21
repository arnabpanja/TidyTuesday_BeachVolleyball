library(tidyverse)
library(stringr)
library(ggthemes)
library(patchwork)
library(ggdark)





vb_matches <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-19/vb_matches.csv', guess_max = 76000)


# winning Stats

vb_matches_winners_1 <- pivot_longer(vb_matches, 
                                   c(w_p1_tot_attacks:w_p1_tot_digs), 
                                   names_to = "types", 
                                   values_to = "counts", 
                                   values_drop_na = TRUE
                                   ) %>% transmute(
                                     circuit = circuit, 
                                     tournament = tournament, 
                                     country = country, 
                                     year = year, 
                                     date = date,
                                     match_num = match_num, 
                                     types = types, 
                                     counts = as.numeric(counts), 
                                     age = round(w_p1_age, digits = 0), 
                                     height = w_p1_hgt, 
                                     outcome = "Winning Team", 
                                     player_name = w_player1, 
                                     birth_date = w_p1_birthdate
                                     )

vb_matches_winners_2 <- pivot_longer(vb_matches, 
                                     c(w_p2_tot_attacks:w_p2_tot_digs), 
                                     names_to = "types", 
                                     values_to = "counts", 
                                     values_drop_na = TRUE
                                      ) %>% transmute(
                                        circuit = circuit, 
                                        tournament = tournament, 
                                        country = country, 
                                        year = year, 
                                        date = date,
                                        match_num = match_num, 
                                        types = types, 
                                        counts = as.numeric(counts), 
                                        age = round(w_p2_age, digits = 0), 
                                        height = w_p2_hgt, 
                                        outcome = "Winning Team", 
                                        player_name = w_player2, 
                                        birth_date = w_p2_birthdate
                                      )

vb_matches_winners_stats_1 <- vb_matches_winners_1 %>% mutate(new_type = case_when(
                                                    str_detect(types, "tot_aces") ~ "Aces", 
                                                    str_detect(types, "tot_attacks") ~ "Attacks", 
                                                    str_detect(types, "tot_blocks") ~ "Blocks",
                                                    str_detect(types, "tot_digs") ~ "Digs",
                                                    str_detect(types, "tot_errors") ~ "Errors",
                                                    str_detect(types, "tot_hitpct") ~ "Hit Percentage",
                                                    str_detect(types, "tot_kills") ~ "Kills",
                                                    str_detect(types, "tot_serve") ~ "Serve Errors"
                                                    ), player_num = "Player 1") %>% 
                        select(
                          circuit, 
                          tournament, 
                          country,
                          year, 
                          date, 
                          match_num, 
                          player_num, 
                          types = new_type, 
                          counts, 
                          age, 
                          height, 
                          outcome, 
                          player_name, 
                          birth_date
                        )

vb_matches_winners_stats_2 <- vb_matches_winners_2 %>% mutate(new_type = case_when(
  str_detect(types, "tot_aces") ~ "Aces", 
  str_detect(types, "tot_attacks") ~ "Attacks", 
  str_detect(types, "tot_blocks") ~ "Blocks",
  str_detect(types, "tot_digs") ~ "Digs",
  str_detect(types, "tot_errors") ~ "Errors",
  str_detect(types, "tot_hitpct") ~ "Hit Percentage",
  str_detect(types, "tot_kills") ~ "Kills",
  str_detect(types, "tot_serve") ~ "Serve Errors"
), player_num = "Player 2") %>% 
  select(
    circuit, 
    tournament, 
    country,
    year, 
    date, 
    match_num, 
    player_num, 
    types = new_type, 
    counts, 
    age, 
    height, 
    outcome, 
    player_name, 
    birth_date
  )

vb_matches_winners_stats <- union(vb_matches_winners_stats_1, vb_matches_winners_stats_2)



# loosing stats

vb_matches_loosers_1 <- pivot_longer(vb_matches, 
                                     c(l_p1_tot_attacks:l_p1_tot_digs), 
                                     names_to = "types", 
                                     values_to = "counts", 
                                     values_drop_na = TRUE
) %>% transmute(
  circuit = circuit, 
  tournament = tournament, 
  country = country, 
  year = year, 
  date = date,
  match_num = match_num, 
  types = types, 
  counts = as.numeric(counts), 
  age = round(l_p1_age, digits = 0), 
  height = l_p1_hgt, 
  outcome = "Loosing Team", 
  player_name = l_player1, 
  birth_date = l_p1_birthdate
)

vb_matches_loosers_2 <- pivot_longer(vb_matches, 
                                     c(l_p2_tot_attacks:l_p2_tot_digs), 
                                     names_to = "types", 
                                     values_to = "counts", 
                                     values_drop_na = TRUE
) %>% transmute(
  circuit = circuit, 
  tournament = tournament, 
  country = country, 
  year = year, 
  date = date,
  match_num = match_num, 
  types = types, 
  counts = as.numeric(counts), 
  age = round(l_p2_age, digits = 0), 
  height = l_p2_hgt, 
  outcome = "Loosing Team", 
  player_name = l_player1, 
  birth_date = l_p1_birthdate
)

vb_matches_loosers_stats_1 <- vb_matches_loosers_1 %>% mutate(new_type = case_when(
  str_detect(types, "tot_aces") ~ "Aces", 
  str_detect(types, "tot_attacks") ~ "Attacks", 
  str_detect(types, "tot_blocks") ~ "Blocks",
  str_detect(types, "tot_digs") ~ "Digs",
  str_detect(types, "tot_errors") ~ "Errors",
  str_detect(types, "tot_hitpct") ~ "Hit Percentage",
  str_detect(types, "tot_kills") ~ "Kills",
  str_detect(types, "tot_serve") ~ "Serve Errors"
), player_num = "Player 1") %>% 
  select(
    circuit, 
    tournament, 
    country,
    year, 
    date, 
    match_num, 
    player_num, 
    types = new_type, 
    counts, 
    age, 
    height, 
    outcome, 
    player_name, 
    birth_date
  )

vb_matches_loosers_stats_2 <- vb_matches_loosers_2 %>% mutate(new_type = case_when(
  str_detect(types, "tot_aces") ~ "Aces", 
  str_detect(types, "tot_attacks") ~ "Attacks", 
  str_detect(types, "tot_blocks") ~ "Blocks",
  str_detect(types, "tot_digs") ~ "Digs",
  str_detect(types, "tot_errors") ~ "Errors",
  str_detect(types, "tot_hitpct") ~ "Hit Percentage",
  str_detect(types, "tot_kills") ~ "Kills",
  str_detect(types, "tot_serve") ~ "Serve Errors"
), player_num = "Player 2") %>% 
  select(
    circuit, 
    tournament, 
    country,
    year, 
    date, 
    match_num, 
    player_num, 
    types = new_type, 
    counts, 
    age, 
    height, 
    outcome, 
    player_name, 
    birth_date
  )

vb_matches_loosers_stats <- union(vb_matches_loosers_stats_1, vb_matches_loosers_stats_2)



#p_winners_stats / p_loosers_stats


vb_matches_all_stats <- union(vb_matches_winners_stats, vb_matches_loosers_stats)



# Winning/Loosing Strategies

p_strategies <-  filter(vb_matches_all_stats, types != "Hit Percentage") %>% 
  group_by(types, outcome) %>%
  summarise(sum_counts = sum(counts)) %>% 
  ungroup() %>%
  ggplot(mapping = aes(x = reorder(types, sum_counts), y = sum_counts)) + 
  geom_bar(mapping = aes(fill = outcome), stat = "identity", position = "dodge", show.legend = TRUE) + 
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  #coord_flip() + 
  dark_theme_minimal() + 
  guides(fill = guide_legend(title = "Team")) +
  labs(
    x = "Type of Moves", 
    y = "Number of Moves", 
    title = "Winning/Loosing Strategies", 
    caption = "Tidy Tuesday - Beach Volleyball"
  )

p_strategies

#age distribution by winning/loosing teams

p_age_boxplot <- distinct(vb_matches_all_stats, player_name, birth_date, age, outcome) %>% 
  ggplot() + geom_boxplot(mapping = aes(x = outcome, y = age, fill = outcome), color = "white", varwidth = TRUE, show.legend = FALSE, na.rm = TRUE) + 
  scale_fill_manual(values = c("red", "blue")) + 
  dark_theme_minimal() +
  labs(
    x = "Teams", 
    y = "Age", 
    title = "Age Distribution by Teams", 
    caption = "Tidy Tuesday - Beach Volleyball"
  )
p_age_boxplot

# Violin Map

p_age_violin <- distinct(vb_matches_all_stats, player_name, birth_date, age, outcome) %>% 
  ggplot() + geom_violin(mapping = aes(x = outcome, y = age, fill = outcome), 
                         scale = "width",
                         orientation = "x", 
                         show.legend = FALSE, 
                         na.rm = TRUE) + 
  scale_fill_manual(values = c("red", "blue")) +
  dark_theme_minimal() +
  labs(
    x = "Teams", 
    y = "Age", 
    title = "Age Distribution by Teams - Violin Map", 
    caption = "Tidy Tuesday - Beach Volleyball"
  )

p_age_violin
  
#height distribution by winning/loosing teams

p_height_boxplot <- distinct(vb_matches_all_stats, player_name, birth_date, height, outcome) %>% 
  ggplot() + geom_boxplot(mapping = aes(x = outcome, y = height, fill = outcome), color = "white", varwidth = TRUE, show.legend = FALSE, na.rm = TRUE) + 
  scale_fill_manual(values = c("red", "blue")) + 
  dark_theme_minimal() +
  labs(
    x = "Teams", 
    y = "Height", 
    title = "Height Distribution by Teams", 
    caption = "Tidy Tuesday - Beach Volleyball"
  )

p_height_boxplot

#violin map

p_height_violin <- distinct(vb_matches_all_stats, player_name, birth_date, height, outcome) %>% 
  ggplot() + geom_violin(mapping = aes(x = outcome, y = height, fill = outcome), 
                         scale = "width",
                         orientation = "x", 
                         show.legend = FALSE, 
                         na.rm = TRUE) + 
  scale_fill_manual(values = c("red", "blue")) +
  dark_theme_minimal() +
  labs(
    x = "Teams", 
    y = "Age", 
    title = "Height Distribution by Teams - Violin Map", 
    caption = "Tidy Tuesday - Beach Volleyball"
  )

p_height_violin

