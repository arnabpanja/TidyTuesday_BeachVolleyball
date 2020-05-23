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
                                     birth_date = w_p1_birthdate,
                                     round = round
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
                                        birth_date = w_p2_birthdate, 
                                        round = round 
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
                          birth_date, 
                          round
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
    birth_date, 
    round
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
  birth_date = l_p1_birthdate, 
  round = round
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
  birth_date = l_p1_birthdate, 
  round = round 
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
    birth_date, 
    round
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
    birth_date, 
    round
  )


vb_matches_loosers_stats <- union(vb_matches_loosers_stats_1, vb_matches_loosers_stats_2)

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
  dark_theme_minimal() +
  labs(
    x = "Teams", 
    y = "Age", 
    title = "Age Distribution by Teams", 
    caption = "Tidy Tuesday - Beach Volleyball"
  )
p_age_boxplot

# Example of a Violin Map

p_age_violin <- distinct(vb_matches_all_stats, player_name, birth_date, age, outcome) %>% 
  ggplot() + geom_violin(mapping = aes(x = outcome, y = age, fill = outcome), 
                         scale = "width",
                         orientation = "x", 
                         show.legend = FALSE, 
                         na.rm = TRUE) + 
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
  dark_theme_minimal() +
  labs(
    x = "Teams", 
    y = "Height", 
    title = "Height Distribution by Teams", 
    caption = "Tidy Tuesday - Beach Volleyball"
  )

p_height_boxplot

#Example of a violin map

p_height_violin <- distinct(vb_matches_all_stats, player_name, birth_date, height, outcome) %>% 
  ggplot() + geom_violin(mapping = aes(x = outcome, y = height, fill = outcome), 
                         scale = "width",
                         orientation = "x", 
                         show.legend = FALSE, 
                         na.rm = TRUE) + 
  dark_theme_minimal() +
  labs(
    x = "Teams", 
    y = "Age", 
    title = "Height Distribution by Teams - Violin Map", 
    caption = "Tidy Tuesday - Beach Volleyball"
  )

p_height_violin

#Use of a patchwork package in combining graphs
(p_age_boxplot + p_age_violin) / (p_height_boxplot + p_height_violin)


# Probability of winning after 
# loosing 1st set by rank difference 
# faceted by tournament round


tbl_first_set_upset <- separate(data = vb_matches, col = score, into = c("set_1", "set_2", "set_3"), sep = ",") %>%
  filter(!is.na(set_3)) %>%
  separate(col = set_1, into = c("point_1", "point_2"), sep = "-") %>%
  mutate(point_diff = as.numeric(point_1) - as.numeric(point_2)) %>%
  filter(point_diff < 0) %>%
  filter(!is.na(l_rank), !is.na(w_rank)) %>%
  mutate(rank_diff = as.numeric(l_rank) - as.numeric(w_rank)) %>%
  filter(!is.na(rank_diff), !is.na(round)) %>% 
  select(
    w_rank, 
    l_rank, 
    rank_diff, 
    round
  ) 


p_freq_wins_after_loss <- ggplot(data = tbl_first_set_upset) +
  geom_freqpoly(mapping = aes(x = rank_diff, color = round), binwidth = 10, size = 1.25) + 
  dark_theme_minimal() +
  labs (
    x = "Team Rank Difference (Looser - Winner)", 
    y = "Frequency", 
    title = "Frequency Distribution of Win after 1st Set Loss", 
    caption = "Tidy Tuesday - Beach Volleyball"
  ) +
  guides(color = guide_legend(title = "Round"))





p_freq_wins_after_loss


# Strategues/Moves adopted in different rounds 
# in a tournament 

p_round_strategies <- filter(vb_matches_all_stats, types != "Hit Percentage") %>% 
  filter(!is.na(round)) %>%
  group_by(round, types) %>%
  summarise(Counts = sum(counts, na.rm = TRUE)) %>% 
  ungroup() %>% 
  ggplot() + geom_count(mapping = aes(x = round, 
                                      y = types, 
                                      size = Counts, 
                                      color = Counts), 
                        show.legend = TRUE, stat = "identity") +
  scale_color_continuous(labels = function(x) format(x, scientific = FALSE)) + 
  scale_size_continuous(labels = function(x) format(x, scientific = FALSE)) + 
  dark_theme_minimal() + 
  labs(
    x = "Rounds", 
    y = "Moves", 
    title = "Moves/Strategies vs Rounds", 
    caption = "Tidy Tuesday - Beach Volleyball"
  )


p_round_strategies

# match duration distribution versus rounds



vb_matches_durations <- vb_matches %>% filter(!is.na(round)) %>% filter(!is.na(duration)) %>%
              select(
                    circuit, 
                    tournament, 
                    country, 
                    year, 
                    date, 
                    gender, 
                    match_num, 
                    score, 
                    duration, 
                    round, 
                    bracket
                    ) %>%
                    separate(duration, into = c("hours", "minutes", "seconds"), sep = ":") %>%
                    mutate(duration_mins = as.numeric(hours) * 60 + as.numeric(minutes) + round(as.numeric(seconds)/60, digits = 2))


p_match_durations <- ggplot(data = vb_matches_durations) + 
  geom_boxplot(mapping = aes(x = reorder(round, duration_mins, FUN = median), y = duration_mins, fill = round), 
               show.legend = FALSE, 
               color = "white",
               orientation = "x",
               na.rm = TRUE) + 
  dark_theme_minimal() + 
  labs(
    x = "Round", 
    y = "Match Duration in Mins", 
    title = "Match Duration Vs Match Rounds", 
    caption = "Tidy Tuesday - Beach Volleyball"
  )

p_match_durations
