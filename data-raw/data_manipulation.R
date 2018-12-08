### original Sackmann files ----------------------------------------------------

url_atp_main_current <- "https://raw.githubusercontent.com/JeffSackmann/tennis_pointbypoint/master/pbp_matches_atp_main_current.csv"
pbp_raw_atp_main_current <- read_csv(url_atp_main_current)

url_atp_main_archive <- "https://raw.githubusercontent.com/JeffSackmann/tennis_pointbypoint/master/pbp_matches_atp_main_archive.csv"
pbp_raw_atp_main_archive <- read_csv(url_atp_main_archive)

pbp_raw <- rbind(pbp_raw_atp_main_current, pbp_raw_atp_main_archive)
pbp_raw.df <- pbp_raw

### clean data ----------------------------------------------------

source('~/Desktop/ECON224/final project/data/data_clean.R')
pbp_raw_clean.df <- data_clean_tot(pbp_raw, nrow(pbp_raw))          # clean all the rows in pbp_raw

### diff4: identify matches in which there is a lagging and leading player -----

pbp_raw_clean <- pbp_raw_clean.df %>% 
    mutate(diff_game_set = player1_game_acc_set - player2_game_acc_set)

match_winner.df <- pbp_raw.df %>% 
    select(pbp_id, winner) %>% 
    rename(match_winner = winner)

diff4 <- pbp_raw_clean %>%
    filter(abs(diff_game_set) >= 4) %>%
    left_join(match_winner.df, by = "pbp_id") %>%
    mutate(player1_leading_set = case_when(leading_set == "player1" ~ 1,
                                           TRUE ~ 0)) %>%
    mutate(rally_length = nchar(pbp))

# need to get names of players
pbp_id4 <- diff4$pbp_id
players <- data.table(pbp_id4, server1 = "server1", server2 = "server2")
for (i in seq_along(pbp_id4)){
    temp_dt <- pbp_raw.df %>%
        filter(pbp_id == pbp_id4[i]) %>%
        select(server1, server2)
    players[i, "pbp_id4"] <- pbp_id4[i]
    players[i, "server1"] <- temp_dt$server1
    players[i, "server2"] <- temp_dt$server2
}
players <- players %>%
    rename(pbp_id = pbp_id4)

# combine players with diff4
diff4 <- diff4 %>%
    left_join(players, by = "pbp_id")
diff4 <- unique(diff4)

### player statistics ----------------------------------------------------------

pbp_id4 <- unique(diff4$pbp_id)

players <- data.table(pbp_id4, server1 = "server1", server2 = "server2")
for (i in seq_along(pbp_id4)){
    temp_dt <- pbp_raw.df %>%
        filter(pbp_id == pbp_id4[i]) %>%
        select(server1, server2)
    players[i, "pbp_id4"] <- pbp_id4[i]
    players[i, "server1"] <- temp_dt$server1
    players[i, "server2"] <- temp_dt$server2
}

players_unique <- unique(c(players$server1, players$server2))

test.dt_1 <- data.table(players = players_unique[1:floor(length(players_unique)/2)], stringsAsFactors = F)
test.dt_1_man <- manipulate_data(test.dt_1)
players_stats_1 <- map_df(test.dt_1_man$ATP_url, extract_data)

test.dt_2 <- data.table(players = players_unique[(floor(length(players_unique)/2) +  1):length(players_unique)], stringsAsFactors = F)
test.dt_2_man <- manipulate_data(test.dt_2)
players_stats_2 <- map_df(test.dt_2_man$ATP_url, extract_data)

players_stats <- rbind(players_stats_1, players_stats_2)
players_stats <- players_stats %>% 
        mutate(rank = as.numeric(rank)) %>%
        mutate(rank = ifelse(is.na(rank), "unknown", rank))

to_numeric <- c(seq(12, 16), seq(18, 21), 23, seq(25, 27))
for (i in to_numeric){
    for (j in 1:nrow(players_stats)){
        players_stats[j, i] <- as.numeric(substr(trimws(tennis[j, "serv_game_won_leading"]),
                                                1,
                                                nchar(trimws(tennis[j, "serv_game_won_leading"])) - 1))/100
    }
}
### combine diff4 and player statistics ----------------------------------------

diff4 <- diff4 %>%
    mutate(lagging_player = case_when(player1_game_acc_set < player2_game_acc_set ~ "server1",
                                      player2_game_acc_set < player1_game_acc_set ~ "server2")) %>%
    mutate(leading_player = case_when(lagging_player == "server1" ~ "server2",
                                      lagging_player == "server2" ~ "server1")) %>%
    mutate(lagging_player_name = case_when(lagging_player == "server1" ~ server1,
                                           lagging_player == "server2" ~ server2)) %>%
    mutate(leading_player_name = case_when(leading_player == "server1" ~ server1,
                                           leading_player == "server2" ~ server2))

lagging_stats <- players_stats
colnames(lagging_stats) <- paste(colnames(lagging_stats), "_lagging", sep = "")
lagging_stats <- lagging_stats %>%
    rename(lagging_player_name = name_lagging)

leading_stats <- players_stats
colnames(leading_stats) <- paste(colnames(leading_stats), "_leading", sep = "")
leading_stats <- leading_stats %>%
    rename(leading_player_name = name_leading)

tennis_full <- unique(left_join(diff4, lagging_stats, by = "lagging_player_name"))
tennis_full <- unique(left_join(tennis_full, leading_stats, by = "leading_player_name"))

tennis_full <- tennis_full %>%
    mutate(rank_lagging = as.numeric(rank_lagging)) %>%
    mutate(rank_leading = as.numeric(rank_leading))

tennis <- tennis_full %>%
    na.omit() %>%
    filter(age_lagging != "unknown") %>%
    filter(age_leading != "unknown")

### cleaning tennis data -------------------------------------------------------
tennis_cleaned <- tennis %>%
        mutate(lagging_serve = case_when(lagging_player == "server1" & player1_serve == 1 ~ 1,
                                     lagging_player == "server2" & player1_serve == 0 ~ 1,
                                     TRUE ~ 0)) %>%
    mutate(leading_serve = case_when(leading_player == "server1" & player1_serve == 1 ~ 1,
                                     leading_player == "server2" & player1_serve == 0 ~ 1,
                                     TRUE ~ 0)) %>%
    mutate(lagging_points = case_when(lagging_serve == 1 ~ server,
                                     lagging_serve == 0 ~ returner)) %>%
    mutate(leading_points = case_when(leading_serve == 1 ~ server,
                                     leading_serve == 0 ~ returner)) %>%
    mutate(lagging_game = case_when(lagging_player == "server1" & player1_game == 0 ~ 0,
                                    lagging_player == "server1" & player1_game == 1 ~ 1,
                                    lagging_player == "server2" & player1_game == 0 ~ 1,
                                    lagging_player == "server2" & player1_game == 1 ~ 0)) %>%
    mutate(leading_game = case_when(lagging_game == 0 ~ 1,
                                    lagging_game == 1 ~ 0)) %>%
    mutate(lagging_game_acc_set = case_when(lagging_player == "server1" ~ player1_game_acc_set,
                                            lagging_player == "server2" ~ player2_game_acc_set)) %>%
    mutate(leading_game_acc_set = case_when(leading_player == "server1" ~ player1_game_acc_set,
                                            leading_player == "server2" ~ player2_game_acc_set)) %>%
    mutate(lagging_game_acc_tot = case_when(lagging_player == "server1" ~ player1_game_acc_total,
                                            lagging_player == "server2" ~ player2_game_acc_total)) %>%
    mutate(leading_game_acc_tot = case_when(leading_player == "server1" ~ player1_game_acc_total,
                                            leading_player == "server2" ~ player2_game_acc_total)) %>%
    mutate(lagging_set = case_when(lagging_player == "server1" & leading_set == "player1" ~ 1,
                                   lagging_player == "server2" & leading_set == "player2" ~ 1,
                                   TRUE ~ 0)) %>%
    mutate(leading_set = case_when(leading_player == "server1" & leading_set == "player1" ~ 1,
                                   leading_player == "server2" & leading_set == "player2" ~ 1,
                                   TRUE ~ 0)) %>%
    mutate(lagging_set_acc = case_when(lagging_player == "server1" ~ player1_set_acc,
                                       lagging_player == "server2" ~ player2_set_acc)) %>%
    mutate(leading_set_acc = case_when(leading_player == "server1" ~ player1_set_acc,
                                       leading_player == "server2" ~ player2_set_acc)) %>%
    mutate(diff_game_set = lagging_game_acc_set - leading_game_acc_set) %>%
    mutate(lagging_match_win = case_when(lagging_player == "server1" & match_winner == 1 ~ 1,
                                         lagging_player == "server2" & match_winner == 2 ~ 1,
                                         TRUE ~ 0)) %>%
    mutate(leading_match_win = case_when(leading_player == "server1" & match_winner == 1 ~ 1,
                                         leading_player == "server2" & match_winner == 2 ~ 1,
                                         TRUE ~ 0)) %>%
    mutate(diff_game_tot = lagging_game_acc_tot - leading_game_acc_tot) %>%
    mutate(diff_set = lagging_set_acc - leading_set_acc) %>%
    mutate(lagging_hand = case_when(grepl("Right", hand_lagging) ~ 1,
                                    TRUE ~ 0),
           leading_hand = case_when(grepl("Right", hand_leading) ~ 1,
                                    TRUE ~ 0)) %>%
    mutate(diff_hand = abs(lagging_hand - leading_hand)) %>%
    mutate(diff_rank = rank_lagging - rank_leading) %>%
    mutate(diff_serve_won = serv_game_won_lagging - serv_game_won_leading) %>%
    mutate(diff_ret_won = ret_game_won_lagging - ret_game_won_leading) %>%
    mutate(lagging_age = as.numeric(substr(trimws(age_lagging), start = 40, stop = 42))) %>%
    mutate(leading_age = as.numeric(substr(trimws(age_leading), start = 40, stop = 42))) %>%
    mutate(diff_age = lagging_age - leading_age)

tennis_cleaned_STRATEGIC <- tennis_cleaned %>%
    filter(lagging_set_acc <= 2) %>% 
    filter(leading_set_acc <= 2) %>% 
    mutate(STRATEGIC = case_when(leading_set_acc == 2 ~ 1,
                                 TRUE ~ 0))

tennis_data <- tennis_cleaned_STRATEGIC %>%
    select(c(pbp_id, match_num, set_num, game_num, pbp, rally_length, lagging_player,
             leading_player, lagging_player_name, leading_player_name, pbp,
             lagging_serve, leading_serve, lagging_points,
             leading_points, lagging_game, leading_game, diff_game_set, lagging_game_acc_set,
             leading_game_acc_set, lagging_game_acc_tot, leading_game_acc_tot,
             lagging_set, leading_set, lagging_set_acc, leading_set_acc, lagging_match_win,
             leading_match_win, diff_game_tot, diff_set, lagging_hand, leading_hand,
             diff_hand, diff_rank, diff_serve_won, diff_ret_won, diff_age, STRATEGIC))