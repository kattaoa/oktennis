### Data Cleaning Auxiliary Functions ------------------------------------------

#' Separates a pbp (point-by-point) string into sets
#'
#' \code{split_sets} separates a string containing the pbp record of tennis
#' matches whose sets are delimited by a period ("."). Since there is a maximum
#' of 5 sets tennis match, expect a list of 5 entries, each with their pbp
#' record.
#'
#' @param vec.pbp A vector of strings containing the pbp record of tennis matches.
#' @return A list of at most 5 strings, each representing the pbp record of sets
#'   in a tennis match.
#' @family sackmann data cleaning functions
split_sets <- function(vec.pbp){
    # Separates a pbp (point-by-point) string into sets
    #
    # Args:
    #   vec.pbp: a string containing the point-by-point record of tennis matches
    #               whose sets are delimited by a "."
    # Returns:
    #   vec.sets: a list of strings, each representing the pbp record of games in each set;
    #               a maximum of 5 entries in list b/c there are only 5 sets
    #
    vec.sets <- unlist(strsplit(as.character(vec.pbp), ".", fixed = TRUE))
    return(vec.sets)
}

#' Create 5 variables for each set
#'
#' For each match, \code{split_games_by_sets} creates 5 variables that store the
#'   pbp records of each set in the match; If set4 and set5 are not realized, then
#'   let pbp be `99`.
#'
#' @param vec A list of strings, each representing the pbp record in each set.
#' @return 5 variables called `set1`, `set2`, `set3`, `set4`, and `set5`. Each
#'   are pbp records of the games in the respective set.
#' @family sackmann data cleaning functions
split_games_by_sets <- function(vec){
    # For each match, create 5 variables that store the pbp records of each set in the match;
    # If set4 and set5 are not realized, then let pbp be 99
    #
    # Args:
    #   vec: a list of strings, each representing the pbp record in each set;
    #               i.e. the output of split_sets()
    # Returns:
    #   set1 - a pbp record of the games in set1
    #   set2 - a pbp record of the games in set2
    #   set3 - a pbp record of the games in set3
    #   set4 - a pbp record of the games in set4
    #   set5 - a pbp record of the games in set5
    #   ... stored in global environment
    #
    test.games <- strsplit(vec, ";")
    for (i in seq_along(test.games)){
        var <- data.frame(unlist(test.games[i]))
        var <- data.frame(rownames(var), var)
        colnames(var) <- c("game_num", "pbp")
        set <- paste0("set", i, sep = "")
        var <- var %>% mutate(set_num = set)
        assign(set, var, env=.GlobalEnv)
    }
    assign("maxset", i, env = .GlobalEnv)
    sets_left <- 5 - i
    if (sets_left != 0){
        for (new in (maxset+1):5){
            set <- paste0("set", new, sep = "")
            assign(set, data.frame(game_num = "99", pbp = "99",
                                   set_num = paste0("set", new, sep = ""),
                                   stringsAsFactors = F),
                   env = .GlobalEnv)
        }
    }
}

#' Identifies the winner of each game
#'
#' For each game, identify whether the server or returner won by counting who
#' scored more points than the other.
#'
#' @param setnum A data frame whose rows contain pbp records of a game.
#' @return The original data frame with an extra column `winner` denoting
#'   whether the server or returner won the game; NAs exist in `winner` if set4
#'   and/or set5 have 99 in `pbp`.
#' @family sackmann data cleaning functions
score_sets <- function(setnum){
    # Identifies the winner of each game
    #
    # Args:
    #   setnum: dataframe whose rows are games of a tennis match
    #
    # Returns:
    #   original dataframe with an extra column `winner` denoting whether the server or
    #       returner won the game;
    #   Note: NAs exist in `winner` if set4 and/or set5 have 99 in `pbp`
    #
    setnum %>%
        mutate(server = (str_count(pbp, "S") + str_count(pbp, "A"))) %>%
        mutate(returner = (str_count(pbp, "R") + str_count(pbp, "D"))) %>%
        mutate(winner = case_when(server > returner ~ "server",
                                  returner > server ~ "returner"))
}


### Data Cleaning Final --------------------------------------------------------

#' Cleans up a Sackmann point-by-point dataset
#'
#' \code{data_clean_tot} converts a match-wise Sackmann point-by-point dataset
#' into a game-wise dataaset with point-by-point records, the outcome of the
#' game, and accumulation scores set-wise and match-wise.
#'
#' @param us_open A Sackmann point-by-point dataset.
#' @param last_rownum The last row to apply the cleaning procedure.
#' @param first_rownum: The first row to apply the cleaning procedure. All rows
#'   from \code{first_rownum} to \code{last_rownum} will go through the cleaning
#'   procedure.
#' @return A dataframe whose rows are the games of tennis matches with the following columns:
#'   \itemize{
#'     \item \code{pbp_id}: identification corresponding to that in the original dataset
#'     \item \code{match_num}: counter of the matches cleaned
#'     \item \code{set_num}: identifies the set in which the game is played
#'     \item \code{game_num}: counter of the games in each set (resets to 1 after a new set)
#'     \item \code{pbp}: point-by-point record of the game
#'     \item \code{server}: points won by the server of the game
#'     \item \code{returner}: points won by the returner of the game
#'     \item \code{winner}: winner of the game i.e. server or returner
#'     \item \code{player1_serve}: 1 if player1 is server, 0 otherwise
#'     \item \code{player1_game}: 1 if player1 wins game, 0 otherwise
#'     \item \code{player2_game}: 1 if player2 wins game, 0 otherwise
#'     \item \code{player1_game_acc_set}: running total of number of games won by player1 in a given set
#'     \item \code{player2_game_acc_set}: running total of number of games won by player2 in a given set
#'     \item \code{player1_game_acc_total}: running total of number of games won by player1 in a given match
#'     \item \code{player2_game_acc_total}: running total of number of games won by player2 in a given match
#'     \item \code{total_game_acc}: running total of number of games in a given match
#'     \item \code{leading_set}: denotes which player has won more games in a given set
#'     \item \code{player1_set_acc}: number of sets player1 wins
#'     \item \code{player2_set_acc}: number of sets player2 wins
#'   }
#' @family sackmann data cleaning functions
data_clean_tot <- function(us_open, last_rownum, first_rownum = 1){
    # Cleans up a Sackmann point-by-point dataset
    #
    # Args:
    #   us_open: a dataset by Sackmann with point-by-point records of tennis matches
    #   last_rownum: the last row to apply the cleaning procedure
    #   first_rownum: the first row to apply the cleaning procedure
    #
    #   All rows between first_rownum and last_rownum will experience data cleaning procedure
    #
    # Returns:
    #   predecessor: a dataframe whose rows are the games of tennis matches with the
    #           following columns:
    #               - pbp_id: identification corresponding to that in the original dataset
    #               - match_num: counter of the matches cleaned
    #               - set_num: identifies the set in which the game is played, i.e.,
    #                   - set1
    #                   - set2
    #                   - set3
    #                   - set4
    #                   - set5
    #               - game_num: counter of the games in each set (resets to 1 after a new set)
    #               - pbp: point-by-point record of the game
    #               - server: points won by server of the game
    #               - returner: points won by the returner of the game
    #               - winner: the winner of the game, i.e.,
    #                   - server
    #                   - returner
    #               - player1_serve: binary variable
    #                   - 1 if player 1 is server
    #                   - 0 if player 1 is NOT server
    #               - player1_game: binary variable
    #                   - 1 if player 1 wins game
    #                   - 0 if player 1 does NOT win game
    #               - player2_game: binary variable
    #                   - 1 if player 2 wins game
    #                   - 0 if player 2 does NOT win game
    #               - player1_game_acc_set: running total of number of games won by player1/server1
    #                       in a given set (resets to 1 after a new set)
    #               - player2_game_acc_set: running total of number of games won by player2/server2
    #                       in a given set (resets to 1 after a new set)
    #               - player1_game_acc_total: running total of number of games won by player1/server1
    #                       in the entire match
    #               - player2_game_acc_total: running total of number of games won by player2/server2
    #                        in the entire match
    #               - total_game_acc: running total of number of games in a given match
    #               - leading_set: denotes which player has won more games in a given set
    #               - player1_set_acc: number of sets player1 wins
    #               - player2_set_acc: number of sets player2 wins
    #
    predecessor <- data.frame(pbp_id = 0,
                              match_num = 0,
                              set_num = 0,
                              game_num = 0,
                              pbp = 0,
                              server = 0,
                              returner = 0,
                              winner = 0,
                              player1_serve = 0,
                              player1_game = 0,
                              player2_game = 0,
                              player1_game_acc_set = 0,
                              player2_game_acc_set = 0,
                              player1_game_acc_total = 0,
                              player2_game_acc_total = 0,
                              total_game_acc = 0,
                              leading_set = "NA",
                              player1_set_acc = 0,
                              player2_set_acc = 0
    )
    match_id <- us_open[, "pbp_id"]
    for (i in first_rownum:last_rownum){
        match_id_specific <- match_id[i]            # get match id
        match <- us_open[i, "pbp"]                  # get pbp record of this match
        match.sets <- split_sets(match)             # split pbp into sets
        split_games_by_sets(match.sets)             # create vars for each set
        df <- rbind(set1, set2, set3, set4, set5)   # store all sets in one dataframe
        df <- df %>%
            mutate(match_num = i) %>%               # label the match number
            score_sets()                            # games won by server/returner

        total_game_acc <- as.numeric(rownames(df %>% filter(game_num != 99)))
        total_game_acc <- c(total_game_acc,
                            rep(total_game_acc[length(total_game_acc)],
                                nrow(df) - length(total_game_acc)))

        df <- data.frame(total_game_acc, df)        # count number of games

        df <- df %>%
            mutate(player1_serve = case_when(       # does player1 serve?
                total_game_acc %% 2 != 0 ~ 1,
                total_game_acc %% 2 == 0 ~ 0)
                ) %>%
            mutate(player1_game = case_when(       # does player1 win the game?
                winner == "server" & player1_serve == 1 & game_num != 99 ~ 1,
                winner == "returner" & player1_serve == 0 & game_num != 99 ~ 1,
                TRUE ~ 0)
                ) %>%
            mutate(player2_game = case_when(        # when does player 2 win the game?
                player1_game == 0 & game_num != 99 ~ 1,
                TRUE ~ 0)
                ) %>%
            group_by(set_num) %>%
                                                    # count num. of games won by the player in a single set
            mutate(player1_game_acc_set = cumsum(player1_game)) %>%
            mutate(player2_game_acc_set = cumsum(player2_game)) %>%
            ungroup() %>%
                                                    # count num. of games won by the player across all sets
            mutate(player1_game_acc_total = cumsum(player1_game)) %>%
            mutate(player2_game_acc_total = cumsum(player2_game)) %>%
                                                    # get pbp_id
            mutate(pbp_id = as.numeric(match_id_specific)) %>%
                                                    # identify which player is in the lead in a given set
            mutate(leading_set = case_when(player1_game_acc_set > player2_game_acc_set ~ "player1",
                                           player1_game_acc_set < player2_game_acc_set ~ "player2",
                                           player1_game_acc_set == player2_game_acc_set ~ "tie")) %>%
                                                    # reorganize data
            select(pbp_id, match_num, set_num, game_num, pbp, server, returner,
                   winner, player1_serve, player1_game, player2_game,
                   player1_game_acc_set, player2_game_acc_set,
                   player1_game_acc_total, player2_game_acc_total,
                   total_game_acc, leading_set)

        set_acc <- df %>%                           # identify number of sets won by each player
            group_by(set_num) %>%
            filter(set_num != 0) %>%
            summarize(player1_game_acc_set = max(player1_game_acc_set),
                      player2_game_acc_set = max(player2_game_acc_set)) %>%
            mutate(winner_1 = case_when(player1_game_acc_set > player2_game_acc_set ~ 1,
                                        player2_game_acc_set > player1_game_acc_set ~ 0)) %>%
            mutate(winner_2 = case_when(player1_game_acc_set > player2_game_acc_set ~ 0,
                                        player2_game_acc_set > player1_game_acc_set ~ 1)) %>%
            mutate(player1_set_acc = cumsum(winner_1),
                   player2_set_acc = cumsum(winner_2))

        set1_score <- filter(set_acc, set_num == "set1") %>%
            select(player1_set_acc, player2_set_acc)
        set2_score <- filter(set_acc, set_num == "set2") %>%
            select(player1_set_acc, player2_set_acc)
        set3_score <- filter(set_acc, set_num == "set3") %>%
            select(player1_set_acc, player2_set_acc)
        set4_score <- filter(set_acc, set_num == "set4") %>%
            select(player1_set_acc, player2_set_acc)
        set5_score <- filter(set_acc, set_num == "set5") %>%
            select(player1_set_acc, player2_set_acc)

        df <- df %>%                                # set level score at the moment of the game
            mutate(player1_set_acc = case_when(set_num == "set1" ~ 0,
                                               set_num == "set2" ~ set1_score$player1_set_acc,
                                               set_num == "set3" ~ set2_score$player1_set_acc,
                                               set_num == "set4" ~ set3_score$player1_set_acc,
                                               set_num == "set5" ~ set4_score$player1_set_acc)) %>%
            mutate(player2_set_acc = case_when(set_num == "set1" ~ 0,
                                               set_num == "set2" ~ set1_score$player2_set_acc,
                                               set_num == "set3" ~ set2_score$player2_set_acc,
                                               set_num == "set4" ~ set3_score$player2_set_acc,
                                               set_num == "set5" ~ set4_score$player2_set_acc))
        df <- rbind(predecessor, df)                # add to dataframe of already-cleaned games
        predecessor <- df
    }
    return(predecessor)
}



### Web Scraping Auxiliary Functions -------------------------------------------

#' Replaces spaces in names with "+"
#'
#' \code{get_plus_name} takes a full name in the form of a string and converts
#' it into a string that has "+" instead of spaces separating the name. The
#' output is meant to be pasted into a google search URL.
#'
#' @param x A column of names as strings (note: not factors).
#' @return Full name separated by "+" instead of " ".
#' @family web scraping functions
get_plus_name <- function(x){
    # Replace spaces in names with "+"
    #
    # Args:
    #   a column of names as strings (note: not factors)
    #
    # Returns:
    #   a column of names whose spaces are replaced with "+"
    #
    # Example: "Jane Doe" ----> "Jane+Doe"
    new <- unlist(strsplit(x, " ", fixed = TRUE))
    name <- paste(new, sep = "", collapse = "+")
    return(name)
}


#' Retrieves ATP Code for each player
#'
#' ATP has a player-specific code. This function retrieves that code based on a
#' google search of the player's name and basic search parameters to find the
#' ATP url where the code is located.
#'
#' @param x A column of google search URLs in the form of a string for each player.
#' @return ATP code for each player.
#' @family web scraping functions
get_ATP_code <- function(x){
    # Retrieves ATP Code of each player
    #
    # Args:
    #   A column of google search urls for each player
    #
    # Returns:
    #   ATP Code for each player
    #
    pg <- read_html(x)
    actual <- pg %>%
        html_nodes(xpath='//h3/a') %>%
        html_attr('href') %>%
        .[grepl("atpworldtour.com", .)] %>%
        .[grepl("overview", .)]
    if (identical(actual, character(0))){
        actual <- "no_code"
    } else {
        actual <- strsplit(actual, "/")[[1]] %>%
            .[length(.) - 1]
    }
    return(actual)
}


#' Create ATP URL from ATP Code
#'
#' The function generates the URL that links a player's name to his player stats profile on the ATP website.
#'
#' @param x ATP code as a string
#' @return The URL of the player stats on the ATP website
#' @family web scraping functions
get_ATP_url <- function(x){
    # create ATP url from ATP code
    #
    # Args:
    #   ATP code for each player i.e. output of get_ATP_code()
    #
    # Returns:
    #   url of players' statistics based on ATP code unique to each player
    #
    if (x != "no_code"){
        url <- paste("https://www.atpworldtour.com/en/players/roger-federer/",
                     x,
                     "/player-stats", sep = "")
    } else {
        url <- "no_url"
    }
    return(url)
}

#' Cleans data extracted from ATP player stats website
#'
#' After having retrieved data from the ATP website, this function cleans the
#' player stats in a systematic way given that all the information has been
#' found and is in the correct format.
#'
#' @param x Data from ATP site
#' @return Cleans the data to have the following columns:
#'   \itemize{
#'     \item \code{name}: First and Last Name
#'     \item \code{rank}: Rank
#'     \item \code{age}: Age and Birth Date
#'     \item \code{pro_start}: Pro_start
#'     \item \code{weight}: Weight
#'     \item \code{height}: Height
#'     \item \code{residence}: Residence
#'     \item \code{hand}: Hand
#'     \item \code{coach}: Coach
#'     \item \code{aces}: Aces
#'     \item \code{df}: Double Faults
#'     \item \code{first_serve}: 1st Serve
#'     \item \code{first_serve_won}: 1st Serve Points Won
#'     \item \code{second_serve_won}: 2nd Serve Points Won
#'     \item \code{bp_faced}: Break Points Faced
#'     \item \code{bp_saved}: Break Points Saved
#'     \item \code{serv_game_played}: Service Games Played
#'     \item \code{serv_game_won}: Service Games Won
#'     \item \code{total_serv_won}: Total Service Points Won
#'     \item \code{first_return}: 1st Serve Return Points Won
#'     \item \code{second_return}: 2nd Serve Return Points Won
#'     \item \code{bp_opp}: Break Points Opportunities
#'     \item \code{bp_conv}: Break Points Converted
#'     \item \code{ret_game_played}: Return Games Played
#'     \item \code{ret_game_won}: Return Games Won
#'     \item \code{ret_won}: Return Points Won
#'     \item \code{total_ret_won}: Total Points Won
#'
#'   }
#' @family web scraping functions
clean_data <- function(x){
    # Takes data from ATP player stats website and cleans it up
    #
    # Args:
    #   data from ATP player stats website
    #
    # Returns:
    #   cleaned up version of player stats
    #       - name: first and last Name
    #       - rank: rank of player
    #       - age: age and birth date of player
    #       - pro_start: start of professional tennis career
    #       - weight: weight in pounds and kilograms
    #       - height: height in feet/inches and centimeters
    #       - residence: location of players' home
    #       - hand: dominant hand, type of backhand
    #       - coach: name of coach
    #       - aces: number of aces
    #       - df: number of double faults
    #       - first_serve: 1st serve percentage
    #       - first_serve_won: 1st serve points won
    #       - second_serve_won: 2nd serve points won
    #       - bp_faced: break points faced
    #       - bp_saved: break points saved
    #       - serv_game_played: service games played
    #       - serv_game_won: service games won
    #       - total_serv_won: total service points won
    #       - first_return: 1st serve return points won
    #       - second_return: 2nd serve return points won
    #       - bp_opp: break points opportunities
    #       - bp_conv = break points converted
    #       - ret_game_played = return games played
    #       - ret_game_won = return games won
    #       - ret_won = return points won
    #       - total_ret_won = total points won
    #
    Name = paste(x[1,1], x[2,1], sep = " ")
    Rank = paste(x[3,1])
    if (nrow(x) == 47){ # given enough data...
        Age = x[4,1]
        Pro_start = x[5,1]
        Weight = x[6,1]
        Height = x[7,1]
        Residence = x[9,1]
        Hand = x[10,1]
        Coach = x[11,1]
        for (i in 5:22){
            title_ind <- 2*i + 2
            assign(paste0(str_trim(x[title_ind, 1])), x[title_ind+1, 1])
        }
        stat <- data.frame(name = Name,
                           rank = Rank,
                           age = Age, pro_start = Pro_start, weight = Weight,
                           height = Height, residence = Residence, hand = Hand,
                           coach = Coach,
                           aces = Aces,
                           df = `Double Faults`,
                           first_serve = `1st Serve`,
                           first_serve_won = `1st Serve Points Won`,
                           second_serve_won = `2nd Serve Points Won`,
                           bp_faced = `Break Points Faced`,
                           bp_saved = `Break Points Saved`,
                           serv_game_played = `Service Games Played`,
                           serv_game_won = `Service Games Won`,
                           total_serv_won = `Total Service Points Won`,
                           first_return = `1st Serve Return Points Won`,
                           second_return = `2nd Serve Return Points Won`,
                           bp_opp = `Break Points Opportunities`,
                           bp_conv = `Break Points Converted`,
                           ret_game_played = `Return Games Played`,
                           ret_game_won = `Return Games Won`,
                           ret_won = `Return Points Won`,
                           total_ret_won = `Total Points Won`)
        return(stat)
    } else{
        data.frame(name = Name,
                   rank = Rank,
                   age = "unknown", pro_start = "unknown", weight = "unknown",
                   height = "unknown", residence = "unknown", hand = "unknown",
                   coach = "unknown",
                   aces = "unknown",
                   df = "unknown",
                   first_serve = "unknown",
                   first_serve_won = "unknown",
                   second_serve_won = "unknown",
                   bp_faced = "unknown",
                   bp_saved = "unknown",
                   serv_game_played = "unknown",
                   serv_game_won = "unknown",
                   total_serv_won = "unknown",
                   first_return = "unknown",
                   second_return = "unknown",
                   bp_opp = "unknown",
                   bp_conv = "unknown",
                   ret_game_played = "unknown",
                   ret_game_won = "unknown",
                   ret_won = "unknown",
                   total_ret_won = "unknown")
    }

}

### Web-Scraping Functions -----------------------------------------------------

#' Creates a dataframe of ATP urls and ATP codes given the names of players
#'
#' Given the names of the players, this function creates the Google URLS,
#' identifies the ATP codes, and generates the ATP URLs.
#'
#' @param x Dataframe of players we are interested in.
#' @return Dataframe with the \code{google_url}, \code{get_ATP_code}, and
#'   \code{get_ATP_url} for every player
#' @family web scraping functions
manipulate_data <- function(x){
    # creates a dataframe of ATP urls and ATP codes given the names of players
    #
    # Args:
    #   Names of players we are interested in
    #
    # Returns:
    #   a dataframe with:
    #       google_url, get_ATP_code, and get_ATP_url associated with each player
    #
    output <- x %>%
        mutate(plus_names = map_chr(players, get_plus_name)) %>%
        mutate(google_url = paste("https://www.google.com/search?hl=en&q=overview+",
                                  plus_names,"+ATP&btnG=Google+Search", sep = "")) %>%
        mutate(ATP_code = map_chr(google_url, get_ATP_code)) %>%
        mutate(ATP_url = map_chr(ATP_code, get_ATP_url))

    return(output)
}

#' Extracts and cleans data from the ATP URLs
#'
#' With the ATP URLs, this function retreives the desired information. If there
#' is no incomplete information, the data cleaning procedure will return a
#' dataset with the players' statistics. If there is incomplete informaiton on
#' the website, the players' statistics will be reported as "unknown."
#'
#' @param x A vector of ATP URLs
#' @return A dataframe whose rows are players and whose columns are the player
#'   statistics of interest
#' @family web scraping functions
extract_data <- function(x){
    # Takes ATP urls and then extracts and clean data from ATP website
    #
    # Args:
    #   vector of ATP player_stats urls i.e. output of get_ATP_url()
    #
    # Returns:
    #   player statistics, cleaned up
    #
    tryCatch(
        dt <- x %>%
            read_html() %>%
            html_nodes(., ".data-number , .first-name, .last-name , td") %>%
            html_text() %>%
            data.frame() %>%
            clean_data(),
        error = function(e){data.frame(name = "unknown",
                                       rank = "unknown",
                                       age = "unknown", pro_start = "unknown", weight = "unknown",
                                       height = "unknown", residence = "unknown", hand = "unknown",
                                       coach = "unknown",
                                       aces = "unknown",
                                       df = "unknown",
                                       first_serve = "unknown",
                                       first_serve_won = "unknown",
                                       second_serve_won = "unknown",
                                       bp_faced = "unknown",
                                       bp_saved = "unknown",
                                       serv_game_played = "unknown",
                                       serv_game_won = "unknown",
                                       total_serv_won = "unknown",
                                       first_return = "unknown",
                                       second_return = "unknown",
                                       bp_opp = "unknown",
                                       bp_conv = "unknown",
                                       ret_game_played = "unknown",
                                       ret_game_won = "unknown",
                                       ret_won = "unknown",
                                       total_ret_won = "unknown")}
    )
}
