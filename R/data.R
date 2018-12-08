#' Point-by-point records of tennis matches
#'
#' A dataset made from combining `pbp_matches_atp_main_current.csv` and
#' `pbp_matches_atp_main_archive` by Jeff Sackmann. Contains point-by-point
#' records of ATP tennis matches from 2011 to 2017.
#'
#' @format A data frame with 13050 rows and 12 variables:
#' \describe{
#'   \item{pbp_id}{unique identifier of a tennis match}
#'   \item{date}{date of match, format: DD MON YY}
#'   \item{tny_name}{tournament name}
#'   \item{tour}{name of tour, i.e., ATP}
#'   \item{draw}{draw, i.e., Main}
#'   \item{server1}{name of player who served first, often referred to as player1}
#'   \item{server2}{name of opponent of \code{server1}, often referred to as player2}
#'   \item{winner}{winner of match, i.e., 1 or 2, corresponding to one of the previous two columns}
#'   \item{pbp}{point-by-point record; S (server won), R (returner won), A (ace), D (double fault); Sets are delimited with the '.' character, games are delimited with the ';' character, and the '/' character indicates changes of serve during a tiebreak}
#'   \item{score}{final score of match}
#'   \item{adf_flag}{1 if the point sequence notes any aces or double faults, 0 if not}
#'   \item{wh_minutes}{...}
#' }
#'
#' @source \url{https://github.com/JeffSackmann/tennis_pointbypoint}
#' @family Sackmann original files
#' @seealso \code{\link{pbp_raw_clean.df}} for cleaned version
"pbp_raw.df"

#' Cleaned version of Sackmann's \code{pbp_raw.df}
#'
#' A game-wise manipulation of Sackmann's pbp_raw.df
#'
#' @format A data frame with 364797 rows and 19 variables:
#' \describe{
#'     \item{pbp_id}{identification corresponding to that in the original dataset}
#'     \item{match_num}{counter of the matches cleaned}
#'     \item{set_num}{identifies the set in which the game is played}
#'     \item{game_num}{counter of the games in each set (resets to 1 after a new set)}
#'     \item{pbp}{point-by-point record of the game; S (server won), R (returner won), A (ace), D (double fault)}
#'     \item{server}{points won by the server of the game}
#'     \item{returner}{points won by the returner of the game}
#'     \item{winner}{winner of the game i.e. server or returner}
#'     \item{player1_serve}{1 if player1 is server, 0 otherwise}
#'     \item{player1_game}{1 if player1 wins game, 0 otherwise}
#'     \item{player2_game}{1 if player2 wins game, 0 otherwise}
#'     \item{player1_game_acc_set}{running total of number of games won by player1 in a given set}
#'     \item{player2_game_acc_set}{running total of number of games won by player2 in a given set}
#'     \item{player1_game_acc_total}{running total of number of games won by player1 in a given match}
#'     \item{player2_game_acc_total}{running total of number of games won by player2 in a given match}
#'     \item{total_game_acc}{running total of number of games in a given match}
#'     \item{leading_set}{denotes which player has won more games in a given set}
#'     \item{player1_set_acc}{number of sets player1 wins}
#'     \item{player2_set_acc}{number of sets player2 wins}
#' }
#'
#' @family Sackmann manipulated files
#' @seealso \code{\link{pbp_raw.df}} for original file, \code{\link{data_clean_tot}} for cleaning function
"pbp_raw_clean.df"

#' Match-wise pairing of \code{server1} and \code{server2}
#'
#' A data frame that shows who the players are for every match that has a set-wise game deficit of at least 4
#'
#' @format A data frame with 7194 rows and 3 variables:
#' \describe{
#'   \item{pbp_id4}{pbp_id for matches in which there is a set-wise difference of at least 4 games between the lagging and leading player}
#'   \item{server1}{}
#'   \item{server2}{}
#' }
#'
#' @family Sackmann manipulated files, player stats
"players"


#' Players' Statistics
#'
#' Web-scraped statistics from ATP website re: players in matches that have a set-wise game deficit of at least 4
#'
#' @format A data frame of 887 rows and 27 variables
#' \describe{
#'   \item{name}{First and Last Name}
#'   \item{rank}{Rank}
#'   \item{age}{Age and Birth Date}
#'   \item{pro_start}{Pro_start}
#'   \item{weight}{Weight}
#'   \item{height}{Height}
#'   \item{residence}{Residence}
#'   \item{hand}{Hand}
#'   \item{coach}{Coach}
#'   \item{aces}{Aces}
#'   \item{df}{Double Faults}
#'   \item{first_serve}{1st Serve}
#'   \item{first_serve_won}{1st Serve Points Won}
#'   \item{second_serve_won}{2nd Serve Points Won}
#'   \item{bp_faced}{Break Points Faced}
#'   \item{bp_saved}{Break Points Saved}
#'   \item{serv_game_played}{Service Games Played}
#'   \item{serv_game_won}{Service Games Won}
#'   \item{total_serv_won}{Total Service Points Won}
#'   \item{first_return}{1st Serve Return Points Won}
#'   \item{second_return}{2nd Serve Return Points Won}
#'   \item{bp_opp}{Break Points Opportunities}
#'   \item{bp_conv}{Break Points Converted}
#'   \item{ret_game_played}{Return Games Played}
#'   \item{ret_game_won}{Return Games Won}
#'   \item{ret_won}{Return Points Won}
#'   \item{total_ret_won}{Total Points Won}
#' }
#'
#' @family player stats
#' @source \url{http://atpworldtour.com}
"players_stats"

#' Cleaned game-wise dataset with player statistics
#'
#' A combination of player statistics and game-wise pbp data in which there is a lagging and a leading player
#'
#' @format A data frame with 17680 rows and 81 variables. Variables that are character specific have been omitted (e.g., ace_lagging and ace_leading have the same description, but refer to different players hence only ace_lagging appears in the list below):
#' \describe{
#'   \item{pbp_id}{pbp_id for matches in which there is a set-wise difference of at least 4 games between the lagging and leading player}
#'   \item{match_num}{counter of the matches cleaned from \code{pbp_raw_clean.df}}
#'   \item{set_num}{identifies the set in which the game is played}
#'   \item{game_num}{counter of the games in each set (resets to 1 after a new set)}
#'   \item{pbp}{point-by-point record of the game; S (server won), R (returner won), A (ace), D (double fault)}
#'   \item{server}{points won by the server of the game}
#'   \item{returner}{points won by the returner of the game}
#'   \item{winner}{winner of the game i.e. server or returner}
#'   \item{player1_serve}{1 if player1 is server, 0 otherwise}
#'   \item{player1_game}{1 if player1 wins game, 0 otherwise}
#'   \item{player2_game}{1 if player2 wins game, 0 otherwise}
#'   \item{player1_game_acc_set}{running total of number of games won by player1 in a given set}
#'   \item{player2_game_acc_set}{running total of number of games won by player2 in a given set}
#'   \item{player1_game_acc_total}{running total of number of games won by player1 in a given match}
#'   \item{player2_game_acc_total}{running total of number of games won by player2 in a given match}
#'   \item{total_game_acc}{running total of number of games in a given match}
#'   \item{leading_set}{denotes which player has won more games in a given set}
#'   \item{player1_set_acc}{number of sets player1 wins}
#'   \item{player2_set_acc}{number of sets player2 wins}
#'   \item{diff_game_set}{set-wise game deficit; by construction, must be of magnitude at least four}
#'   \item{match_winner}{winner of the match, i.e., 1 or 2}
#'   \item{player1_leading_set}{1 if player1 has more games than player2 in a given set}
#'   \item{rally_length}{length of rally, i.e., nchar(pbp)}
#'   \item{server1}{Name of server 1}
#'   \item{server2}{Name of server 2}
#'   \item{rank_lagging}{Rank}
#'   \item{age_lagging}{Age and Birth Date}
#'   \item{pro_start_lagging}{Pro_start}
#'   \item{weight_lagging}{Weight}
#'   \item{height_lagging}{Height}
#'   \item{residence_lagging}{Residence}
#'   \item{hand_lagging}{Hand}
#'   \item{coach_lagging}{Coach}
#'   \item{aces_lagging}{Aces}
#'   \item{df_lagging}{Double Faults}
#'   \item{first_serve_lagging}{1st Serve}
#'   \item{first_serve_won_lagging}{1st Serve Points Won}
#'   \item{second_serve_won_lagging}{2nd Serve Points Won}
#'   \item{bp_faced_lagging}{Break Points Faced}
#'   \item{bp_saved_lagging}{Break Points Saved}
#'   \item{serv_game_played_lagging}{Service Games Played}
#'   \item{serv_game_won_lagging}{Service Games Won}
#'   \item{total_serv_won_lagging}{Total Service Points Won}
#'   \item{first_return_lagging}{1st Serve Return Points Won}
#'   \item{second_return_lagging}{2nd Serve Return Points Won}
#'   \item{bp_opp_lagging}{Break Points Opportunities}
#'   \item{bp_conv_lagging}{Break Points Converted}
#'   \item{ret_game_played_lagging}{Return Games Played}
#'   \item{ret_game_won_lagging}{Return Games Won}
#'   \item{ret_won_lagging}{Return Points Won}
#'   \item{total_ret_won_lagging}{Total Points Won}
#' }
#'
#' @family final files
#' @seealso \code{\link{tennis_cleaned}} for a player1/player2-agnostic dataset
"tennis"

#' Cleaned version of \code{tennis}
#'
#' This dataset has lagging-leading differences in player statistics and renames descriptions of the game in terms of lagging and leading player as opposed to player1 and player2.
#'
#' @format A data frame with 17680 rows and 107 variables; only variables not in \code{tennis} are explained below. Note that differences were calculated by subtracting leading numbers from lagging numbers.
#' \describe{
#'   \item{see \code{tennis}}{}
#'   \item{lagging_serve}{1 if lagging player serves, 0 otherwise}
#'   \item{leading_serve}{1 if leading player serves, 0 otherwise}
#'   \item{lagging_points}{points scored by lagging player in the game}
#'   \item{leading_points}{points scored by leading player in the game}
#'   \item{lagging_game}{1 if lagging player wins game, 0 otherwise}
#'   \item{leading_game}{1 if leading player wins game, 0 otherwise}
#'   \item{lagging_game_acc_set}{number of games won by the lagging player in the given set thus far}
#'   \item{leading_game_acc_set}{number of games won by the leading player in the given set thus far}
#'   \item{lagging_game_acc_tot}{number of games won by the lagging player in entire match thus far}
#'   \item{leading_game_acc_tot}{number of games won by the leading player in entire match thus far}
#'   \item{lagging_set}{1 if lagging player wins set, 0 otherwise}
#'   \item{leading_set}{1 if leading player wins set, 0 otherwise}
#'   \item{lagging_set_acc}{number of sets won by the lagging player thus far}
#'   \item{leading_set_acc}{number of sets won by the leading player thus far}
#'   \item{lagging_match_win}{1 if lagging player wins match}
#'   \item{leading_match_win}{1 if leading player wins match}
#'   \item{diff_game_tot}{match-wise game deficit}
#'   \item{diff_set}{match-wise set deficit}
#'   \item{lagging_hand}{1 if lagging player is right-hand dominant, 0 otherwise}
#'   \item{leading_hand}{1 if leading player is right-hand dominant, 0 otherwise}
#'   \item{diff_hand}{1 if players are dominant in different hands, 0 otherwise}
#'   \item{diff_rank}{difference in the players' ranks}
#'   \item{diff_serve_won}{difference in the service games won}
#'   \item{diff_ret_won}{difference in return games won}
#'   \item{lagging_age}{age of lagging player}
#'   \item{leading_age}{age of leading player}
#'   \item{diff_age}{difference in the players' ages}
#' }
#'
#' @family final files
#' @seealso \code{\link{tennis_cleaned}} for a player1/player2-defined dataset; \code{\link{tennis_data}} for child set
"tennis_cleaned"

#' Organized dataset with STRATEGIC measurement
#'
#' Organized and abridged version of \code{tennis_cleaned} with STRATEGIC
#'
#' @format A data frame with 17571 rows (removing tiebreakers) and 37 variables:
#' \describe{
#'   \item{pbp_id}{point-by-point record of the game}
#'   \item{match_num}{counter of the matches cleaned from \code{pbp_raw_clean.df}}
#'   \item{set_num}{identifies the set in which the game is played}
#'   \item{game_num}{counter of the games in each set (resets to 1 after a new set)}
#'   \item{pbp}{point-by-point record of the game; S (server won), R (returner won), A (ace), D (double fault)}
#'   \item{rally_length}{length of rally, i.e., nchar(pbp)}
#'   \item{lagging_player}{denotes which of the two players is lagging, player1 or player2}
#'   \item{leading_player}{denotes which of the two players is lagging, player1 or player2}
#'   \item{lagging_player_name}{name of lagging player}
#'   \item{leading_player_name}{name of leading player}
#'   \item{lagging_serve}{1 if lagging player serves, 0 otherwise}
#'   \item{leading_serve}{1 if leading player serves, 0 otherwise}
#'   \item{lagging_points}{points scored by lagging player in the game}
#'   \item{leading_points}{points scored by leading player in the game}
#'   \item{lagging_game}{1 if lagging player wins game, 0 otherwise}
#'   \item{leading_game}{1 if leading player wins game, 0 otherwise}
#'   \item{diff_game_set}{set-wise game deficit; by construction, must be of magnitude at least four}
#'   \item{lagging_game_acc_set}{number of games won by the lagging player in the given set thus far}
#'   \item{leading_game_acc_set}{number of games won by the leading player in the given set thus far}
#'   \item{lagging_game_acc_tot}{number of games won by the lagging player in entire match thus far}
#'   \item{leading_game_acc_tot}{number of games won by the leading player in entire match thus far}
#'   \item{lagging_set}{1 if lagging player wins set, 0 otherwise}
#'   \item{leading_set}{1 if leading player wins set, 0 otherwise}
#'   \item{lagging_set_acc}{number of sets won by the lagging player thus far}
#'   \item{leading_set_acc}{number of sets won by the leading player thus far}
#'   \item{lagging_match_win}{1 if lagging player wins match}
#'   \item{leading_match_win}{1 if leading player wins match}
#'   \item{diff_game_tot}{match-wise game deficit}
#'   \item{diff_set}{match-wise set deficit}
#'   \item{lagging_hand}{1 if lagging player is right-hand dominant, 0 otherwise}
#'   \item{leading_hand}{1 if leading player is right-hand dominant, 0 otherwise}
#'   \item{diff_hand}{1 if players are dominant in different hands, 0 otherwise}
#'   \item{diff_rank}{difference in the players' ranks}
#'   \item{diff_serve_won}{difference in the service games won}
#'   \item{diff_ret_won}{difference in return games won}
#'   \item{diff_age}{difference in the players' ages}
#'   \item{STRATEGIC}{1 if lagging player should exert full effort, 0 otherwise; 1 corresponds to when the leading player has won two sets already, 0 otherwise}
#' }
#'
#' @family final files
#' @seealso \code{\link{tennis_cleaned}} for parent set
"tennis_data"
