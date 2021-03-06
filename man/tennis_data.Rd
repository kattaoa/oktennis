% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{tennis_data}
\alias{tennis_data}
\title{Organized dataset with STRATEGIC measurement}
\format{A data frame with 17571 rows (removing tiebreakers) and 37 variables:
\describe{
  \item{pbp_id}{point-by-point record of the game}
  \item{match_num}
  \item{set_num}
  \item{game_num}{counter of the games in each set (resets to 1 after a new set)}
  \item{pbp}{point-by-point record of the game; S (server won), R (returner won), A (ace), D (double fault)}
  \item{rally_length}{length of rally, i.e., nchar(pbp)}
  \item{lagging_player}{denotes which of the two players is lagging, player1 or player2}
  \item{leading_player}{denotes which of the two players is lagging, player1 or player2}
  \item{lagging_player_name}{name of lagging player}
  \item{leading_player_name}{name of leading player}
  \item{lagging_serve}{1 if lagging player serves, 0 otherwise}
  \item{leading_serve}{1 if leading player serves, 0 otherwise}
  \item{lagging_points}{points scored by lagging player in the game}
  \item{leading_points}{points scored by leading player in the game}
  \item{lagging_game}{1 if lagging player wins game, 0 otherwise}
  \item{leading_game}{1 if leading player wins game, 0 otherwise}
  \item{diff_game_set}{set-wise game deficit; by construction, must be of magnitude at least four}
  \item{lagging_game_acc_set}{number of games won by the lagging player in the given set thus far}
  \item{leading_game_acc_set}{number of games won by the leading player in the given set thus far}
  \item{lagging_game_acc_tot}{number of games won by the lagging player in entire match thus far}
  \item{leading_game_acc_tot}{number of games won by the leading player in entire match thus far}
  \item{lagging_set}{1 if lagging player wins set, 0 otherwise}
  \item{leading_set}{1 if leading player wins set, 0 otherwise}
  \item{lagging_set_acc}{number of sets won by the lagging player thus far}
  \item{leading_set_acc}{number of sets won by the leading player thus far}
  \item{lagging_match_win}{1 if lagging player wins match}
  \item{leading_match_win}{1 if leading player wins match}
  \item{diff_game_tot}{match-wise game deficit}
  \item{diff_set}{match-wise set deficit}
  \item{lagging_hand}{1 if lagging player is right-hand dominant, 0 otherwise}
  \item{leading_hand}{1 if leading player is right-hand dominant, 0 otherwise}
  \item{diff_hand}{1 if players are dominant in different hands, 0 otherwise}
  \item{diff_rank}{difference in the players' ranks}
  \item{diff_serve_won}{difference in the service games won}
  \item{diff_ret_won}{difference in return games won}
  \item{diff_age}{difference in the players' ages}
  \item{STRATEGIC}{1 if lagging player should exert full effort, 0 otherwise; 1 corresponds to when the leading player has won two sets already, 0 otherwise}
}}
\usage{
tennis_data
}
\description{
Organized and abridged version of \code{tennis_cleaned} with STRATEGIC
}
\seealso{
\code{\link{tennis_cleaned}} for parent set

Other final files: \code{\link{tennis_cleaned}},
  \code{\link{tennis}}
}
\concept{final files}
\keyword{datasets}
