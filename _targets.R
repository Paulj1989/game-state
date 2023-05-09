
# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
tar_option_set(
  packages = c("dplyr", "stringr", "ggplot2", "glue", "patchwork"),
  format = "rds" # default storage format
  # Set other options as needed.
)

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multicore")

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to 
# allow use_targets() to configure tar_make_future() options.

# Run the R scripts in the R/ folder with your custom functions:
tar_source("R/functions.R")
# source("other_functions.R") # Source other scripts as needed. # nolint

# Replace the target list below with your own:
list(
  
  # raw data files
  tar_target(teams_raw,
             "data/buli_team_stats.csv",
             format = "file"),
  tar_target(leads_raw,
             "data/buli_leads.csv",
             format = "file"),
  tar_target(deficits_raw,
             "data/buli_deficits.csv",
             format = "file"),
  
  # transform data
  tar_target(leads, transform_leads_data(leads_raw)),
  tar_target(deficits, transform_deficits_data(deficits_raw)),
  tar_target(bvb_game_state,
             transform_game_state_data(teams_raw, team = "Borussia Dortmund")),
  tar_target(buli_game_state, transform_game_state_data(teams_raw)),
  tar_target(timings, transform_timing_data(teams_raw)),
  
  # plot figures
  tar_target(bvb_stats, plot_bvb_stats(bvb_game_state), format = "file"),
  tar_target(
    shots_comparison,
    plot_buli_stats(
      buli_game_state, shot_diff,
      subtitle = glue::glue(
        "<b style='color:#ffa600;'>Borussia Dortmund</b> have clearly been ",
        "taking fewer shots, across the board, over the seasons. What is ",
        "particularly concerning is that the short difference /90 has dropped ",
        "to around 0 when winning and losing.<br>BVB are not dominating games ",
        "when winning or losing, and their performances in either game state ",
        "look to be weaker than when everything is level. In fact, BVB's shot ",
        "difference is barely better than league<br>average when they are ",
        "losing. <b style='color:#DB4254;'>Bayern Munich</b>, however, get an ",
        "astronomical number of shots when they are losing. Given their ",
        "struggles this season, it's not entirely sure how good or bad this ",
        "actually is!"
      )
    ),
    format = "file"),
  tar_target(
    goals_comparison,
    plot_buli_stats(
      buli_game_state, goal_diff,
      subtitle = glue::glue(
        "<b style='color:#ffa600;'>Borussia Dortmund</b>'s goal difference ",
        "across game states has stayed relatively stable. It has improved a ",
        "little when drawing, and dipped a little when losing, but on the ",
        "whole BVB have been consistent.<br>What is notable, however, is that ",
        "when winning and losing this season, BVB are averaging less than a ",
        "+1 goal difference this season. <b style='color:#DB4254;'>",
        "Bayern Munich</b>, on the other hand, have consistently outperformed",
        "<br>BVB when winning and drawing, and on average have BVB beat when ",
        "losing too, but they have been on a pretty steady decline when ",
        "winning, slipping from around +3 goals to a little less than +2."
      )
    ),
    format = "file"),
  tar_target(
    xg_comparison,
    plot_buli_stats(
      buli_game_state, xG_diff,
      subtitle = glue::glue(
        "Comparing <b style='color:#ffa600;'>Borussia Dortmund</b>'s xG ",
        "difference /90 (xG for – XG against) across game states with ",
        "<b style='color:#DB4254;'>Bayern Munich</b>'s helps illustrate ",
        "Borussia Dortmund's biggest issue. Their xG difference is comparable ",
        "with Bayern's<br>when drawing, and while there's a relatively ",
        "significant difference between the two when losing, it remains ",
        "(relatively) constant over the course of the seasons analysed. When ",
        "winning, however, Bayern's<br>xG difference has remained around +2, ",
        "while BVB's has dropped significantly. In the last three seasons, ",
        "BVB's performances when winning have been closer to the league ",
        "average than their title rival."
        )
      ),
    format = "file"),
  tar_target(
    points_comparison,
    plot_buli_points(leads),
    format = "file"
  ),
  tar_target(
    leads_comparison,
    plot_leads(leads),
    format = "file"
  ),
  tar_target(
    deficits_comparison,
    plot_deficits(deficits),
    format = "file"
  ),
  tar_target(
    comebacks_comparison,
    plot_comebacks(deficits),
    format = "file"
  ),
  tar_target(
    timing_distributions_comparison,
    plot_timing_distributions(timings),
    format = "file"
  ),
  tar_target(
    shot_timing_comparison,
    plot_timing_splits(timings, shot_diff, subtitle=NULL),
    format = "file"
  ),
  tar_target(
    goal_timing_comparison,
    plot_timing_splits(timings, goal_diff, subtitle=NULL),
    format = "file"
  ),
  tar_target(
    xg_timing_comparison,
    plot_timing_splits(timings, xG_diff, subtitle=NULL),
    format = "file"
  )
)
