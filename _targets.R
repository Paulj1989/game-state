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
    format = "file"
  ),
  tar_target(leads_raw,
    "data/buli_leads.csv",
    format = "file"
  ),
  tar_target(deficits_raw,
    "data/buli_deficits.csv",
    format = "file"
  ),

  # transform data
  tar_target(leads, transform_leads_data(leads_raw)),
  tar_target(deficits, transform_deficits_data(deficits_raw)),
  tar_target(
    bvb_game_state,
    transform_game_state_data(teams_raw, team = "Borussia Dortmund")
  ),
  tar_target(buli_game_state, transform_game_state_data(teams_raw)),
  tar_target(timings, transform_timing_data(teams_raw)),

  # plot figures
  tar_target(bvb_stats, plot_bvb_stats(bvb_game_state), format = "file"),
  tar_target(
    shots_comparison,
    plot_buli_stats(
      buli_game_state, shot_diff,
      subtitle = glue::glue(
        "<b style='color:#FFA600;'>Borussia Dortmund</b> and ",
        "<b style='color:#DB4254;'>Bayern Munich</b>'s shot difference, ",
        "across game states, is generally much better than league average. ",
        "However, BVB's shot difference has declined in recent years, even ",
        "hovering around<br>zero when winning. Bayern have consistently ",
        "outperformed BVB in this area, but there are some signs that the gap ",
        "is narrowing, despite Bayern's performances while losing last season ",
        "masking the trend a little."
      )
    ),
    format = "file"
  ),
  tar_target(
    goals_comparison,
    plot_buli_stats(
      buli_game_state, goal_diff,
      subtitle = glue::glue(
        "<b style='color:#FFA600;'>Borussia Dortmund</b>'s goal difference ",
        "varies a lot across both game states and seasons. Last season, BVB's ",
        "goal difference improved to ~1.2 goals when winning and ~1.5 when ",
        "drawing (slightly<br>lower than <b style='color:#DB4254;'>Bayern ",
        "Munich</b>'s ~1.6), but it was exactly zero when losing. Generally, ",
        "Bayern have outperformed BVB despite a steady decline in ",
        "performance, when winning, from ~3 goals to ~1.5."
      )
    ),
    format = "file"
  ),
  tar_target(
    xg_comparison,
    plot_buli_stats(
      buli_game_state, xG_diff,
      subtitle = glue::glue(
        "Comparing <b style='color:#FFA600;'>Borussia Dortmund</b>'s xG ",
        "performance across game states with <b style='color:#DB4254;'>",
        "Bayern Munich</b>'s highlights that BVB's biggest issue has been ",
        "maintaining their level when winning. Their performance is comparable",
        "<br>with Bayern's when drawing, and the gap has narrowed when losing, ",
        "but BVB's xG difference when winning has been closer to league average ",
        "than to Bayern. However, they did close that gap a lot last season."
      )
    ),
    format = "file"
  ),
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
    plot_timing_splits(
      timings, shot_diff,
      subtitle = glue::glue(
        "When splitting <b style='color:#FFA600;'>Borussia Dortmund</b> and ",
        "<b style='color:#DB4254;'>Bayern Munich</b>'s shot differences by 15 ",
        "minute periods in games, both look relatively consistent. There is ",
        "minimal variance within games or between seasons, with<br>the ",
        "exception of the final 15 minutes of games, where both teams perform ",
        "a little better. Bayern's shot difference is consistently better than ",
        "BVB's at all points in games, in all but a small handful of cases."
      )
    ),
    format = "file"
  ),
  tar_target(
    goal_timing_comparison,
    plot_timing_splits(
      timings, goal_diff,
      subtitle = glue::glue(
        "Splitting <b style='color:#FFA600;'>Borussia Dortmund</b>'s goal ",
        "difference by 15 minute periods in games, across seasons, points to ",
        "variance in performance between halves. BVB are better in the second ",
        "half, particularly in the final<br>30 minutes. <b style='color:#DB4254;'>",
        "Bayern Munich</b>'s performances are more consistent over the course ",
        "of games, however they were more inconsistent last season, ",
        "performing well in the first half and struggling late in games."
      )
    ),
    format = "file"
  ),
  tar_target(
    xg_timing_comparison,
    plot_timing_splits(
      timings, xG_diff,
      subtitle = glue::glue(
        "<b style='color:#FFA600;'>Borussia Dortmund</b> & <b style='color:",
        "#DB4254;'>Bayern Munich</b>'s xG difference over the course of games ",
        "is relatively consistent. Bayern's xG difference is generally higher ",
        "than BVB's, but the gap between the two narrowed last season,<br>and ",
        "BVB's xG difference was actually higher than Bayern's in the final 15 ",
        "minutes of games last season. However, Dortmund's performances in the ",
        "final 15 minutes of games varies quite a bit between seasons."
      )
    ),
    format = "file"
  )
)
