# Setup functions ----

theme_ftw <-
  function(base_family = "Inter", base_size = 15,
           grid_y = TRUE, grid_x = FALSE) {
    # Use theme_minimal as basis for theme
    ggplot2::theme_minimal(
      base_size = base_size,
      base_family = base_family
    ) %+replace%
      ggplot2::theme(
        # plot elements
        plot.background = element_rect(fill = "white", color = "white"),
        plot.margin = margin(25, 45, 25, 45),
        # text elements
        plot.title = ggtext::element_markdown(
          size = rel(1.7),
          hjust = 0,
          margin = margin(b = 15)
        ),
        plot.subtitle = ggtext::element_markdown(
          colour = "gray40", size = rel(0.9),
          hjust = 0, lineheight = 1.3,
          margin = margin(b = 15)
        ),
        plot.caption = element_text(
          colour = "gray60", size = rel(.8),
          hjust = 0, margin = margin(t = 25)
        ),
        strip.text = element_text(
          size = rel(1.2),
          margin = margin(t = 10, b = 20)
        ),
        plot.title.position = "plot", plot.caption.position = "plot",
        # axis elements
        axis.title.y = element_text(
          colour = "gray20", size = rel(1.2),
          angle = 90, vjust = 5
        ),
        axis.text = element_text(size = rel(1)),
        # legend elements
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = rel(1)),
        legend.key.width = unit(2, "cm"),
        # panel elements
        panel.grid.major.x =
          if (grid_x) {
            ggplot2::element_line(
              linewidth = 0.6,
              colour = "grey80",
              linetype = "dotted"
            )
          } else {
            ggplot2::element_blank()
          },
        panel.grid.major.y =
          if (grid_y) {
            ggplot2::element_line(
              linewidth = 0.6,
              colour = "grey80",
              linetype = "dotted"
            )
          } else {
            ggplot2::element_blank()
          },
        panel.grid.minor.x = ggplot2::element_blank(),
        panel.grid.minor.y = ggplot2::element_blank(),
        panel.spacing.x = unit(3.5, "lines"),
        complete = TRUE
      )
  }

# Data functions ----

# transform_teams_data <- function(data) {
#   readr::read_csv(data) |>
#     rename(
#       team = team_name,
#       stat = stat_name,
#       season = season_start_year,
#       shots_against = against.shots,
#       goals_against = against.goals,
#       xG_against = against.xG,
#       mins = time
#     ) |>
#     mutate(
#       stat = recode(game_state,
#         "Goal diff 0" = "Drawing",
#         "Goal diff +1" = "Winning (+1)",
#         "Goal diff > +1" = "Winning (> +1)",
#         "Goal diff -1" = "Losing (-1)",
#         "Goal diff < -1" = "Losing (< -1)"
#       )
#     )
# }

transform_leads_data <-
  function(data) {
    readr::read_csv(data) |>
      mutate(
        dropped_points = (leads * 3) - points,
        win_percent = wins / leads,
        dropped_points_percent = dropped_points / (points + dropped_points),
        games = factor(games,
          levels = c("All Games", "Home Games", "Away Games")
        )
      ) |>
      arrange(desc(season), games, desc(leads))
  }

transform_deficits_data <-
  function(data) {
    readr::read_csv(data) |>
      mutate(
        win_percent = wins / deficits,
        comeback_percent = (wins + draws) / deficits,
        games = factor(games,
          levels = c("All Games", "Home Games", "Away Games")
        )
      ) |>
      arrange(desc(season), games, desc(deficits))
  }


transform_game_state_data <- function(data, team = NULL) {
  readr::read_csv(data) |>
    filter(if (is.null({{ team }})) {
      stat_group_name == "gameState"
    } else {
      team_name == {{ team }} & stat_group_name == "gameState"
    }) |>
    rename(
      team = team_name,
      game_state = stat_name,
      season = season_start_year,
      shots_against = against.shots,
      goals_against = against.goals,
      xG_against = against.xG,
      mins = time
    ) |>
    mutate(
      game_state = case_when(
        game_state %in% c("Goal diff +1", "Goal diff > +1") ~ "Winning",
        game_state %in% c("Goal diff -1", "Goal diff < -1") ~ "Losing",
        .default = "Drawing"
      ),
      game_state = factor(game_state,
        levels = c("Winning", "Drawing", "Losing")
      )
    ) |>
    summarise(across(shots:mins, ~ sum(.x)),
      .by = c(team, season, game_state)
    ) |>
    mutate(
      across(shots:xG_against, ~ .x / (mins / 90)),
      goal_diff = goals - goals_against,
      xG_diff = xG - xG_against,
      shot_diff = shots - shots_against
    )
}

transform_timing_data <-
  function(data) {
    readr::read_csv(here::here(data)) |>
      filter(stat_group_name == "timing") |>
      select(!time) |>
      rename(
        team = team_name,
        timing = stat_name,
        season = season_start_year,
        shots_against = against.shots,
        goals_against = against.goals,
        xG_against = against.xG
      ) |>
      mutate(
        goal_diff = (goals - goals_against) / 34,
        xG_diff = (xG - xG_against) / 34,
        shot_diff = (shots - shots_against) / 34,
        timing = case_when(
          timing == "1-15" ~ "1-15\nmins",
          .default = timing
        ),
        .by = c(team, season, timing)
      )
  }

# Plot Functions ----

# bvb performance

plot_bvb_stats <-
  function(data) {
    data |>
      tidyr::pivot_longer(
        cols = ends_with("diff"),
        names_to = "stat"
      ) |>
      mutate(
        stat = recode(
          stat,
          "goal_diff" = "Goal Difference /90",
          "xG_diff" = "xG Difference /90",
          "shot_diff" = "Shot Difference /90"
        ),
        stat = factor(stat, levels = c(
          "Shot Difference /90",
          "Goal Difference /90",
          "xG Difference /90"
        ))
      ) |>
      ggplot(aes(season, value, colour = game_state)) +
      geom_hline(
        yintercept = 0, colour = "grey20",
        linetype = "dashed", linewidth = 1
      ) +
      geom_smooth(
        method = lm, formula = y ~ splines::bs(x), se = FALSE,
        linewidth = 1.2, alpha = 1
      ) +
      geom_point(aes(fill = game_state),
        shape = 21, size = 7, alpha = .9,
        stroke = 1, colour = "grey20"
      ) +
      facet_wrap(vars(stat), scales = "free_y") +
      scale_fill_manual(values = c("#ffa600", "#dee2e6", "#343a40")) +
      scale_colour_manual(values = c("#ffa600", "#dee2e6", "#343a40")) +
      scale_x_continuous(breaks = c(2014:2022), labels = c(
        "2014/15", "2015/16", "2016/17", "2017/18", "2018/19",
        "2019/20", "2020/21", "2021/22", "2022/23"
      )) +
      labs(
        title = glue(
          "Borussia Dortmund's Bundesliga Performances by Game ",
          "State, 2014/15 - 2022/23"
        ),
        subtitle = glue(
          "BVB's goal and xG difference are better when they are winning or ",
          "drawing, and the former tends to be significantly worse ",
          "when losing, while their shot difference is the complete opposite. ",
          "Last season, BVB<br>improved significantly when winning or drawing, ",
          "making particularly big improvements when winning. When losing, ",
          "on the other hand, Dortmund's performances took a significant dive, across ",
          "every metric."
        ),
        caption = glue(
          "Source: Understat (via worldfootballR) | ",
          "Graphic: Paul Johnson (@paul_johnson89)"
        ),
        x = NULL, y = NULL
      ) +
      theme_ftw() +
      theme(
        axis.text.x = element_text(angle = 30, vjust = 0.5, hjust = 0.5),
        axis.text.y = element_text(size = rel(1.2))
      )

    ggsave(here::here("figures", "bvb_game_state.png"),
      dpi = 320, width = 20, height = 10
    )
  }


# compare performance (using goals, shots, and xG)
plot_buli_stats <-
  function(data, stat, subtitle) {
    stat <- enquo(stat)
    label <- if (rlang::as_name(stat) != "xG_diff") {
      str_to_title(str_remove(as_label(stat), "_diff"))
    } else {
      str_remove(as_label(stat), "_diff")
    }
    file <- if (rlang::as_name(stat) != "xG_diff") {
      str_c(str_to_lower(label), "s", sep = "")
    } else {
      str_to_lower(label)
    }

    data |>
      mutate(
        team_colours = case_when(
          team == "Bayern Munich" ~ "#DB4254",
          team == "Borussia Dortmund" ~ "#ffa600",
          .default = "#dee2e6"
        ),
        team_alpha = case_when(
          team %in% c("Bayern Munich", "Borussia Dortmund") ~ 1,
          .default = .2
        ),
        team_size = case_when(
          team %in% c("Bayern Munich", "Borussia Dortmund") ~ 7,
          .default = 4
        )
      ) |>
      ggplot(aes(season, !!stat)) +
      geom_hline(
        yintercept = 0, colour = "grey20",
        linetype = "dashed", linewidth = 1
      ) +
      geom_smooth(aes(colour = team_colours, alpha = team_alpha),
        method = lm, formula = y ~ splines::bs(x),
        se = FALSE, linewidth = 1.2
      ) +
      geom_point(aes(fill = team_colours, alpha = team_alpha, size = team_size),
        shape = 21, stroke = 1
      ) +
      facet_wrap(vars(game_state)) +
      scale_colour_identity() +
      scale_fill_identity() +
      scale_alpha_identity() +
      scale_size_identity() +
      scale_x_continuous(
        breaks = c(2014:2022),
        labels = c(
          "2014/15", "2015/16", "2016/17", "2017/18", "2018/19",
          "2019/20", "2020/21", "2021/22", "2022/23"
        )
      ) +
      labs(
        title = glue(
          "Bundesliga Performance (Measured Using {label} ",
          "Difference) by Game State, 2014/15 - 2022/23"
        ),
        subtitle = subtitle,
        caption = glue(
          "Source: Understat (via worldfootballR) | ",
          "Graphic: Paul Johnson (@paul_johnson89)"
        ),
        x = NULL, y = glue("{label} Difference /90")
      ) +
      theme_ftw() +
      theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust = 0.5))


    ggsave(here::here("figures", paste0(file, "_comparison.png")),
      dpi = 320, width = 20, height = 10
    )
  }

# compare points dropped from winning positions
plot_points_dropped <- function(data, games) {
  data |>
    filter(season == 2022 & games == {{ games }}) |>
    mutate(
      team_colours = case_when(team == "Bayern Munich" ~ "#DB4254",
        team == "Borussia Dortmund" ~ "#ffa600",
        .default = "#dee2e6"
      ),
      team_text = case_when(
        dropped_points == 0 ~ .75,
        TRUE ~ as.numeric(dropped_points) - .5
      )
    ) |>
    ggplot(aes(dropped_points, reorder(team, dropped_points), fill = team_colours)) +
    geom_col(colour = "grey20") +
    ## add text (another option would be stat_summary())
    geom_text(
      aes(label = dropped_points, x = team_text),
      ## adjust posiiton inside
      hjust = 1,
      ## label formatting
      size = 4, fontface = "bold",
      colour = "grey10"
    ) +
    geom_vline(xintercept = 0, linewidth = 1, colour = "grey10") +
    labs(title = glue("{ games }"), x = NULL, y = NULL) +
    ## reduce spacing between labels and bars
    scale_x_continuous(expand = c(.025, .025)) +
    # scale_y_discrete(labels = function(x) str_wrap(x, width = 12)) +
    ## add custom colors
    scale_fill_identity() +
    theme_ftw(grid_y = FALSE, grid_x = TRUE) +
    theme(
      plot.title = ggtext::element_markdown(
        size = rel(1), hjust = .5,
        margin = margin(t = 10, b = 5)
      ),
      plot.title.position = "panel",
      axis.text.y = element_text(
        size = rel(0.8), vjust = .5,
        margin = margin(l = 5, r = -5)
      ),
      axis.text.x = element_text(size = rel(0.8)),
      plot.margin = margin(l = 10, 0, 0, 0)
    )
}

plot_buli_points <- function(data) {
  p1 <- plot_points_dropped(data, games = "All Games")
  p2 <- plot_points_dropped(data, games = "Home Games")
  p3 <- plot_points_dropped(data, games = "Away Games")

  (p1 | p2 | p3) +
    plot_annotation(
      title = "Points Dropped from Winning Positions by Bundesliga Teams, 2022/23",
      subtitle = glue(
        "<b style='color:#ffa600;'>Borussia Dortmund</b> dropped relatively ",
        "few points from winning positions last season, though they were much ",
        "stronger at home, dropping only three points, versus seven points ",
        "away from home.<br>All this is overshadowed by <b style='color:#DB4254;'>",
        "Bayern Munich</b>'s uncharacteristically poor grip on winning positions. ",
        "Bayern dropped more points from winning positions than Borussia ",
        "Dortmund, both home and away."
      ),
      caption = glue(
        "Source: Transfermarkt (via worldfootballR) | ",
        "Graphic: Paul Johnson (@paul_johnson89)"
      ),
      theme = theme_ftw()
    )

  ggsave(here::here("figures", "dropped_points.png"),
    dpi = 320, width = 20, height = 10
  )
}


# plot results as proportion of winning or losing positions
plot_results <-
  function(data, team) {
    data |>
      filter(games == "All Games" & team == {{ team }}) |>
      tidyr::pivot_longer(
        cols = c(wins, draws, losses),
        names_to = "outcome",
        values_to = "total"
      ) |>
      mutate(outcome = factor(outcome,
        levels = c("wins", "draws", "losses")
      )) |>
      ggplot(aes(reorder(season, rev(season)), total, fill = outcome)) +
      geom_col(position = "fill", colour = "grey20") +
      scale_fill_manual(
        values = if (team == "Borussia Dortmund") {
          c("#ffa600", "#fec43d", "#ffdf68")
        } else {
          c("#db4254", "#e86d73", "#f29394")
        },
        labels = function(x) str_to_title(x)
      ) +
      scale_x_discrete(labels = rev(c(
        "2014/15", "2015/16", "2016/17", "2017/18",
        "2018/19", "2019/20", "2020/21", "2021/22",
        "2022/23"
      ))) +
      geom_hline(yintercept = 0, linewidth = 1, colour = "grey10") +
      coord_flip() +
      labs(
        x = NULL, y = "Proportion of Games"
      ) +
      theme_ftw(grid_y = FALSE, grid_x = TRUE) +
      theme(
        axis.text.y = element_text(vjust = .5, margin = margin(l = 5, r = -5)),
        plot.margin = margin(l = 10, 0, 0, 0)
      )
  }

plot_leads <-
  function(data) {
    p1 <- plot_results(data, "Borussia Dortmund")
    p2 <- plot_results(data, "Bayern Munich")

    (p1 | p2) +
      plot_annotation(
        title = glue(
          "<b style='color:#ffa600;'>Borussia Dortmund</b> & ",
          "<b style='color:#DB4254;'>Bayern Munich</b>'s Bundesliga ",
          "Results from Winning Positions, 2014/15 - 2022/23"
        ),
        subtitle = glue(
          "Despite a lot of draws in 2017/18, Dortmund have been pretty ",
          "consistent when holding a lead, giving up more than 25% of winning ",
          "positions just once. Last season, BVB dropped relatively few points ",
          "from<br>winning positions, while Bayern Munich dropped points from ",
          "~25% of all winning positions. Bayern's grip on leads has loosened ",
          "a little in recent years, with the last two seasons being their ",
          "worst in this area."
        ),
        caption = glue(
          "Source: Transfermarkt (via worldfootballR) | ",
          "Graphic: Paul Johnson (@paul_johnson89)"
        ),
        theme = theme_ftw(grid_y = FALSE, grid_x = FALSE)
      )

    ggsave(here::here("figures", "leads_comparison.png"),
      dpi = 320, width = 20, height = 10
    )
  }

plot_deficits <-
  function(data) {
    p1 <- plot_results(data, "Borussia Dortmund")
    p2 <- plot_results(data, "Bayern Munich") +
      theme(axis.text.y = element_blank())

    (p1 | p2) +
      plot_annotation(
        title = glue(
          "<b style='color:#ffa600;'>Borussia Dortmund</b> & ",
          "<b style='color:#DB4254;'>Bayern Munich</b>'s Bundesliga ",
          "Results from Losing Positions, 2014/15 - 2022/23"
        ),
        subtitle = glue(
          "Despite variance in the proportion of deficits that both turn ",
          "around, BVB usually end up worse off than Bayern. Dortmund ",
          "overcame more than 50% of deficits three times, compared with ",
          "Bayern's six. Last<br>season, however, Bayern claimed points from ",
          "~44% of deficits compared with Dortmund's ~42%. Bayern have ",
          "seemingly declined in recent seasons, while BVB have remained ",
          "relatively steady in that time."
        ),
        caption = glue(
          "Source: Transfermarkt (via worldfootballR) | ",
          "Graphic: Paul Johnson (@paul_johnson89)"
        ),
        theme = theme_ftw(grid_y = FALSE, grid_x = FALSE)
      )

    ggsave(here::here("figures", "deficits_comparison.png"),
      dpi = 320, width = 20, height = 10
    )
  }

plot_comebacks <-
  function(data) {
    facet_labels <- as_labeller(c(
      win_percent = "Wins",
      comeback_percent = "Wins + Draws"
    ))

    data |>
      filter(games == "All Games") |>
      tidyr::pivot_longer(
        cols = ends_with("percent"),
        names_to = "stat",
        values_to = "percent"
      ) |>
      mutate(
        stat = factor(stat, levels = c("win_percent", "comeback_percent")),
        team_colours = case_when(
          team == "Bayern Munich" ~ "#DB4254",
          team == "Borussia Dortmund" ~ "#ffa600",
          .default = "#dee2e6"
        ),
        team_alpha = case_when(
          team %in% c("Bayern Munich", "Borussia Dortmund") ~ 1,
          .default = .2
        ),
        team_size = case_when(
          team %in% c("Bayern Munich", "Borussia Dortmund") ~ 7,
          .default = 5
        )
      ) |>
      ggplot(aes(season, percent)) +
      geom_smooth(aes(colour = team_colours, alpha = team_alpha),
        method = lm, formula = y ~ splines::bs(x),
        se = FALSE, linewidth = 1.2
      ) +
      geom_point(aes(fill = team_colours, alpha = team_alpha, size = team_size),
        shape = 21, stroke = 1
      ) +
      facet_wrap(vars(stat), labeller = facet_labels) +
      scale_colour_identity() +
      scale_fill_identity() +
      scale_alpha_identity() +
      scale_size_identity() +
      scale_x_continuous(
        breaks = c(2014:2022),
        labels = c(
          "2014/15", "2015/16", "2016/17", "2017/18", "2018/19",
          "2019/20", "2020/21", "2021/22", "2022/23"
        )
      ) +
      scale_y_continuous(labels = scales::label_percent()) +
      labs(
        title = glue::glue(
          "Percentage of Deficits that Bundesliga Teams Reverse ",
          "to Win or Draw, 2014/15 - 2022/23"
        ),
        subtitle = glue::glue(
          "<b style='color:#DB4254;'>Bayern Munich</b> have been winning ",
          "fewer games from losing positions in recent seasons, levelling ",
          "things up between them and <b style='color:#ffa600;'>Borussia ",
          "Dortmund</b>, but are still overturning more deficits overall. Both<br>",
          "are closer to league average in recent seasons, suggesting the ",
          "Bundesliga is a little more competitive right now (though it's not clear ",
          "if they are getting worse or the rest of the league is getting better)."
        ),
        caption = glue(
          "Source: Transfermarkt (via worldfootballR) | ",
          "Graphic: Paul Johnson (@paul_johnson89)"
        ),
        x = NULL, y = "% of Total Deficits"
      ) +
      theme_ftw() +
      theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust = 0.5))

    ggsave(here::here("figures", paste0("comebacks.png")),
      dpi = 320, width = 20, height = 10
    )
  }

plot_timing_distributions <-
  function(data) {
    data |>
      filter(season == 2022) |>
      mutate(
        team = case_when(
          team == "Bayern Munich" ~ "Bayern Munich",
          team == "Borussia Dortmund" ~ "Borussia Dortmund",
          .default = "Rest of the Bundesliga"
        ),
        team = factor(team, levels = c(
          "Bayern Munich",
          "Borussia Dortmund",
          "Rest of the Bundesliga"
        ))
      ) |>
      tidyr::pivot_longer(
        cols = ends_with("diff"),
        names_to = "stat"
      ) |>
      mutate(
        stat = recode(stat,
          goal_diff = "Goal Difference /90",
          xG_diff = "xG Difference /90",
          shot_diff = "Shot Difference /90"
        ),
        stat = factor(stat, levels = c(
          "Shot Difference /90",
          "Goal Difference /90",
          "xG Difference /90"
        )),
        team_colours = case_when(
          team == "Bayern Munich" ~ "#DB4254",
          team == "Borussia Dortmund" ~ "#ffa600",
          .default = "#dee2e6"
        ),
        point_alpha = case_when(
          team %in% c("Bayern Munich", "Borussia Dortmund") ~ 1,
          .default = .2
        ),
        line_alpha = case_when(
          team %in% c("Bayern Munich", "Borussia Dortmund") ~ 1,
          .default = 0
        ),
        team_size = case_when(
          team %in% c("Bayern Munich", "Borussia Dortmund") ~ 8,
          .default = 4
        )
      ) |>
      ggplot(aes(timing, value)) +
      geom_hline(
        yintercept = 0, colour = "grey20",
        linetype = "dashed", linewidth = 1
      ) +
      geom_line(aes(group = team, alpha = line_alpha), colour = "grey20") +
      geom_point(aes(fill = team_colours, alpha = point_alpha, size = team_size),
        shape = 21, stroke = 1
      ) +
      facet_wrap(vars(stat), scales = "free_y") +
      scale_fill_identity() +
      scale_alpha_identity() +
      scale_size_identity() +
      labs(
        title = glue::glue(
          "Bundesliga Performances (Shots, Goals, and xG ",
          "Difference) by Game Minutes, 2022/23"
        ),
        subtitle = glue::glue(
          "<b style='color:#ffa600;'>Borussia Dortmund</b> & <b style='color:",
          "#DB4254;'>Bayern Munich</b> outperformed the league average across ",
          "all three metrics last season. However, the change in Bayern's goal ",
          "difference between halves is striking. Their goal difference<br>",
          "in the first half of games was sky high, but it plummets in the second ",
          "half. Dortmund's xG difference also drops off in the second half, ",
          "but it bounces back to a BVB game high in the final ",
          "15 minutes of games."
        ),
        caption = glue(
          "Source: Understat (via worldfootballR) | ",
          "Graphic: Paul Johnson (@paul_johnson89)"
        ),
        x = NULL, y = NULL
      ) +
      theme_ftw()

    ggsave(here::here("figures", "timings.png"),
      dpi = 320, width = 20, height = 10
    )
  }

plot_timing_splits <-
  function(data, stat, subtitle) {
    stat <- enquo(stat)
    label <- if (rlang::as_name(stat) != "xG_diff") {
      str_to_title(str_remove(as_label(stat), "_diff"))
    } else {
      str_remove(as_label(stat), "_diff")
    }
    file <- if (rlang::as_name(stat) != "xG_diff") {
      str_c(str_to_lower(label), "s", sep = "")
    } else {
      str_to_lower(label)
    }

    data |>
      mutate(
        team_colours = case_when(
          team == "Bayern Munich" ~ "#DB4254",
          team == "Borussia Dortmund" ~ "#ffa600",
          .default = "#dee2e6"
        ),
        team_alpha = case_when(
          team %in% c("Bayern Munich", "Borussia Dortmund") ~ 1,
          .default = .2
        ),
        team_size = case_when(
          team %in% c("Bayern Munich", "Borussia Dortmund") ~ 6,
          .default = 4
        )
      ) |>
      ggplot(aes(season, !!stat)) +
      geom_hline(
        yintercept = 0, colour = "grey20",
        linetype = "dashed", linewidth = 1
      ) +
      geom_smooth(aes(colour = team_colours, alpha = team_alpha),
        method = lm, formula = y ~ splines::bs(x),
        se = FALSE, linewidth = 1.2
      ) +
      geom_point(aes(fill = team_colours, alpha = team_alpha, size = team_size),
        shape = 21, stroke = 1
      ) +
      facet_wrap(vars(timing)) +
      scale_colour_identity() +
      scale_fill_identity() +
      scale_alpha_identity() +
      scale_size_identity() +
      scale_x_continuous(
        breaks = c(2014:2022),
        labels = c(
          "2014/15", "2015/16", "2016/17", "2017/18", "2018/19",
          "2019/20", "2020/21", "2021/22", "2022/23"
        )
      ) +
      labs(
        title = glue(
          "Bundesliga Performance (Measured Using {label} ",
          "Difference) by Game Minutes, 2014/15 - 2022/23"
        ),
        subtitle = subtitle,
        caption = glue(
          "Source: Understat (via worldfootballR) | ",
          "Graphic: Paul Johnson (@paul_johnson89)"
        ),
        x = NULL, y = glue("{label} Difference /90")
      ) +
      theme_ftw() +
      theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust = 0.5))


    ggsave(here::here("figures", paste0(file, "_timings.png")),
      dpi = 320, width = 20, height = 10
    )
  }
