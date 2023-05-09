# Understat ----

buli_shots <-
  worldfootballR::understat_league_season_shots(
    league = "Bundesliga",
    season_start_year = 2022
  )

buli_meta <-
  worldfootballR::understat_team_meta(
    team_name = unique(buli_shots$home_team)
  )

buli_team_stats <-
  worldfootballR::understat_team_stats_breakdown(
    team_urls = buli_meta$url
  )

# readr::write_csv(buli_shots, here::here("data", "buli_shots.csv"))
readr::write_csv(buli_team_stats, here::here("data", "buli_team_stats.csv"))

# Transfermarkt ----

leads_params <- list(
  state = rep("punktenachfuehrung", times = 27),
  season = rep(2014:2022, each = 3),
  games = rep(c("alle", "heim", "gast"), times = 9)
)

deficits_params <- list(
  state = rep("punktenachrueckstand", times = 27),
  season = rep(2014:2022, each = 3),
  games = rep(c("alle", "heim", "gast"), times = 9)
)


get_tm_state_data <-
  function(state, season, games) {
    html <-
      rvest::read_html(
        glue::glue("https://www.transfermarkt.com/bundesliga/{state}/",
                   "wettbewerb/L1/plus/1?saison_id={season}&spiele={games}")
        )

    raw_data <-
      html |>
      rvest::html_element("#yw1") |>
      rvest::html_table(header = TRUE)

    if (state == "punktenachfuehrung") {
      tmp_df <- raw_data |>
        setNames(make.unique(names(raw_data))) |>
        dplyr::rename(
          team = Club,
          leads = Club.1,
          wins = `Times leading`,
          draws = Wins,
          losses = Draws,
          points = Losses,
          points_per_lead = Points
        ) |>
        dplyr::select(team:points_per_lead) |>
        dplyr::mutate(
          season = season,
          games = games,
          games = dplyr::recode(
            games,
            "alle" = "All Games",
            "heim" = "Home Games",
            "gast" = "Away Games"
          )
        )
    } else {
      tmp_df <- raw_data |>
        setNames(make.unique(names(raw_data))) |>
        dplyr::rename(
          team = Club,
          deficits = Club.1,
          wins = deficits,
          draws = Wins,
          losses = Draws,
          points = Losses,
          points_per_deficit = Points
        ) |>
        dplyr::select(team:points_per_deficit) |>
        dplyr::mutate(
          season = season,
          games = games,
          games = dplyr::recode(
            games,
            "alle" = "All Games",
            "heim" = "Home Games",
            "gast" = "Away Games"
          )
        )
    }
  }

leads_dfs <- purrr::pmap(leads_params, get_tm_state_data)
deficits_dfs <- purrr::pmap(deficits_params, get_tm_state_data)

buli_leads <- leads_dfs |> dplyr::bind_rows()
buli_deficits <- deficits_dfs |> dplyr::bind_rows()

readr::write_csv(buli_leads, here::here("data", "buli_leads.csv"))
readr::write_csv(buli_deficits, here::here("data", "buli_deficits.csv"))
