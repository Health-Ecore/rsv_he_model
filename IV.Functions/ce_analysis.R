box::use(dplyr[...])

#' Test dominance and extended dominance of alternative strategies
#'
#' @param ce_data tibble with strategy, costs and qalys, one row per strategy.
#'
#' @returns tibble with strategies and whether they are dominated or should remain as an option
#' @export
#'
test_dominance <- function(ce_data) {
  ce_data <- ungroup(ce_data)

  #first test for strongly dominated options
  dominated_strategies <- character()

  for (i in 1:nrow(ce_data)) {
    ce_ref <- ce_data %>%
      filter(strategy == ce_data$strategy[i])

    ref_costs <- ce_ref$costs
    ref_effects <- ce_ref$qalys

    add_dominated <- ce_data %>%
      filter(strategy != ce_data$strategy[i]) %>%
      mutate(dif_c = costs - ref_costs, dif_u = qalys - ref_effects) %>%
      filter(dif_u < 0, dif_c > 0) %>%
      pull(strategy)

    dominated_strategies <- c(dominated_strategies, add_dominated)
  }

  strong_dominance <- tibble(
    strategy = dominated_strategies,
    result = "dominated"
  ) %>%
    distinct()

  finished <- FALSE
  weakly_dominated_strategies <- character()

  while (finished == FALSE) {
    # test for weakly dominated options
    iterator <- ce_data %>%
      filter(
        !(strategy %in% dominated_strategies),
        !(strategy %in% weakly_dominated_strategies)
      ) %>%
      arrange(costs) %>%
      mutate(
        dif_c = costs - lag(costs),
        dif_u = qalys - lag(qalys),
        icer = dif_c / dif_u,
        dif_icer = lead(icer) - icer,
        result = case_when(
          dif_icer < 0 ~ "weakly dominated",
          dif_icer >= 0 ~ "remain",
          is.na(dif_c) ~ "reference",
          is.na(dif_icer) ~ "remain"
        )
      ) %>%
      select(strategy, result)

    weakly_dominated_add <- iterator %>%
      filter(result == "weakly dominated") %>%
      pull(strategy)

    weakly_dominated_strategies <- c(
      weakly_dominated_strategies,
      weakly_dominated_add
    )

    if (length(weakly_dominated_add > 0)) {
      finished <- FALSE
    } else {
      finished <- TRUE
    }
  }

  remain <- iterator %>%
    filter(result != "weakly dominated")

  weak_dominance <- tibble(
    strategy = weakly_dominated_strategies,
    result = "weakly dominated"
  )

  result <- bind_rows(strong_dominance, weak_dominance, remain)

  return(result)
}

#' Create CE frontier
#'
#' @param ce_data tibble with strategy, costs and qalys, one row per strategy.
#'
#' @returns coordinates for CE frontier (costs and qalys)
#' @export
#'
#' @examples
create_ce_frontier <- function(ce_data) {
  #make selection of relevant data
  ce_data <- ungroup(ce_data)

  #exclude dominated and weakly dominated strategies
  included_strategies <- test_dominance(ce_data) %>%
    filter(result != "dominated", result != "weakly dominated") %>%
    pull(strategy)

  reference <- test_dominance(ce_data) %>%
    filter(result == "reference") %>%
    pull(strategy)

  #return only relevant outcomes
  outc_relevant <- ce_data %>%
    filter(strategy %in% included_strategies)

  outc_reference <- outc_relevant |>
    filter(strategy == reference)

  outc_incremental <- ce_data |>
    filter(strategy %in% included_strategies) |>
    mutate(
      costs = costs - outc_reference$costs,
      qalys = qalys - outc_reference$qalys
    )

  return(outc_incremental)
}

#' Create ICERs for various strategies
#'
#' @param ce_data tibble with column for strategy, costs and qalys
#'
#' @returns
#'
#' @export
#' @examples
create_icers <- function(ce_data) {
  ce_data <- ungroup(ce_data)
  dominance <- test_dominance(ce_data)
  frontier <- create_ce_frontier(ce_data) |>
    arrange(costs) |>
    mutate(order = row_number()) |>
    select(strategy, order)

  ce_combined <- left_join(ce_data, dominance, by = "strategy") |>
    left_join(frontier, by = "strategy") |>
    arrange(order)

  icers <- ce_combined |>
    arrange(order) |>
    mutate(
      reference = case_when(
        result == "dominated" ~ "no reference",
        order == 1 ~ "no reference",
        order > 1 ~ lag(strategy)
      ),
      ref_qalys = case_when(
        result == "dominated" ~ NA,
        order == 1 ~ NA,
        order > 1 ~ lag(qalys)
      ),
      ref_costs = case_when(
        result == "dominated" ~ NA,
        order == 1 ~ NA,
        order > 1 ~ lag(costs)
      ),
      icer = case_when(
        result == "remain" ~ round((costs - ref_costs) / (qalys - ref_qalys)),
        result != "remain" ~ NA
      ),
      result = case_when(
        result == "remain" ~ as.character(icer),
        .default = result
      )
    )
  return(icers)
}
