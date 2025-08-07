## Load packages -----------------------------------------------------------
box::use(dplyr[...])
box::use(purrr[...])
box::use(furrr[...])
box::use(future[...])
box::use(stringr[...])
box::use(arrow[...])
box::use(ggplot2[...])
box::use(gt[...])
library(showtext)


options(box.path = getwd())
box::purge_cache()
box::use(IV.Functions / currency_cbs)
box::use(IV.Functions / demographics)
box::use(IV.Functions / reference_prices)
box::use(IV.Functions / vaccine)
box::use(IV.Functions / rsv_dectree)
box::use(IV.Functions / discounting)
box::use(IV.Functions / record_results)
box::use(IV.Functions / ce_analysis)

settings <- rsv_dectree$create_settings()
showtext_auto()
font_add_google(name = "assistant")
health_ecore_colors <- c(
  "#0091d3",
  "#82bb25",
  "#ed1262",
  "#01775f",
  "#94372f",
  "#005c87",
  "#4d6e16",
  "#99003a"
)
rivernights_colors <- c(
  "#b30000",
  "#7c1158",
  "#4421af",
  "#1a53ff",
  "#0d88e6",
  "#00b7c7",
  "#5ad45a",
  "#8be04e",
  "#ebdc78"
)
theme_set(
  theme_minimal() + theme(text = element_text(family = "assistant", size = 20))
)

save_name <- "may26"
save_path <- stringr::str_c("V. Output/results/", save_name, "/ce")

res <- arrow::open_dataset(save_path) |>
  select(-strategy) |>
  rename(strategy = vac_strategy) |>
  ungroup()

res <- collect(res) |> ungroup() |> mutate(qalys = qalys * -1)

nmb <- res |>
  mutate(
    data = pmap(
      list(costs, qalys),
      ~ tibble(wtp = 100 * (0:2000)) |> mutate(nmb = (..2 * wtp) - ..1)
    )
  ) |>
  tidyr::unnest(data)

ceac_data_max <- nmb |>
  group_by(iter, wtp) |>
  summarise(max_nmb = max(nmb), .groups = "drop")


iterations <- max(ceac_data_max$iter)

ceac_data <- nmb |>
  left_join(ceac_data_max, by = c("wtp", "iter")) |>
  mutate(option_ce = if_else(nmb == max_nmb, 1, 0)) |>
  group_by(strategy, wtp) |>
  summarise(prob = (sum(option_ce) / iterations), .groups = "drop") |>
  mutate(
    timing = case_when(
      strategy == "reference" ~ "No vaccination",
      str_sub(strategy, 1, 1) == "2" ~ "Every two years",
      str_sub(strategy, 1, 1) == "3" ~ "Every three years",
    ),
    age_group = case_when(
      strategy == "reference" ~ "No vaccination",
      str_sub(strategy, 5) == "all_60" ~ "ages 60 and over",
      str_sub(strategy, 5) == "all_75" ~ "ages 75 and over",
      str_sub(strategy, 5) == "all_75_hr_60" ~
        "ages 75 and over, risk groups 60 and over",
      .default = "error"
    ),
    strategy = case_when(
      strategy == "reference" ~ "No vaccination",
      strategy != "reference" ~
        str_c("Vaccination ", timing, " for ", age_group)
    )
  )


ceac_data |>
  ggplot2::ggplot(ggplot2::aes(
    x = wtp,
    y = prob,
    color = age_group,
    linetype = timing
  )) +
  ggplot2::geom_line() +
  ggplot2::scale_y_continuous(
    name = "Probability RSV vaccination is cost effective",
    limits = c(0, 1),
    labels = scales::label_percent()
  ) +
  ggplot2::scale_x_continuous(
    name = "Willingness to pay",
    labels = scales::label_currency(prefix = "€")
  ) +
  scale_color_manual(
    values = c(
      "No vaccination" = rivernights_colors[1],
      "ages 60 and over" = rivernights_colors[3],
      "ages 75 and over" = rivernights_colors[5],
      "ages 75 and over, risk groups 60 and over" = rivernights_colors[8]
    ),
    name = "Vaccinated age group"
  ) +
  scale_linetype_manual(
    values = c(
      "Every two years" = "longdash",
      "Every three years" = "solid",
      "No vaccination" = "dotted"
    ),
    name = "Timing of vaccination"
  ) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 4), linetype = guide_legend(nrow = 4))

ggsave(
  str_c("VI. Tables and figures/", save_name, "_ceac.tiff"),
  scale = 1.5,
  height = 1000,
  width = 1500,
  units = "px"
)

nmb_mean <- res |>
  ungroup() |>
  group_by(strategy) |>
  summarise(costs = mean(costs), qalys = mean(qalys), .groups = "drop") |>
  mutate(
    qalys = qalys,
    data = pmap(
      list(costs, qalys),
      ~ tibble(wtp = 100 * (0:2000)) |> mutate(nmb = (..2 * wtp) - ..1)
    )
  ) |>
  tidyr::unnest(data)

ceaf_data <- nmb_mean |>
  group_by(wtp) |>
  summarise(max_nmb = max(nmb), .groups = "drop") |>
  left_join(
    select(nmb_mean, optimal_strategy = strategy, nmb),
    by = c(max_nmb = "nmb")
  ) |>
  left_join(ceac_data, by = c(optimal_strategy = "strategy", "wtp"))

ceaf_data |>
  ggplot2::ggplot(ggplot2::aes(x = wtp, y = prob, color = optimal_strategy)) +
  ggplot2::geom_line() +
  ggplot2::scale_y_continuous(
    name = "Probability RSV vaccination is cost effective",
    limits = c(0, 1),
    labels = scales::label_percent()
  ) +
  ggplot2::scale_x_continuous(
    name = "Willingness to pay",
    labels = scales::label_currency(prefix = "€")
  ) +
  ggplot2::theme_minimal()

ref <- res |>
  filter(strategy == "reference") |>
  rename(costs_ref = costs, qalys_ref = qalys) |>
  select(-strategy)

int <- res |>
  filter(strategy != "reference") |>
  rename(costs_int = costs, qalys_int = qalys) |>
  left_join(ref, by = "iter") |>
  mutate(
    costs = costs_int - costs_ref,
    qalys = qalys_int - qalys_ref,
    timing = case_when(
      as.double(str_sub(strategy, 1, 1)) == 2 ~ "Every two years",
      as.double(str_sub(strategy, 1, 1)) == 3 ~ "Every three years",
    ),
    age_group = case_when(
      str_sub(strategy, 5) == "all_60" ~ "Ages 60 and over",
      str_sub(strategy, 5) == "all_75" ~ "Ages 75 and over",
      str_sub(strategy, 5) == "all_75_hr_60" ~
        "Ages 75 and over, risk groups 60 and over",
      .default = "error"
    )
  )

int_mean <- int |>
  bind_rows()


res_mean <- res |>
  group_by(strategy) |>
  summarise(costs = mean(costs), qalys = mean(qalys))

dominated_strategies <- ce_analysis$test_dominance(res_mean)
ce_frontier <- ce_analysis$create_ce_frontier(res_mean)

int_ce <- int |>
  mutate(icer = costs / qalys)

int_ce |>
  ggplot2::ggplot() +
  geom_abline(intercept = 0, slope = 50000, color = "#808080", linetype = 2) +
  ggplot2::geom_point(
    ggplot2::aes(y = costs, x = qalys, color = age_group),
    alpha = .01
  ) +
  ggplot2::geom_point(
    mapping = aes(y = costs, x = qalys, shape = timing, color = age_group),
    data = int_mean,
    size = 2
  ) +
  ggplot2::geom_line(aes(y = costs, x = qalys), data = ce_frontier) +
  ggplot2::scale_x_continuous(name = "Incremental QALYs", limits = c(0, NA)) +
  ggplot2::scale_y_continuous(
    name = "Incremental costs",
    labels = scales::label_currency(prefix = "€"),
    limits = c(0, NA)
  ) +
  annotate(
    "text",
    x = 29000,
    y = 1250000000,
    label = "WTP: €50,000 per QALY",
    size = 5,
    color = "#808080"
  ) +
  scale_shape_manual(
    values = c("Every two years" = 17, "Every three years" = 15),
    name = "Timing of vaccination"
  ) +
  scale_color_manual(
    values = rivernights_colors[c(1, 3, 6)],
    name = "Vaccinated age group"
  )

ggsave(
  str_c("VI. Tables and figures/", save_name, "_ce_plane.tiff"),
  scale = 1.7,
  height = 800,
  width = 1500,
  units = "px",
  dpi = 300
)


#Various tables
save_path <- stringr::str_c("V. Output/results/", save_name, "/mean_outcomes")

#table with results for first season
tbl <- arrow::open_dataset(save_path) |>
  select(-strategy, -cri_low, -cri_high, -median) |>
  filter(discounted == FALSE | is.na(discounted)) |>
  rename(strategy = vac_strategy) |>
  collect() |>
  mutate(
    item = case_when(
      category == "costs" ~ cost_category,
      category != "costs" ~ item
    )
  ) |>
  group_by(strategy, season, category, item, discounted) |>
  summarise(value = sum(mean), .groups = "drop")

tbl_ref <- tbl |>
  filter(strategy == "reference") |>
  rename(value_reference = value) |>
  select(-strategy)

tbl_int <- tbl |>
  filter(strategy != "reference") |>
  rename(value_intervention = value)

tbl_inc <- left_join(
  tbl_int,
  tbl_ref,
  by = join_by(season, category, item, discounted)
) |>
  mutate(
    category = case_when(
      category == "qalys" ~ "QALYs lost",
      category == "life years" ~ "Life years lost",
      str_detect(item, "mortality") ~ "Mortality",
      category == "events" & item == "non-medically attended RSV" ~
        "Non-medically attended cases",
      category == "events" ~ "Healthcare utilization",
      category != "qalys" ~ str_to_sentence(category)
    ),
    item = case_when(
      item == "ed" ~ "Emergency department",
      item == "gp" ~ "GP",
      item == "total RSV cases" ~ "Total RSV cases",
      item == "gp visit" ~ "GP visits",
      item == "nma" ~ "Non-medically attended RSV",
      item == "non-medically attended RSV" ~ "Non-medically attended RSV",
      item == "Non-medically attended rsv" ~ "Non-medically attended RSV",
      item == "adverse event, grade 4 (gbs)" ~ "Adverse event, grade 4 (GBS)",
      item == "hospitalization (non-ICU)" ~
        "Hospitalization (not addmited to ICU)",
      item == "hospitalization (ICU)" ~
        "Hospitalization (including ICU admission)",
      item == "productivity" ~ "Productivity losses",
      item == "mortality (hospital)" ~ "Hospital mortality",
      item == "mortality (nursing homes)" ~ "Nursing home mortality",
      item == "life years lost (hospital)" ~ "Hospital mortality",
      item == "life years lost (nursing homes)" ~ "Nursing home mortality",
      .default = str_to_sentence(item)
    ),
    value_reference = if_else(is.na(value_reference), 0, value_reference),
    value_intervention = value_intervention - value_reference
  ) |>
  filter(
    season == "2025 - 2026",
    item != "Hospitalization (total)",
    item != "Mortality (total)"
  ) |>
  select(-discounted, -season) |>
  tidyr::pivot_wider(
    values_from = "value_intervention",
    names_from = "strategy"
  )


tbl_out <- tbl_inc |>
  gt(rowname_col = "item", groupname_col = "category") |>
  tab_spanner(
    label = "Vaccination every two years",
    columns = starts_with("2yr")
  ) |>
  tab_spanner(
    label = "Vaccination every three years",
    columns = starts_with("3yr")
  ) |>
  summary_rows(
    groups = "Costs",
    columns = everything(),
    fns = list(Total = ~ sum(.)),
    fmt = list(~ fmt_currency(., currency = "EUR", decimals = 0))
  ) |>
  summary_rows(
    groups = c("Mortality", "Life years lost", "QALYs lost"),
    columns = everything(),
    fns = list(Total = ~ sum(.)),
    fmt = list(~ fmt_number(., decimals = 0))
  ) |>
  cols_label(
    value_reference = "No vaccination",
    ends_with("all_60") ~ "All adults 60 and over",
    ends_with("all_75") ~ "All adults 75 and over",
    ends_with("hr_60") ~ "All adults 75 and over + risk groups older than 60"
  ) |>
  fmt_currency(
    rows = category == "Costs",
    currency = "EUR",
    decimals = 0
  ) |>
  fmt_number(
    rows = category != "Costs",
    decimals = 0
  ) |>
  row_group_order(
    c(
      "Healthcare utilization",
      "Non-medically attended cases",
      "Mortality",
      "Costs",
      "Qaly"
    )
  ) |>
  tab_footnote(
    "Displayed are the annual results, for the first RSV season modelled"
  ) |>
  tab_footnote(
    "GBS: Guillain-Barré syndrome, GP: General Practitioner, ICU: Intensive Care Unit, QALY: Quality Adjusted Life year, RSV: Respiratory Syncytial Virus"
  )


gtsave(
  tbl_out,
  filename = str_c(
    "VI. Tables and figures/",
    save_name,
    "results_firstseason.html"
  )
)


# Table with average results per season
tbl_inc_mean <- left_join(
  tbl_int,
  tbl_ref,
  by = join_by(season, category, item, discounted)
) |>
  mutate(
    category = case_when(
      category == "qalys" ~ "QALYs lost",
      category == "life years" ~ "Life years lost",
      str_detect(item, "mortality") ~ "Mortality",
      category == "events" & item == "non-medically attended RSV" ~
        "Non-medically attended cases",
      category == "events" ~ "Healthcare utilization",
      category != "qalys" ~ str_to_sentence(category)
    ),
    item = case_when(
      item == "ed" ~ "Emergency department",
      item == "gp" ~ "GP",
      item == "total RSV cases" ~ "Total RSV cases",
      item == "gp visit" ~ "GP visits",
      item == "nma" ~ "Non-medically attended RSV",
      item == "non-medically attended RSV" ~ "Non-medically attended RSV",
      item == "Non-medically attended rsv" ~ "Non-medically attended RSV",
      item == "adverse event, grade 4 (gbs)" ~ "Adverse event, grade 4 (GBS)",
      item == "hospitalization (non-ICU)" ~
        "Hospitalization (not addmited to ICU)",
      item == "hospitalization (ICU)" ~
        "Hospitalization (including ICU admission)",
      item == "productivity" ~ "Productivity losses",
      item == "mortality (hospital)" ~ "Hospital mortality",
      item == "mortality (nursing homes)" ~ "Nursing home mortality",
      item == "life years lost (hospital)" ~ "Hospital mortality",
      item == "life years lost (nursing homes)" ~ "Nursing home mortality",
      .default = str_to_sentence(item)
    ),
    value_reference = if_else(is.na(value_reference), 0, value_reference),
    value_intervention = value_intervention - value_reference
  ) |>
  filter(
    item != "Hospitalization (total)",
    item != "Mortality (total)",
    discounted == FALSE | is.na(discounted)
  ) |>
  group_by(strategy, category, item) |>
  summarise(
    value_intervention = mean(value_intervention),
    value_reference = mean(value_reference),
    .groups = "drop"
  ) |>
  tidyr::pivot_wider(
    values_from = "value_intervention",
    names_from = "strategy"
  )


tbl_out_mean <- tbl_inc_mean |>
  gt(rowname_col = "item", groupname_col = "category") |>
  tab_spanner(
    label = "Vaccination every two years",
    columns = starts_with("2yr")
  ) |>
  tab_spanner(
    label = "Vaccination every three years",
    columns = starts_with("3yr")
  ) |>
  summary_rows(
    groups = "Costs",
    columns = everything(),
    fns = list(Total = ~ sum(.)),
    fmt = list(~ fmt_currency(., currency = "EUR", decimals = 0))
  ) |>
  summary_rows(
    groups = c("Mortality", "Life years lost", "QALYs lost"),
    columns = everything(),
    fns = list(Total = ~ sum(.)),
    fmt = list(~ fmt_number(., decimals = 0))
  ) |>
  cols_label(
    value_reference = "No vaccination",
    ends_with("all_60") ~ "All adults 60 and over",
    ends_with("all_75") ~ "All adults 75 and over",
    ends_with("hr_60") ~ "All adults 75 and over + risk groups older than 60"
  ) |>
  fmt_currency(
    rows = category == "Costs",
    currency = "EUR",
    decimals = 0
  ) |>
  fmt_number(
    rows = category != "Costs",
    decimals = 0
  ) |>
  row_group_order(
    c(
      "Costs",
      "Healthcare utilization",
      "Mortality",
      "Non-medically attended cases",
      "Qaly"
    )
  ) |>
  tab_footnote(
    "Displayed are the mean annual undiscounted results, over the six modelled seasons"
  ) |>
  tab_footnote(
    "GBS: Guillain-Barré syndrome, GP: General Practitioner, ICU: Intensive Care Unit, QALY: Quality Adjusted Life year, RSV: Respiratory Syncytial Virus"
  )


gtsave(
  tbl_out_mean,
  filename = str_c(
    "VI. Tables and figures/",
    save_name,
    "clinical_mean_results.html"
  )
)

#table with results per season
tbl_inc_ps <- left_join(
  tbl_int,
  tbl_ref,
  by = join_by(season, category, item, discounted)
) |>
  filter(str_sub(strategy, 1, 3) == "3yr") |>
  mutate(
    category = case_when(
      category == "qaly" ~ "QALYs lost",
      category == "qale" ~ "QALYs lost",
      category == "le" ~ "Life years lost",
      str_detect(item, "mortality") ~ "Mortality",
      category == "events" & item == "non-medically attended RSV" ~
        "Non-medically attended cases",
      category == "events" ~ "Healthcare utilization",
      category != "qalys" ~ str_to_sentence(category)
    ),
    item = case_when(
      item == "ed" ~ "Emergency department",
      item == "gp" ~ "GP",
      item == "total RSV cases" ~ "Total RSV cases",
      item == "gp visit" ~ "GP visits",
      item == "nma" ~ "Non-medically attended RSV",
      item == "non-medically attended RSV" ~ "Non-medically attended RSV",
      item == "Non-medically attended rsv" ~ "Non-medically attended RSV",
      item == "adverse event, grade 4 (gbs)" ~ "Adverse event, grade 4 (GBS)",
      item == "hospitalization (non-ICU)" ~
        "Hospitalization (not addmited to ICU)",
      item == "hospitalization (ICU)" ~
        "Hospitalization (including ICU admission)",
      item == "productivity" ~ "Productivity losses",
      item == "mortality (hospital)" ~ "Hospital mortality",
      item == "mortality (nursing homes)" ~ "Nursing home mortality",
      item == "life years lost (hospital)" ~ "Hospital mortality",
      item == "life years lost (nursing homes)" ~ "Nursing home mortality",
      .default = str_to_sentence(item)
    ),
    value_reference = if_else(is.na(value_reference), 0, value_reference),
    value_intervention = value_intervention - value_reference,
    strategy = str_c(strategy, "_", season)
  ) |>
  filter(
    item != "Hospitalization (total)",
    item != "Mortality (total)",
    item != "Total RSV cases",
    season %in% c("2025 - 2026", "2026 - 2027", "2027 - 2028")
  ) |>
  select(-season, -discounted) |>
  tidyr::pivot_wider(
    values_from = c("value_intervention", "value_reference"),
    names_from = "strategy"
  ) |>
  select(-starts_with("value_reference_3yr_all_75"))

tbl_out_ps <- tbl_inc_ps |>
  mutate(across(.cols = 3:14, ~ if_else(is.na(.x), 0, .x))) |>
  gt(rowname_col = "item", groupname_col = "category") |>
  summary_rows(
    groups = "Costs",
    columns = everything(),
    fns = list(Total = ~ sum(.)),
    fmt = list(~ fmt_currency(., currency = "EUR", decimals = 0))
  ) |>
  summary_rows(
    groups = c("Mortality", "Life years lost", "QALYs lost"),
    columns = everything(),
    fns = list(Total = ~ sum(.)),
    fmt = list(~ fmt_number(., decimals = 0))
  ) |>
  cols_label(
    starts_with("value_reference") ~ "No vaccination",
    starts_with("value_intervention_3yr_all_60") ~
      "All adults 60 and over (incremental)",
    starts_with("value_intervention_3yr_all_75") ~
      "All adults 75 and over (incremental)",
    starts_with("value_intervention_3yr_all_75_hr_60") ~
      "All adults 75 and over + risk groups older than 60 (incremental)"
  ) |>
  fmt_currency(
    rows = category == "Costs",
    currency = "EUR",
    decimals = 0
  ) |>
  fmt_number(
    rows = category != "Costs",
    decimals = 0
  ) |>
  row_group_order(
    c(
      "Healthcare utilization",
      "Non-medically attended cases",
      "Mortality",
      "Costs",
      "Life years lost",
      "QALYs lost"
    )
  ) |>
  sub_missing() |>
  sub_zero(zero_text = "—") |>
  tab_spanner(
    label = "2025 - 2026",
    columns = c(
      "value_reference_3yr_all_60_2025 - 2026",
      ends_with("2025 - 2026")
    )
  ) |>
  tab_spanner(
    label = "2026 - 2027",
    columns = c(
      "value_reference_3yr_all_60_2026 - 2027",
      ends_with("2026 - 2027")
    )
  ) |>
  tab_spanner(
    label = "2027 - 2028",
    columns = c(
      "value_reference_3yr_all_60_2027 - 2028",
      ends_with("2027 - 2028")
    )
  ) |>
  tab_footnote(
    "Relevant if vaccinating every three years",
    location = cells_column_spanners("2027 - 2028")
  ) |>
  tab_footnote("Per season undiscounted results") |>
  tab_footnote(
    "GBS: Guillain-Barré syndrome, GP: General Practitioner, ICU: Intensive Care Unit, QALY: Quality Adjusted Life year, RSV: Respiratory Syncytial Virus"
  )

gtsave(
  tbl_out_ps,
  filename = str_c(
    "VI. Tables and figures/",
    save_name,
    "table_2_expanded.html"
  )
)

#smaller table with results per season
tbl_inc_ps_sm <- left_join(
  tbl_int,
  tbl_ref,
  by = join_by(season, category, item, discounted)
) |>
  filter(str_sub(strategy, 1, 3) == "3yr") |>
  mutate(
    category = case_when(
      category == "qaly" ~ "QALYs lost",
      category == "life years" ~ "Life years lost",
      str_detect(item, "mortality") ~ "Mortality",
      category == "events" & item == "non-medically attended RSV" ~
        "Non-medically attended cases",
      category == "events" ~ "Healthcare utilization",
      category != "qalys" ~ str_to_sentence(category)
    ),
    item = case_when(
      item == "ed" ~ "Emergency department",
      item == "gp" ~ "GP",
      item == "total RSV cases" ~ "Total RSV cases",
      item == "gp visit" ~ "GP visits",
      item == "nma" ~ "Non-medically attended RSV",
      item == "non-medically attended RSV" ~ "Non-medically attended RSV",
      item == "Non-medically attended rsv" ~ "Non-medically attended RSV",
      item == "adverse event, grade 4 (gbs)" ~ "Adverse event, grade 4 (GBS)",
      item == "hospitalization (non-ICU)" ~
        "Hospitalization (not admitted to ICU)",
      item == "hospitalization (ICU)" ~
        "Hospitalization (including ICU admission)",
      item == "productivity" ~ "Productivity losses",
      item == "mortality (hospital)" ~ "Hospital mortality",
      item == "mortality (nursing homes)" ~ "Nursing home mortality",
      item == "life years lost (hospital)" ~ "Hospital mortality",
      item == "life years lost (nursing homes)" ~ "Nursing home mortality",
      .default = str_to_sentence(item)
    ),
    value_reference = if_else(is.na(value_reference), 0, value_reference),
    value_intervention = value_intervention - value_reference,
    strategy = str_c(strategy, "_", season)
  ) |>
  filter(
    item != "Hospitalization (not admitted to ICU)",
    item != "Mortality (total)",
    item != "Emergency department visit (patient not hospitalized)",
    item != "Hospitalization (surviving patients)",
    category != "Life years lost",
    item != "Total RSV cases",
    season %in% c("2025 - 2026", "2026 - 2027", "2027 - 2028")
  ) |>
  mutate(
    item_aggr = case_when(
      category == "Costs" & item %in% c("Hospitalization") ~
        "Direct medical costs (inpatient)",
      category == "Costs" &
        item %in% c("Emergency department", "GP", "Medication") ~
        "Direct medical costs (outpatient)",
      str_detect(item, "dverse event") ~ "Adverse events",
      category == "Healthcare utilization" &
        item == "Hospitalization (including ICU admission)" ~
        "ICU admissions",
      category == "Healthcare utilization" & item == "Hospitalization (total)" ~
        "Hospitalizations",
      category == "QALYs lost" &
        item %in%
          c(
            "Emergency department visit (patient not hospitalized)",
            "GP visits",
            "Hospitalization (surviving patients)",
            "Non-medically attended RSV"
          ) ~
        "Morbidity (surviving patients)",
      category == "QALYs lost" &
        item %in% c("Nursing home mortality", "Hospital mortality") ~
        "Mortality",
      .default = item
    ),
    importance = case_when(
      item_aggr == "Vaccination" ~ 1,
      item_aggr == "Adverse events" ~ 5,
      ,
      .default = 2
    ),
    category = case_when(
      category == "Non-medically attended cases" ~ "Healthcare utilization",
      .default = category
    )
  ) |>
  group_by(strategy, category, item_aggr, importance) |>
  summarise(
    value_intervention = sum(value_intervention),
    value_reference = sum(value_reference),
    .groups = "drop"
  ) |>
  tidyr::pivot_wider(
    values_from = c("value_intervention", "value_reference"),
    names_from = "strategy"
  ) |>
  arrange(importance, item_aggr) |>
  select(-starts_with("value_reference_3yr_all_75"), -importance)

tbl_out_ps_sm <- tbl_inc_ps_sm |>
  mutate(across(.cols = 3:14, ~ if_else(is.na(.x), 0, .x))) |>
  gt(rowname_col = "item_aggr", groupname_col = "category") |>
  cols_label(
    starts_with("value_reference") ~ "No vaccination",
    starts_with("value_intervention_3yr_all_60") ~
      "All adults 60 and over (incremental)",
    starts_with("value_intervention_3yr_all_75") ~
      "All adults 75 and over (incremental)",
    starts_with("value_intervention_3yr_all_75_hr_60") ~
      "All adults 75 and over + risk groups older than 60 (incremental)"
  ) |>
  fmt_currency(
    rows = category == "Costs",
    currency = "EUR",
    decimals = 0
  ) |>
  fmt_number(
    rows = category != "Costs",
    decimals = 0
  ) |>
  row_group_order(
    c("Healthcare utilization", "Mortality", "Costs", "QALYs lost")
  ) |>
  sub_missing() |>
  sub_zero(zero_text = "—") |>
  tab_spanner(
    label = "2025 - 2026",
    columns = c(
      "value_reference_3yr_all_60_2025 - 2026",
      ends_with("2025 - 2026")
    )
  ) |>
  tab_spanner(
    label = "2026 - 2027",
    columns = c(
      "value_reference_3yr_all_60_2026 - 2027",
      ends_with("2026 - 2027")
    )
  ) |>
  tab_spanner(
    label = "2027 - 2028",
    columns = c(
      "value_reference_3yr_all_60_2027 - 2028",
      ends_with("2027 - 2028")
    )
  ) |>
  tab_footnote(
    "Relevant if vaccinating every three years",
    location = cells_column_spanners("2027 - 2028")
  ) |>
  tab_footnote("Per season undiscounted results") |>
  tab_footnote(
    "GBS: Guillain-Barré syndrome, GP: General Practitioner, ICU: Intensive Care Unit, QALY: Quality Adjusted Life year, RSV: Respiratory Syncytial Virus"
  )

gtsave(
  tbl_out_ps_sm,
  filename = str_c(
    "VI. Tables and figures/",
    save_name,
    "table_2_simplified.html"
  )
)

ce_icer <- ce_analysis$create_icers(res_mean) |>
  mutate(
    inc_costs = costs - ref_costs,
    inc_qalys = qalys - ref_qalys,
    icer = case_when(
      strategy == "reference" ~ NA,
      is.na(icer) ~ str_to_sentence(result),
      .default = vec_fmt_currency(icer, currency = "EUR", decimals = 0)
    ),
    icer = str_replace(icer, "EUR", "€"),
    reference = case_when(
      reference == "no reference" ~ NA,
      reference == "reference" ~ "No vaccination",
      .default = reference
    )
  ) |>
  select(strategy, inc_qalys, inc_costs, icer, reference)

#table with cost-effectiveness
ce_tbl <- res |>
  left_join(ref, by = "iter") |>
  mutate(
    incremental_costs = costs - costs_ref,
    incremental_qalys = qalys - qalys_ref
  ) |>
  group_by(strategy) |>
  summarise(
    total_costs = mean(costs),
    total_qalys = mean(qalys) * -1,
    incremental_costs = mean(incremental_costs),
    incremental_qalys = mean(incremental_qalys),
    acer = incremental_costs / incremental_qalys,
    .groups = "drop"
  ) |>
  mutate(
    age_group = case_when(
      strategy == "reference" ~ "No vaccination",
      str_sub(strategy, 5) == "all_60" ~ "Ages 60 and over",
      str_sub(strategy, 5) == "all_75" ~ "Ages 75 and over",
      str_sub(strategy, 5) == "all_75_hr_60" ~
        "Ages 75 and over, risk groups 60 and over",
      .default = "error"
    ),
    timing = case_when(
      strategy == "reference" ~ "No vaccination",
      str_sub(strategy, 1, 1) == "2" ~ "Vaccination every two years",
      str_sub(strategy, 1, 1) == "3" ~ "Vaccination every three years",
    ),
    code = case_when(
      strategy == "reference" ~ "A",
      str_sub(strategy, 5) == "all_60" ~ str_c("B", str_sub(strategy, 1, 1)),
      str_sub(strategy, 5) == "all_75" ~ str_c("C", str_sub(strategy, 1, 1)),
      str_sub(strategy, 5) == "all_75_hr_60" ~
        str_c("D", str_sub(strategy, 1, 1)),
      .default = "error"
    ),
    age_code = str_c(age_group, " [", code, "]")
  ) |>
  left_join(ce_icer, by = "strategy")


ce_out <- ce_tbl |>
  left_join(
    mutate(
      select(ce_tbl, ref_code = code, reference = strategy),
      reference = if_else(reference == "reference", "No vaccination", reference)
    ),
    by = "reference"
  ) |>
  select(-code, -age_group, -strategy, -reference) |>
  gt(rowname_col = "age_code", groupname_col = "timing") |>
  fmt_currency(
    columns = c("total_costs", "incremental_costs", "acer", "inc_costs"),
    currency = "EUR",
    decimals = 0
  ) |>
  fmt_number(
    columns = c("total_qalys", "incremental_qalys", "inc_qalys"),
    decimals = 0
  ) |>
  tab_spanner(label = "Total impact", columns = starts_with("total")) |>
  tab_spanner(
    label = "Incremental results compared to no vaccination",
    columns = c(starts_with("incremental"), "acer")
  ) |>
  tab_spanner(
    label = "Incremental results compared to next best strategy",
    columns = c(starts_with("inc_"), "icer")
  ) |>
  row_group_order(
    c(
      "No vaccination",
      "Vaccination every two years",
      "Vaccination every three years"
    )
  ) |>
  cols_merge(
    columns = c(icer, ref_code),
    pattern = "{1}<< [{2}]>>"
  ) |>
  cols_label(
    ends_with("costs") ~ "Costs",
    total_qalys = "QALYs lost",
    inc_qalys = "QALYs gained",
    incremental_qalys = "QALYs gained",
    acer = "ACER",
    icer = "ICER [comparator]"
  ) |>
  sub_missing() |>
  sub_zero(zero_text = "—") |>
  tab_footnote("Displayed are the results for a six year period") |>
  tab_footnote(
    "ACER: Average Cost Effectiveness Ratio, ICER: Incremental Cost Effectiveness Ratio, QALY: Quality Adjusted Life year"
  )

gtsave(
  ce_out,
  filename = str_c("VI. Tables and figures/", save_name, "table_3.html")
)
