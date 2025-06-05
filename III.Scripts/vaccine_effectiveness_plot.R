# Load packages -----------------------------------------------------------
box::use(dplyr[...])
box::use(purrr[...])
box::use(gt[...])
box::use(stringr[...])
box::use(ggplot2[...])
library(showtext)

## Load custom modules -----------------------------------------------------
options(box.path = getwd())
box::purge_cache()
box::use(IV.Functions/currency_cbs)
box::use(IV.Functions/demographics)
box::use(IV.Functions/reference_prices)
box::use(IV.Functions/discounting)
box::use(IV.Functions/vaccine)
box::use(IV.Functions/rsv_dectree)

showtext_auto()
font_add_google(name = "assistant")
health_ecore_colors <- c("#0091d3", "#82bb25", "#ed1262", "#01775f", "#94372f", "#005c87", "#4d6e16", "#99003a")
rivernights_colors <- c("#b30000", "#7c1158", "#4421af", "#1a53ff", "#0d88e6", "#00b7c7", "#5ad45a", "#8be04e", "#ebdc78")
theme_set(theme_minimal() + theme(text = element_text(family = "assistant", size = 20)))

probabilistic <- TRUE
iter <- 1000

settings <- rsv_dectree$create_settings()

settings_annual <- rsv_dectree$create_settings(
  included_strategies = list(
    no_vaccine = list(
      name = "no_vaccine",
      nice_name = "No vaccine",
      settings_file = "no_vaccine",
      administration = 1,
      waning = "none",
      price = 0
    ),
    rsv_generic_vaccine = list(
      name = "rsv_generic_vaccine",
      nice_name = "RSV generic vaccine",
      settings_file = "rsv_generic_vaccine",
      administration = 1,
      waning = "sigmoid",
      price = 150
    )
  )
)




settings_biannual <- rsv_dectree$create_settings(
  included_strategies = list(
    no_vaccine = list(
      name = "no_vaccine",
      nice_name = "No vaccine",
      settings_file = "no_vaccine",
      administration = 2,
      waning = "none",
      price = 0
    ),
    rsv_generic_vaccine = list(
      name = "rsv_generic_vaccine",
      nice_name = "RSV generic vaccine",
      settings_file = "rsv_generic_vaccine",
      administration = 2,
      waning = "sigmoid",
      price = 150
    )
  )
)

settings_60months <- rsv_dectree$create_settings(
  included_strategies = list(
    no_vaccine = list(
      name = "no_vaccine",
      nice_name = "No vaccine",
      settings_file = "no_vaccine",
      administration = 3,
      waning = "none",
      price = 0
    ),
    rsv_generic_vaccine = list(
      name = "rsv_generic_vaccine",
      nice_name = "RSV generic vaccine",
      settings_file = "rsv_generic_vaccine_scenario5",
      administration = 3,
      waning = "sigmoid",
      price = 150
    )
  )
)

settings_36months <- rsv_dectree$create_settings(
  included_strategies = list(
    no_vaccine = list(
      name = "no_vaccine",
      nice_name = "No vaccine",
      settings_file = "no_vaccine",
      administration = 3,
      waning = "none",
      price = 0
    ),
    rsv_generic_vaccine = list(
      name = "rsv_generic_vaccine",
      nice_name = "RSV generic vaccine",
      settings_file = "rsv_generic_vaccine_scenario5b",
      administration = 3,
      waning = "sigmoid",
      price = 150
    )
  )
)

settings_linear <- rsv_dectree$create_settings(
  included_strategies = list(
    no_vaccine = list(
      name = "no_vaccine",
      nice_name = "No vaccine",
      settings_file = "no_vaccine",
      administration = 3,
      waning = "none",
      price = 0
    ),
    rsv_generic_vaccine = list(
      name = "rsv_generic_vaccine",
      nice_name = "RSV generic vaccine",
      settings_file = "rsv_generic_vaccine",
      administration = 3,
      waning = "linear",
      price = 150
    )
  )
)


base_case <- rsv_dectree$load_data(settings, probabilistic, iter)$vaccines$rsv_generic_vaccine$effectiveness |> 
  mutate(effectiveness = map(effectiveness, ~tibble(iter = 1:iter, effectiveness = .x), iter),
         date = lubridate::as_date(stringr::str_c(year, "-", month,"-1")),
         scenario = "Vaccination every three years") |> 
  tidyr::unnest(effectiveness)

linear <- rsv_dectree$load_data(settings_linear, probabilistic, iter)$vaccines$rsv_generic_vaccine$effectiveness |> 
  mutate(effectiveness = map(effectiveness, ~tibble(iter = 1:iter, effectiveness = .x), iter),
         date = lubridate::as_date(stringr::str_c(year, "-", month,"-1")),
         scenario = "Linear waning") |> 
  tidyr::unnest(effectiveness)


annual <- rsv_dectree$load_data(settings_annual, probabilistic, iter)$vaccines$rsv_generic_vaccine$effectiveness |> 
  mutate(effectiveness = map(effectiveness, ~tibble(iter = 1:iter, effectiveness = .x), iter),
         date = lubridate::as_date(stringr::str_c(year, "-", month,"-1")),
         scenario = "Annual vaccination") |> 
  tidyr::unnest(effectiveness)

biannual <- rsv_dectree$load_data(settings_biannual, probabilistic, iter)$vaccines$rsv_generic_vaccine$effectiveness |> 
  mutate(effectiveness = map(effectiveness, ~tibble(iter = 1:iter, effectiveness = .x), iter),
         date = lubridate::as_date(stringr::str_c(year, "-", month,"-1")),
         scenario = "Vaccination every two years") |> 
  tidyr::unnest(effectiveness)

months_60 <- rsv_dectree$load_data(settings_60months, probabilistic, iter)$vaccines$rsv_generic_vaccine$effectiveness |> 
  mutate(effectiveness = map(effectiveness, ~tibble(iter = 1:iter, effectiveness = .x), iter),
         date = lubridate::as_date(stringr::str_c(year, "-", month,"-1")),
         scenario = "60 months waning") |> 
  tidyr::unnest(effectiveness)

months_36 <- rsv_dectree$load_data(settings_36months, probabilistic, iter)$vaccines$rsv_generic_vaccine$effectiveness |> 
  mutate(effectiveness = map(effectiveness, ~tibble(iter = 1:iter, effectiveness = .x), iter),
         date = lubridate::as_date(stringr::str_c(year, "-", month,"-1")),
         scenario = "36 months waning") |> 
  tidyr::unnest(effectiveness)
  


bind_rows(
  base_case,
  linear,
  months_60,
  months_36,
  biannual,
  annual
) |> 
  group_by(outcome, date, scenario) |> 
  summarise(effect = mean(effectiveness),
            low = quantile(effectiveness, .025),
            high = quantile(effectiveness, .975),
            .groups = "drop") |> 
  filter(outcome %in% c("hosp", "non_medical", "out_lower", "out_upper")) |> 
  mutate(outcome = case_when(
    outcome == "hosp" ~ "Hospitalization",
    outcome == "non_medical" ~ "Non-medically attended RSV cases",
    outcome == "out_lower" ~ "Outpatient RSV cases (LRTD)",
    outcome == "out_upper" ~"Outpatient RSV cases (URTD)"
  ),
  scenario = factor(scenario, levels = c("Vaccination every three years", "Vaccination every two years", "Annual vaccination", "Linear waning", "36 months waning", "60 months waning"))) |> 
  ungroup() |> 
  ggplot() +
  geom_line(aes(x = date, y = effect), color = rivernights_colors[[5]]) +
  geom_ribbon(aes(x = date, ymin = low, ymax = high), alpha = .2, fill = rivernights_colors[[5]]) +
  facet_grid(rows = vars(scenario), cols = vars(outcome), switch = "y") +
  ggplot2::scale_y_continuous(name = "Vaccine effectiveness", limits = c(0,1), labels = scales::label_percent()) +
  xlab("Year")

ggsave(str_c("VI. Tables and figures/vaccine_effectiveness.tiff"), scale = 1.5, height = 1500, width = 1000,units = "px")
