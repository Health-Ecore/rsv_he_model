## Load packages -----------------------------------------------------------
box::use(dplyr[...])
box::use(purrr[...])
box::use(furrr[...])
box::use(future[...])
box::use(arrow[...])
box::use(stringr[...])


## Load custom modules -----------------------------------------------------
options(box.path = getwd())
box::purge_cache()
box::use(IV.Functions/currency_cbs)
box::use(IV.Functions/demographics)
box::use(IV.Functions/reference_prices)
box::use(IV.Functions/vaccine)
box::use(IV.Functions/rsv_dectree)
box::use(IV.Functions/discounting)
box::use(IV.Functions/record_results)

set.seed(01042025)
analysis_name <- "dsa"
n_cores <- 10
save_name <- "apr10"
save_path <- stringr::str_c("V. Output/results/", save_name)

strategies_3yr <- list(
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
    waning = "sigmoid",
    price = 150
  )
)

settings <- rsv_dectree$create_settings()

prepped_dsa <- rsv_dectree$prep_dsa(settings, variation = .2)

res_dsa <- rsv_dectree$run_dsa(prepped_dsa, settings, n_cores = n_cores)

record_results$save(res_dsa, stringr::str_c(save_path, "/dsa"), analysis_name)
print(res_dsa)

res_dsa <- arrow::open_dataset(stringr::str_c(save_path, "/dsa")) |>  collect() |> 
  select(-vac_strategy)

library(showtext)
library(ggplot2)
library(gt)
showtext_auto()
font_add_google(name = "assistant")
health_ecore_colors <- c("#0091d3", "#82bb25", "#ed1262", "#01775f", "#94372f", "#005c87", "#4d6e16", "#99003a")
rivernights_colors <- c("#b30000", "#7c1158", "#4421af", "#1a53ff", "#0d88e6", "#00b7c7", "#5ad45a", "#8be04e", "#ebdc78")
theme_set(theme_minimal() + theme(text = element_text(family = "assistant", size = 20)))

rsv_dat <- rsv_dectree$load_data(settings, FALSE, 1)

prob_names <- rsv_dat$probs |> 
  select(param, explanation) |> 
  distinct()

res_dsa2 <- res_dsa |> left_join(prob_names, by = "param") |> 
  mutate(param = str_to_sentence(param),
         category = str_to_sentence(category),
         param = case_when(
    category == "Probs" ~ explanation,
    category == "Productivity" & param == "Duration" ~ "Disease duration",
    param == "Friction_period" ~ "Friction period",
    param == "Gp visit" ~ "GP visit",
    param == "Transport costs gp visit" ~ "Transport costs GP visit",
    param == "Hosp" ~ "Hospitalization",
    param == "Out_upper" ~ "Outpatient URTI",
    param == "Out_lower" ~ "Outpatient LRTI",
    param == "Non_medical" ~ "Non-medical cases",
    param == "Base_case" ~ "base_case",
    param == "Mean_daily_prod_hours" ~ "Mean daily hours of productivity",
    param == "Mean_lifetime_prod_hours" ~ "Mean hours of productivity for remainder of life",
    param == "Qaly_loss | local adverse event" ~ "QALYs lost | Local adverse event",
    param == "Qaly_loss | qaly loss following hospitalization" ~ "QALYs lost | Hospitalization",
    param == "Qaly_loss | severe adverse event" ~ "QALYs lost | Severe adverse event",
    param == "Qaly_loss | systemic adverse event" ~ "QALYs lost | Systemic adverse event",
    .default = param
  ),
  category = case_when(
    category == "Probs" ~ "Probabilities",
    category == "Base_case" ~ "base_case",
    .default = category
  ))

rsv_dectree$create_tornado(res_dsa2, settings, parameter = "icer", cutoff = 10) +
  scale_fill_manual(values = rivernights_colors[c(6,1)])


ggsave(stringr::str_c("VI. Tables and figures/", save_name, "_dsa_main.tiff"), scale = 1.5, height = 800, width = 1000,units = "px")

rsv_dectree$create_tornado(res_dsa2, settings, parameter = "icer") +
  scale_fill_manual(values = rivernights_colors[c(6,1)])


ggsave(stringr::str_c("VI. Tables and figures/", save_name, "_dsa_all.tiff"), scale = 1.5, height = 800, width = 1000,units = "px")

base_case_icer <- res_dsa2 |> filter(param == "base_case") |> pull(icer)

res_dsa_tbl <- res_dsa2 |> 
  arrange(icer) |> 
  filter(category != "base_case") |> 
  mutate(category = if_else(category == "base_case", "Base case", category),
         param = if_else(param == "base_case", "base case", param),
         perc_icer = (icer - base_case_icer) / base_case_icer) |> 
  select(category, param, direction, icer, perc_icer) |> 
  tidyr::pivot_wider(names_from = "direction", values_from = c("icer", "perc_icer")) |> 
  gt(groupname_col = "category", rowname_col = "param") |> 
  fmt_currency(
    columns = starts_with("icer"), currency = "EUR", decimals = 0
  ) |> 
  fmt_percent(
    columns = starts_with("perc_icer"), decimals = 0
  ) |> 
  cols_merge(c("icer_Low value", "perc_icer_Low value"), pattern = "{1} ({2})") |> 
  cols_merge(c("icer_High value", "perc_icer_High value"), pattern = "{1} ({2})") |> 
  cols_label(
    `icer_Low value` = "Low value",
    `icer_High value` = "High value"
  ) |> 
  cols_move_to_end(`icer_High value`) |> 
  tab_footnote(str_c("Base case ICER is: €", vec_fmt_number(base_case_icer, decimals = 0), ". The percentage indicates the deviation from the base-case ICER.")) |> 
  tab_footnote("ICER: Incremental Cost-Effectiveness Ratio, QALY: Quality Adjusted Life year, RSV: Respiratory Syncytial Virus")
gtsave(res_dsa_tbl, filename = str_c("VI. Tables and figures/", save_name, "DSA_tabel.html"))  

                     