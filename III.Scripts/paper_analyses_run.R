## Load packages -----------------------------------------------------------
box::use(dplyr[...])
box::use(purrr[...])
box::use(furrr[...])
box::use(future[...])
box::use(arrow[...])


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

probabilistic <- TRUE
time_step = "annual"
iter <- 5000
n_cores <- 10
save_name <- "apr10"
save_path <- stringr::str_c("V. Output/results/", save_name)

plan(multisession, workers = n_cores)

# Vaccines ----------------------------------------------------------------

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

strategies_2yr <- strategies_3yr
strategies_2yr$no_vaccine$administration <- strategies_2yr$rsv_generic_vaccine$administration <- 2

run_model <- function(population, rsv_prep, rsv_dat, time_step, settings){
  furrr_opts <- furrr_options(
    globals = FALSE,
    packages = NULL,
    seed = TRUE
  )
  
  #bug fix
  population <- population |> 
    mutate(strat = strategy)
  
  if(time_step == "monthly"){
    rsv_runmodel <- population |> 
      left_join(rsv_prep, by = "age") |> 
      mutate( probs = future_pmap(list(risk_group, probs, age, month, year), rsv_dectree$apply_riskstratification, pop_start, .options = furrr_opts),
              probs = future_map2(month, probs, rsv_dectree$add_seasonality, rsv_dat$seasonality, .options = furrr_opts),
              vac_effect = pmap(list(month, year, strat, vaccine_status), rsv_dectree$add_vaccine_effect, rsv_dat)) |> 
      filter(!(month %in% settings$exluded_months)) |> 
      mutate( tree = future_pmap(list(population, probs, vac_effect, prop_nursing), rsv_dectree$run_tree, settings, .options = furrr_opts),
              results = future_pmap(list(month, year, tree, costs, productivity, outcomes, le, le_nursing, indirect_medical_costs, strat, vaccine_status, population, probs), rsv_dectree$calc_results, rsv_dat, probabilistic, iter, settings,
                                    .options = furrr_opts))
  } else if(time_step == "annual"){
    rsv_runmodel <- population |> 
      left_join(rsv_prep, by = "age") |> 
      mutate( probs = pmap(list(risk_group, probs, age, month, year), rsv_dectree$apply_riskstratification, pop_start),
              vac_effect = pmap(list(month, year, strat, vaccine_status), rsv_dectree$add_vaccine_effect, rsv_dat), 
              tree = pmap(list(population, probs, vac_effect, prop_nursing), rsv_dectree$run_tree, settings),
              results = future_pmap(list(month, year, tree, costs, productivity, outcomes, le, le_nursing, indirect_medical_costs, strat, vaccine_status, population, probs), rsv_dectree$calc_results, rsv_dat, probabilistic, iter, settings,
                                    .options = furrr_opts))
  }
  
  #bug fix
  rsv_runmodel <- rsv_runmodel |> 
    select(-strat)
  return(rsv_runmodel)
}
# Run model 3yr -----------------------------------------------------------


settings <- rsv_dectree$create_settings(
  included_strategies = strategies_3yr
)






# Load and prep data ------------------------------------------------------



rsv_dat <- rsv_dectree$load_data(settings, probabilistic, iter)

if(time_step == "monthly"){
  demographic_interval <- "month"
} else if(time_step == "annual"){
  demographic_interval <- "year"
  
  ### CODE TO CHANGE MONTHLY VACCINE EFFECTIVENESS TO ANNUAL ###
  annual_effectiveness <- map(rsv_dat$vaccines, function(x, settings, rsv_dat){
    
    
    new_eff <- x$effectiveness |> 
      left_join(select(rsv_dat$seasonality, month, prop_cases), by = "month") |> 
      mutate(eff_year = case_when(
        month >= settings$start_month ~ year + 1,
        month < settings$start_month ~ year
      ),
      weighed_eff = map2(effectiveness, prop_cases, ~ tibble(iter = 1:length(.x),
                                                             effectiveness = .x * .y))) |> 
      select(-effectiveness) |> 
      tidyr::unnest(weighed_eff) |> 
      group_by(iter, eff_year, outcome) |> 
      summarise(effectiveness = sum(effectiveness),
                month = 1,
                .groups = "drop") |> 
      group_by(outcome, month, eff_year) |> 
      tidyr::nest() |> 
      mutate(effectiveness = map(data, ~.x$effectiveness)) |> 
      ungroup() |> 
      select(outcome, month, effectiveness, year = eff_year)
    
    x$effectiveness <- new_eff
    return(x)
    
  }, settings, rsv_dat)
  
  rsv_dat$vaccines <- annual_effectiveness
  
} else {
  stop("Time step should be either monthly or annual")
}

pop_start <- demographics$create(settings, time_interval = demographic_interval) |>  
  demographics$add_nursinghomes() |> 
  demographics$add_highrisk(settings)


rsv_prep <- rsv_dectree$prep_data(rsv_dat, settings, probabilistic, iter)



# Analysis no vaccination -------------------------------------------------
population <- bind_rows(
  vaccine$add(pop_start, "no_vaccine", settings$vaccine_coverage, settings)
) |> 
  mutate(date = case_when(
    is.na(month) ~ lubridate::as_date(stringr::str_c(year, "-", "01", "-", "01")),
    !(is.na(month)) ~ lubridate::as_date(stringr::str_c(year, "-", month, "-", "01"))
  ),
  season = case_when(
    is.na(month) ~ stringr::str_c(lubridate::year(date) - 1," - ", lubridate::year(date)),
    lubridate::month(date) >= settings$start_month ~ stringr::str_c(lubridate::year(date)," - ", lubridate::year(date)+1),
    lubridate::month(date) < settings$start_month ~ stringr::str_c(lubridate::year(date) - 1," - ", lubridate::year(date))
  ),
  season = as.factor(season),
  strategy = as.factor(strategy)) 

rsv_mr <- run_model(population, rsv_prep, rsv_dat, time_step, settings)  |> 
  select(strategy, season, date, vaccine_status, age, risk_group, population, results) |> 
  tidyr::unnest(results)

rsv_sumres <- rsv_dectree$summarise_results(rsv_mr, settings = settings)
analysis_name <- "reference"

record_results$save(rsv_sumres, stringr::str_c(save_path, "/ce"), analysis_name)

rsv_outcomes_mean <- rsv_mr |> 
  group_by(strategy, season, date, age, vaccine_status, risk_group, category, item, discounted, cost_category, perspective_hc, perspective_soc) |> 
  summarise(value = mean(value),
            low = quantile(value, .025),
            high = quantile(value, .975),
            .groups = "drop")

record_results$save(rsv_outcomes_mean, stringr::str_c(save_path, "/mean_outcomes"), analysis_name)


# Analysis all 60+ strategy -----------------------------------------------
vaccine_coverage  <- "flu"

population <- bind_rows(
  vaccine$add(pop_start, "rsv_generic_vaccine", vaccine_coverage, settings)
) |> 
  mutate(date = case_when(
    is.na(month) ~ lubridate::as_date(stringr::str_c(year, "-", "01", "-", "01")),
    !(is.na(month)) ~ lubridate::as_date(stringr::str_c(year, "-", month, "-", "01"))
  ),
  season = case_when(
    is.na(month) ~ stringr::str_c(lubridate::year(date) - 1," - ", lubridate::year(date)),
    lubridate::month(date) >= settings$start_month ~ stringr::str_c(lubridate::year(date)," - ", lubridate::year(date)+1),
    lubridate::month(date) < settings$start_month ~ stringr::str_c(lubridate::year(date) - 1," - ", lubridate::year(date))
  ),
  season = as.factor(season),
  strategy = as.factor(strategy)) 

rsv_mr <- run_model(population, rsv_prep, rsv_dat, time_step, settings)  |> 
  select(strategy, season, date, vaccine_status, age, risk_group, population, results) |> 
  tidyr::unnest(results)

rsv_sumres <- rsv_dectree$summarise_results(rsv_mr, settings = settings)
analysis_name <- "3yr_all_60"

record_results$save(rsv_sumres, stringr::str_c(save_path, "/ce"), analysis_name)

rsv_outcomes_mean <- rsv_mr |> 
  group_by(strategy, season, date, age, vaccine_status, risk_group, category, item, discounted, cost_category, perspective_hc, perspective_soc) |> 
  summarise(value = mean(value),
            low = quantile(value, .025),
            high = quantile(value, .975),
            .groups = "drop")

record_results$save(rsv_outcomes_mean, stringr::str_c(save_path, "/mean_outcomes"), analysis_name)






# Analysis all 75+ strategy -----------------------------------------------
vaccine_coverage <- "flu_75"

population <- bind_rows(
  vaccine$add(pop_start, "rsv_generic_vaccine", vaccine_coverage, settings)
) |> 
  mutate(date = case_when(
    is.na(month) ~ lubridate::as_date(stringr::str_c(year, "-", "01", "-", "01")),
    !(is.na(month)) ~ lubridate::as_date(stringr::str_c(year, "-", month, "-", "01"))
  ),
  season = case_when(
    is.na(month) ~ stringr::str_c(lubridate::year(date) - 1," - ", lubridate::year(date)),
    lubridate::month(date) >= settings$start_month ~ stringr::str_c(lubridate::year(date)," - ", lubridate::year(date)+1),
    lubridate::month(date) < settings$start_month ~ stringr::str_c(lubridate::year(date) - 1," - ", lubridate::year(date))
  ),
  season = as.factor(season),
  strategy = as.factor(strategy)) 

rsv_mr <- run_model(population, rsv_prep, rsv_dat, time_step, settings)  |> 
  select(strategy, season, date, vaccine_status, age, risk_group, population, results) |> 
  tidyr::unnest(results)

rsv_sumres <- rsv_dectree$summarise_results(rsv_mr, settings = settings)
analysis_name <- "3yr_all_75"

record_results$save(rsv_sumres, stringr::str_c(save_path, "/ce"), analysis_name)

rsv_outcomes_mean <- rsv_mr |> 
  group_by(strategy, season, date, age, vaccine_status, risk_group, category, item, discounted, cost_category, perspective_hc, perspective_soc) |> 
  summarise(value = mean(value),
            low = quantile(value, .025),
            high = quantile(value, .975),
            .groups = "drop")

record_results$save(rsv_outcomes_mean, stringr::str_c(save_path, "/mean_outcomes"), analysis_name)





# Analysis high-risk 60+ all 75+ -----------------------------------------------
vaccine_coverage <- "hr_60_flu_75"

population <- bind_rows(
  vaccine$add(pop_start, "rsv_generic_vaccine", vaccine_coverage, settings)
) |> 
  mutate(date = case_when(
    is.na(month) ~ lubridate::as_date(stringr::str_c(year, "-", "01", "-", "01")),
    !(is.na(month)) ~ lubridate::as_date(stringr::str_c(year, "-", month, "-", "01"))
  ),
  season = case_when(
    is.na(month) ~ stringr::str_c(lubridate::year(date) - 1," - ", lubridate::year(date)),
    lubridate::month(date) >= settings$start_month ~ stringr::str_c(lubridate::year(date)," - ", lubridate::year(date)+1),
    lubridate::month(date) < settings$start_month ~ stringr::str_c(lubridate::year(date) - 1," - ", lubridate::year(date))
  ),
  season = as.factor(season),
  strategy = as.factor(strategy)) 

rsv_mr <- run_model(population, rsv_prep, rsv_dat, time_step, settings)  |> 
  select(strategy, season, date, vaccine_status, age, risk_group, population, results) |> 
  tidyr::unnest(results)

rsv_sumres <- rsv_dectree$summarise_results(rsv_mr, settings = settings)
analysis_name <- "3yr_all_75_hr_60"

record_results$save(rsv_sumres, stringr::str_c(save_path, "/ce"), analysis_name)

rsv_outcomes_mean <- rsv_mr |> 
  group_by(strategy, season, date, age, vaccine_status, risk_group, category, item, discounted, cost_category, perspective_hc, perspective_soc) |> 
  summarise(value = mean(value),
            low = quantile(value, .025),
            high = quantile(value, .975),
            .groups = "drop")

record_results$save(rsv_outcomes_mean, stringr::str_c(save_path, "/mean_outcomes"), analysis_name)





# Analysis 2 yr -----------------------------------------------------------
settings <- rsv_dectree$create_settings(
  outcomes = "base_case",
  probabilities = "base_case",
  n_years = 6,
  included_strategies = strategies_2yr,
  max_age = 90
)


rsv_dat <- rsv_dectree$load_data(settings, probabilistic, iter)

if(time_step == "monthly"){
  demographic_interval <- "month"
} else if(time_step == "annual"){
  demographic_interval <- "year"
  
  ### CODE TO CHANGE MONTHLY VACCINE EFFECTIVENESS TO ANNUAL ###
  annual_effectiveness <- map(rsv_dat$vaccines, function(x, settings, rsv_dat){
    
    
    new_eff <- x$effectiveness |> 
      left_join(select(rsv_dat$seasonality, month, prop_cases), by = "month") |> 
      mutate(eff_year = case_when(
        month >= settings$start_month ~ year + 1,
        month < settings$start_month ~ year
      ),
      weighed_eff = map2(effectiveness, prop_cases, ~ tibble(iter = 1:length(.x),
                                                             effectiveness = .x * .y))) |> 
      select(-effectiveness) |> 
      tidyr::unnest(weighed_eff) |> 
      group_by(iter, eff_year, outcome) |> 
      summarise(effectiveness = sum(effectiveness),
                month = 1,
                .groups = "drop") |> 
      group_by(outcome, month, eff_year) |> 
      tidyr::nest() |> 
      mutate(effectiveness = map(data, ~.x$effectiveness)) |> 
      ungroup() |> 
      select(outcome, month, effectiveness, year = eff_year)
    
    x$effectiveness <- new_eff
    return(x)
    
  }, settings, rsv_dat)
  
  rsv_dat$vaccines <- annual_effectiveness
  
} else {
  stop("Time step should be either monthly or annual")
}
# Analysis all 60+ strategy -----------------------------------------------
vaccine_coverage  <- "flu"

population <- bind_rows(
  vaccine$add(pop_start, "rsv_generic_vaccine", vaccine_coverage, settings)
) |> 
  mutate(date = case_when(
    is.na(month) ~ lubridate::as_date(stringr::str_c(year, "-", "01", "-", "01")),
    !(is.na(month)) ~ lubridate::as_date(stringr::str_c(year, "-", month, "-", "01"))
  ),
  season = case_when(
    is.na(month) ~ stringr::str_c(lubridate::year(date) - 1," - ", lubridate::year(date)),
    lubridate::month(date) >= settings$start_month ~ stringr::str_c(lubridate::year(date)," - ", lubridate::year(date)+1),
    lubridate::month(date) < settings$start_month ~ stringr::str_c(lubridate::year(date) - 1," - ", lubridate::year(date))
  ),
  season = as.factor(season),
  strategy = as.factor(strategy)) 

rsv_mr <- run_model(population, rsv_prep, rsv_dat, time_step, settings)  |> 
  select(strategy, season, date, vaccine_status, age, risk_group, population, results) |> 
  tidyr::unnest(results)

rsv_sumres <- rsv_dectree$summarise_results(rsv_mr, settings = settings)
analysis_name <- "2yr_all_60"

record_results$save(rsv_sumres, stringr::str_c(save_path, "/ce"), analysis_name)

rsv_outcomes_mean <- rsv_mr |> 
  group_by(strategy, season, date, age, vaccine_status, risk_group, category, item, discounted, cost_category, perspective_hc, perspective_soc) |> 
  summarise(value = mean(value),
            low = quantile(value, .025),
            high = quantile(value, .975),
            .groups = "drop")

record_results$save(rsv_outcomes_mean, stringr::str_c(save_path, "/mean_outcomes"), analysis_name)






# Analysis all 75+ strategy -----------------------------------------------
vaccine_coverage <- "flu_75"

population <- bind_rows(
  vaccine$add(pop_start, "rsv_generic_vaccine", vaccine_coverage, settings)
) |> 
  mutate(date = case_when(
    is.na(month) ~ lubridate::as_date(stringr::str_c(year, "-", "01", "-", "01")),
    !(is.na(month)) ~ lubridate::as_date(stringr::str_c(year, "-", month, "-", "01"))
  ),
  season = case_when(
    is.na(month) ~ stringr::str_c(lubridate::year(date) - 1," - ", lubridate::year(date)),
    lubridate::month(date) >= settings$start_month ~ stringr::str_c(lubridate::year(date)," - ", lubridate::year(date)+1),
    lubridate::month(date) < settings$start_month ~ stringr::str_c(lubridate::year(date) - 1," - ", lubridate::year(date))
  ),
  season = as.factor(season),
  strategy = as.factor(strategy)) 

rsv_mr <- run_model(population, rsv_prep, rsv_dat, time_step, settings)  |> 
  select(strategy, season, date, vaccine_status, age, risk_group, population, results) |> 
  tidyr::unnest(results)

rsv_sumres <- rsv_dectree$summarise_results(rsv_mr, settings = settings)
analysis_name <- "2yr_all_75"

record_results$save(rsv_sumres, stringr::str_c(save_path, "/ce"), analysis_name)

rsv_outcomes_mean <- rsv_mr |> 
  group_by(strategy, season, date, age, vaccine_status, risk_group, category, item, discounted, cost_category, perspective_hc, perspective_soc) |> 
  summarise(value = mean(value),
            low = quantile(value, .025),
            high = quantile(value, .975),
            .groups = "drop")

record_results$save(rsv_outcomes_mean, stringr::str_c(save_path, "/mean_outcomes"), analysis_name)





# Analysis high-risk 60+ all 75+ -----------------------------------------------
vaccine_coverage <- "hr_60_flu_75"

population <- bind_rows(
  vaccine$add(pop_start, "rsv_generic_vaccine", vaccine_coverage, settings)
) |> 
  mutate(date = case_when(
    is.na(month) ~ lubridate::as_date(stringr::str_c(year, "-", "01", "-", "01")),
    !(is.na(month)) ~ lubridate::as_date(stringr::str_c(year, "-", month, "-", "01"))
  ),
  season = case_when(
    is.na(month) ~ stringr::str_c(lubridate::year(date) - 1," - ", lubridate::year(date)),
    lubridate::month(date) >= settings$start_month ~ stringr::str_c(lubridate::year(date)," - ", lubridate::year(date)+1),
    lubridate::month(date) < settings$start_month ~ stringr::str_c(lubridate::year(date) - 1," - ", lubridate::year(date))
  ),
  season = as.factor(season),
  strategy = as.factor(strategy)) 

rsv_mr <- run_model(population, rsv_prep, rsv_dat, time_step, settings)  |> 
  select(strategy, season, date, vaccine_status, age, risk_group, population, results) |> 
  tidyr::unnest(results)

rsv_sumres <- rsv_dectree$summarise_results(rsv_mr, settings = settings)
analysis_name <- "2yr_all_75_hr_60"

record_results$save(rsv_sumres, stringr::str_c(save_path, "/ce"), analysis_name)

rsv_outcomes_mean <- rsv_mr |> 
  group_by(strategy, season, date, age, vaccine_status, risk_group, category, item, discounted, cost_category, perspective_hc, perspective_soc) |> 
  summarise(value = mean(value),
            low = quantile(value, .025),
            high = quantile(value, .975),
            .groups = "drop")

record_results$save(rsv_outcomes_mean, stringr::str_c(save_path, "/mean_outcomes"), analysis_name)





plan(sequential)
