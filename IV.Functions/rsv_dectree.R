box::use(dplyr[...])
box::use(purrr[...])
box::use(future[...])
box::use(furrr[...])
box::use(foreach[...])
options(box.path = getwd())
box::use(IV.Functions/reference_prices)
box::use(IV.Functions/currency_cbs)
box::use(IV.Functions/utilities)
box::use(IV.Functions/vaccine)
box::use(IV.Functions/productivity)
box::use(IV.Functions/discounting)
box::use(IV.Functions/paid)
box::use(IV.Functions/demographics)
box::use(IV.Functions/ce_analysis)


#' Function to create settings for RSV model
#'
#' @param min_age minimum age (between 50 and 95)
#' @param max_age maximum age (between 50 and 95)
#' @param cohort_type cohort type is either closed or open, currently supported is closed
#' @param vaccinate_new_cohort should be FALSE at this point
#' @param included_strategies lists with included strategies
#' @param start_year Start year of simulation
#' @param start_month Start month of simulation, should be an integer, i.e., 1 - 12
#' @param n_years number of years to run the analysis
#' @param probabilities csv file containing the probabilities
#' @param costs csv file containing the costs
#' @param outcomes csv file containing the outcomes
#' @param seasonality csv file containing the seasonality
#' @param currency_year the year to be used for the currency
#' @param discount_rate the discount rate to be applied, should be in the form of a list: e.g., list(health = 0.015, economic = 0.03)
#' @param productivity_method method to calculate productivity losses, only friction costs are included now
#' @param indirect_medical indirect medical costs, exclude to exclude indirect medical costs, negative: calculate cost savings for mortality, additive: add medical costs to all survivors (not implemented),  
#' @param currency currency, only "EUR" is supported at this time
#'
#' @return
#' @export
#'
#' @examples
create_settings <- function(
    min_age = 60,
    max_age = 89,
    cohort_type = "closed",
    vaccinate_new_cohort = FALSE,
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
        waning = "sigmoid",
        price = 150
      )
    ),
    start_year = 2025,
    start_month = 10,
    excluded_months = 5:9,
    n_years = 6,
    probabilities = "base_case",
    costs = "base_case",
    outcomes = "base_case",
    seasonality = "nl_pop",
    vaccine_coverage = "hr_60_flu_75",
    currency_year = 2024,
    discount_rate = list(
      health = 0.015,
      economic = 0.03
    ),
    productivity_method = "friction",
    indirect_medical = "exclude",
    include_mortality_nursing = TRUE,
    currency = "EUR",
    risk_stratification = TRUE
){
  #population is not currently used, but is included for compatibility
  population <- c("50-59", "60-64", "65-69", "70-79", "80-89", "90+")
  
  #Perform various checks
  if(min_age < 50 | max_age < 50){
    stop("The included age should not be lower than 50")
  }
  
  if(min_age > 95 | max_age > 95){
    stop("The included age should not be higher than 95")
  }
  
  if(cohort_type != "closed"){
    stop("Currently, only closed cohort types are included")
  }
  
  if(vaccinate_new_cohort != FALSE){
    stop("vaccinate_new_cohort should be FALSE at this time")
  }
  
  if(productivity_method != "friction"){
    stop("Only the friction cost method for productivity is currently supported")
  }
  
  if(!(indirect_medical %in% c("exclude", "negative"))){
    stop("indirect_medical should either be exclude or negative")
  }
  
  if(NA %in% excluded_months){
    excluded_months <- 13
  }
  
  
  return(
    list(
      population = population,
      min_age = min_age,
      max_age = max_age,
      cohort_type = cohort_type,
      vaccinate_new_cohort = vaccinate_new_cohort,
      included_strategies = included_strategies,
      start_year = start_year,
      start_month = start_month,
      n_years = n_years,
      excluded_months = excluded_months,
      probabilities = probabilities,
      costs = costs,
      outcomes = outcomes,
      vaccine_coverage = vaccine_coverage,
      seasonality = seasonality,
      currency_year = currency_year,
      discount_rate = discount_rate,
      productivity_method = productivity_method,
      indirect_medical = indirect_medical, #exclude to exclude indirect medical costs, negative: calculate cost savings for mortality, additive: add medical costs to all survivors (not implemented),  
      include_mortality_nursing = include_mortality_nursing,
      currency = currency,
      risk_stratification = risk_stratification
    )
  )
}

#' Function to load parameters for decision tree
#'
#' @param settings settings list with population details
#'
#' @return list with probabilities and outcomes
#' @export
#'
#' @examples
load_data <- function(settings, probabilistic = FALSE, iter = 1000){
  # Vaccines ----------------------------------------------------------------
  if(settings$cohort_type == "open"){
    warning("Warning: open cohorts currently not implemented correctly")
  }
  if(settings$vaccinate_new_cohort == TRUE){
    warning("Warning: vaccination of new cohorts currently not implemented")
  }
  vaccines <- settings$included_strategies
  
  vac_out <- map(vaccines, ~vaccine$import_rsv_vaccine(
    vaccine = .x$name,
    vaccine_settings_file = .x$settings_file,
    administration = .x$administration,
    waning = .x$waning,
    n_years = settings$n_years,
    probabilistic = probabilistic,
    iter = iter,
    settings = settings
  ))
  
  # Health outcomes ---------------------------------------------------------
  outcomes <- tibble::as_tibble(utils::read.csv(
    stringr::str_c("II.Data/rsv/outcomes/", 
                   settings$outcomes,
                   ".csv")
  )) 
  
  le <- future({utilities$import_le() |> 
      utilities$calculate_discounted_le(settings$discount_rate$health, n_years = settings$n_years) |> 
      filter(age >= settings$min_age)})
  

# Probabilities -----------------------------------------------------------

  
  probabilities <- tibble::as_tibble(utils::read.csv(
                      stringr::str_c("II.Data/rsv/probabilities/", 
                                     settings$probabilities,
                                     ".csv")
                    )) |> 
    mutate(prob = nom / denom) |> 
    filter(explanation != "")
  
  
  

# Costs -------------------------------------------------------------------

  ## load specified costs
  costs_import <- tibble::as_tibble(utils::read.csv(
    stringr::str_c("II.Data/rsv/costs/", 
                   settings$costs,
                   ".csv")
  )) |> 
    rowwise() |> 
    mutate(mean = currency_cbs$convert_cpi(mean, currency_year, settings$currency_year),
           sd = currency_cbs$convert_cpi(sd, currency_year, settings$currency_year),
           currency_year = settings$currency_year) |> 
    ungroup()
  
  hospital_travel_costs <- (reference_prices$get_value("Gemiddelde afstand van huishouden tot ziekenhuis (excl. buitenpolikliniek)") *
    reference_prices$get_value("Auto, kosten per kilometer",
                               currency_year_out = settings$currency_year) +
    reference_prices$get_value("Auto, parkeerkosten per bezoek",
                               currency_year_out = settings$currency_year)) * .7 +
    reference_prices$get_value("Ambulancerit, spoedvervoer",
                               currency_year_out = settings$currency_year) *.3
  
  ## add Dutch reference prices
  ref_prices <- tibble(
    cost_category = c("gp", "ed", "hospitalization and ed", "productivity", "productivity"),
    cost_item = c("gp visit", "emergency department visit", "patient transport to hospital by car", "productivity per hour (paid jobs)", "productivity per hour (informal work)"),
    start_age = 0,
    end_age = 100,
    mean = c(reference_prices$get_value("Huisarts, consult gemiddeld (ook geldend voor telefonisch en e-mail consult)",
                                      currency_year_out = settings$currency_year),
             reference_prices$get_value("Spoedeisende hulp",
                                        currency_year_out = settings$currency_year),
             hospital_travel_costs,
             reference_prices$get_value("Productiviteitskosten per uur per betaald werkende"),
             reference_prices$get_value("Vervangingskosten per uur")),
    sd = NA,
    distribution = "none",
    currency_year = settings$currency_year,
    perspective_hc = c(TRUE, TRUE, FALSE, FALSE, FALSE),
    perspective_soc = TRUE,
    reference = "Dutch costing manual 2024"
  )
  
  
  
  costs <- bind_rows(costs_import, ref_prices)
  

  

# Seasonality -------------------------------------------------------------
  seasonality_import <- tibble::as_tibble(utils::read.csv(
    stringr::str_c("II.Data/rsv/seasonality/", 
                   settings$seasonality,
                   ".csv")
  )) |> 
    mutate(month = lubridate::month(lubridate::ymd(paste0("2024-", month, "-01")), label = FALSE))
  
  


  

# Productivity ------------------------------------------------------------

  productivity_import <- productivity$import_labour(minimum_age = settings$min_age, 
                                                    maximum_age = settings$max_age + settings$n_years - 1)
  

# Indirect medical costs --------------------------------------------------

  le <- value(le)
  
  indirect_medical <- paid$import(settings$currency_year) |> 
    paid$prep_paid(le = le, 
                   min_age = settings$min_age,
                   year = settings$start_year,
                   d_rate = settings$discount_rate$economic)

# Return ------------------------------------------------------------------

return(
  list(
    probs = probabilities,
    costs = costs,
    outcomes = outcomes,
    le = le,
    qol = utilities$import_qol("nl"),
    seasonality = seasonality_import,
    vaccines = value(vac_out),
    labour = productivity_import,
    indirect_medical = indirect_medical
  )
)
}


# Prep functions -----------------------------------------------------------


#' Prepare data for analysis
#'
#' @param settings settings list 
#' @param probabilistic TRUE/FALSE for probabilistic
#' @param data data created with load_data()
#' @param iter number of iterations, only used if probabilistic == TRUE
#'
#' @return a list with the relevant items
#' @export
#'
#' @examples
prep_data <- function(data, settings, probabilistic = FALSE, iter = 1000){
  if(probabilistic == FALSE){
    iter <- 1
  }
  
  qol <- utilities$prep_qol(data$qol, probabilistic, iter) 
  
  le <- future({utilities$calculate_qale(data$le, qol, settings$discount_rate$health) |> 
    tidyr::nest(.by = "age", .key = "le")}, see = TRUE)
  
  le_nursing <- future({prep_data_nursing_mort(data, qol, probabilistic, iter, settings)},
                       seed = TRUE)
  
  probs <- prep_data_prob(data, settings, probabilistic, iter) |> 
    tidyr::nest(.by = "age", .key = "probs")
  
  costs <- prep_data_costs(data, settings, probabilistic, iter) |> 
    tidyr::nest(.by = "age", .key = "costs")
  
  productivity <- prep_data_productivity(data, settings) |> 
    tidyr::nest(.by = "age", .key = "productivity")
  
  outcomes <- prep_data_outcomes(data, settings, probabilistic, iter) |> 
    tidyr::nest(.by = "age", .key = "outcomes")

  
  ret <- probs |> 
    left_join(costs, by = "age") |> 
    left_join(outcomes, by = "age") |> 
    left_join(value(le), by = "age") |> 
    left_join(productivity, by = "age") |> 
    mutate(le_nursing = value(le_nursing))
  
  
    
  return(ret)
  
  
}

#' Helper function to prepare probabilities 
#'
#' @param data data created with load_data()
#' @param settings settings list 
#' @param probabilistic TRUE/FALSE for probabilistic
#' @param iter number of iterations, only used if probabilistic == TRUE
#'
#' @return tibble with probabilities by age for all transition probabilities
#' @export
#'
#' @examples
prep_data_prob <- function(data, settings, probabilistic, iter){
  if(probabilistic == TRUE){
    
    #beta distributions
    probs_beta <- data$probs |> 
      filter(distribution == "beta") |> 
      rowwise() |>
      mutate(prob = list(stats::rbeta(iter, shape1 = nom, shape2 = denom - nom))) |> 
      ungroup()
    
    #truncnormal distributions
    probs_trunc <- data$probs |> 
      filter(distribution == "truncnorm") |> 
      rowwise() |>
      mutate(fr = nom / denom,
             prob = list(truncnorm::rtruncnorm(iter, a = 0, b = 1, mean = fr, sd = se))) |> 
      select(-fr) |> 
      ungroup()
    
    #no distribution
    probs_none <- data$probs |> 
      filter(distribution == "none") |> 
      rowwise() |> 
      mutate(prob = list(nom / denom)) |> 
      ungroup()
    
    probs_cont <- bind_rows(probs_beta, probs_trunc, probs_none)
    
  } else if(probabilistic == FALSE){
    probs_cont <- data$probs |> 
      rowwise() |> 
      mutate(prob = list(nom / denom)) |> 
      ungroup() |> 
      filter(!is.na(prob))
  } else{
    stop("Error: probabilistic should be either TRUE or FALSE")
  }
  
  
  # prep probabilities
  prob_byage <- probs_cont |> 
    mutate(age = map2(age_start, age_end, ~as.list(.x:.y))) |> 
    tidyr::unnest(age) |> 
    filter(age >= settings$min_age,
           age <= settings$max_age + (settings$n_years - 1)) |> 
    mutate(age = as.integer(age)) |> 
    select(age, param, prob)
  
  return(prob_byage)
}

#' Helper function to prepare costs
#'
#' @param data data created with load_data()
#' @param settings settings list 
#' @param probabilistic TRUE/FALSE for probabilistic
#' @param iter number of iterations, only used if probabilistic == TRUE
#'
#' @return tibble with costs by age
#' @export
#'
#' @examples
prep_data_costs <- function(data, settings, probabilistic, iter){
  if(probabilistic == TRUE){
    costs_gamma <- data$costs |> 
      filter(distribution == "gamma") |> 
      rowwise() |> 
      mutate(alpha = (mean^2) / (sd^2),
             beta = (sd^2) / mean,
             cost = list(stats::rgamma(iter, shape = alpha, scale = beta))) |> 
      ungroup() |> 
      select(-alpha, -beta)
    
    costs_none <- data$costs |> 
      filter(distribution == "none") |> 
      mutate(cost = map(mean, ~.x))
    
    
    
    costs_cont <- bind_rows(costs_gamma, costs_none)
  } else if(probabilistic == FALSE){
    costs_cont <- data$costs |> 
      rowwise() |> 
      mutate(cost = list(mean)) |> 
      ungroup()
      
  } else{
    stop("Error: probabilistic should be either TRUE or FALSE")
  }
  
  # prep costs
  costs_byage <- costs_cont |> 
    mutate(age = map2(start_age, end_age, ~as.list(.x:.y))) |> 
    tidyr::unnest(age) |> 
    filter(age >= settings$min_age,
           age <= settings$max_age  + (settings$n_years - 1)) |> 
    mutate(age = as.integer(age)) |> 
    select(age, cost_category, cost_item, cost, perspective_hc, perspective_soc)
  
  return(costs_byage)
}

#' Helper function to prepare outcomes 
#'
#' @param data data created with load_data()
#' @param settings settings list 
#' @param probabilistic TRUE/FALSE for probabilistic
#'
#' @return tibble with outcomes by age
#' @export
#'
#' @examples
prep_data_outcomes <- function(data, settings, probabilistic, iter){
  if(probabilistic == TRUE){
    outc_gamma <- data$outcomes |> 
      filter(distribution == "gamma") |> 
      rowwise() |> 
      mutate(alpha = (mean^2) / (sd^2),
             beta = (sd^2) / mean,
             outc = list(stats::rgamma(iter, shape = alpha, scale = beta))) |> 
      ungroup() |> 
      select(-alpha, -beta)
    
    outc_none <- data$outcomes |> 
      filter(distribution == "none") |> 
      mutate(outc = map(mean, ~.x))
    
    outc_cont <- bind_rows(outc_gamma, outc_none)  
    
  } else if(probabilistic == FALSE){
    outc_cont <- data$outcomes |> 
      mutate(outc = map(mean, ~.x))
  } else{
    stop("Error: probabilistic should be either TRUE or FALSE")
  }
  
  # add outpatient illness to hospitalization duration
  dur_outp <- outc_cont |> 
    filter(item == "gp-attended care",
           category == "duration") |> 
    pull(outc)
  
  outc_hosp <- outc_cont |> 
    filter(item == "hospitalization") |> 
    mutate(outc = map2(outc, dur_outp, ~.x + .y))
  
  outc_total <- outc_cont |> 
    filter(item != "hospitalization") |> 
    add_row(outc_hosp)
  
  
  
  # prep outcomes
  out_byage <- outc_total |> 
    mutate(age = map2(start_age, end_age, ~as.list(.x:.y))) |> 
    tidyr::unnest(age) |> 
    filter(age <= settings$max_age + (settings$n_years - 1),
           age >= settings$min_age) |> 
    mutate(age = as.integer(age)) |> 
    select(age, category, item, outc)
  
  return(out_byage)
}

#' Helper function to prepare productivity effects
#'
#' @param data data created with load_data()
#' @param settings settings list 
#'
#' @return tibble with productivity  by age
#' @export
#'
#' @examples
prep_data_productivity <- function(data, settings){
  method <- settings$productivity_method
  if(method != "friction") {
    stop("Error: only friction cost method is currently implemented")
  }
  
  labour_byage <- data$labour |> 
    mutate(age = map2(min_age, max_age, ~as.list(.x:.y))) |> 
    tidyr::unnest(age) |> 
    filter(age >= settings$min_age,
           age <= settings$max_age  + (settings$n_years - 1)) |> 
    mutate(age = as.integer(age)) |> 
    select(age, indicator, value) |> 
    tidyr::pivot_wider(names_from = "indicator", values_from = value) |> 
    mutate(mean_daily_prod_hours = participation * (duration / 7),
           mean_lifetime_prod_hours = mean_daily_prod_hours * friction_period)
  
  return(labour_byage)
    
}

#' Prep data to calculate mortality in nursing homes
#'
#' @param outcomes outcomes, as part of main rsv data object
#' @param iter number of iterations
#' @param settings settings
#'
#' @returns
#'
#' @examples
prep_data_nursing_mort <- function(data, qol, probabilistic, iter, settings){
  outcomes <- data$outcomes |> 
    filter(stringr::str_detect(item, "nursing"))
  
  
  
  if(probabilistic == TRUE){
    outc_gamma <- outcomes |> 
      filter(distribution == "gamma") |> 
      rowwise() |> 
      mutate(alpha = (mean^2) / (sd^2),
             beta = (sd^2) / mean,
             outc = list(stats::rgamma(iter, shape = alpha, scale = beta))) |> 
      ungroup() |> 
      select(-alpha, -beta)
    
    outc_none <- outcomes |> 
      filter(distribution == "none") |> 
      mutate(outc = map(mean, ~.x))
    
    outc_cont <- bind_rows(outc_gamma, outc_none)  
    
  } else if(probabilistic == FALSE){
    outc_cont <- outcomes |> 
      mutate(outc = map(mean, ~.x))
  } else{
    stop("Error: probabilistic should be either TRUE or FALSE")
  }
  
  
  qol_periter <- qol |> 
    filter(age <= settings$max_age,
           age >= settings$min_age) |> 
    mutate(qol = map(qol, ~tibble(iter = 1:iter, qol = .x))) |> 
    tidyr::unnest(qol) 
  
  le_nursing <- outc_cont |> 
    filter(item == "life expectancy nursing homes (years)") |> 
    mutate(le = map(outc, ~tibble(iter = 1:iter, le = .x)),
           age = 80) |> 
    select(age, item, le) |> 
    tidyr::unnest(le) |> 
    utilities$calculate_discounted_le(settings$discount_rate$health, n_years = settings$n_years) |> 
    filter(year == 1) # Temporary, needs to be changed later to properly incorporate discounting
  
  qale_nursing <- le_nursing |> 
    left_join(qol_periter, by = c("age", "iter")) |> 
    group_by(iter) |> 
    mutate(age2 = age) |> 
    tidyr::nest(le_data = c("age", "le", "year", "disc_le")) |> 
    mutate(qol_data = map2(qol, age2, ~tibble(age = .y, qol = list(.x))),
           qale = map2(le_data, qol_data, ~utilities$calculate_qale(.x, .y, settings$discount_rate$health))) |> 
    select(iter, qale) |> 
    tidyr::unnest(qale) |> 
    group_by(age) |> 
    tidyr::nest() |> 
    mutate(le_nursing = map(data, function(x){
      tibble(le = list(x$le),
             disc_le = list(x$disc_le),
             qale = list(unlist(x$qale)),
             disc_qale = list(unlist(x$disc_qale)))
    })) |> 
    ungroup() |> 
    select(-data, -age)
  
  return(qale_nursing[[1]])
}



# Model run functions -----------------------------------------------------


#' Quick run of RSV model
#'
#' @param settings settings file
#' @param iter number of iterations
#' @param n_cores number of cores
#' @param ret option to set data to return, default is "incremental"
#' @param probabilistic TRUE or FALSE (default), to run the model probabilistic
#' @param time_step "monthly" (default) or "annual", to change the time step in the model
#'
#' @return
#' @export
#'
#' @examples
run <- function(settings, 
                probabilistic = FALSE, 
                iter = 1, 
                n_cores = 1, 
                time_step = "annual"){
  if(n_cores > 1){
    plan(multisession, workers = n_cores)
  }
  
  furrr_opts <- furrr_options(
    globals = FALSE,
    packages = NULL,
    seed = TRUE
  )
  
  ## Load data ---------------------------------------------------------------
  rsv_dat <- load_data(settings, probabilistic, iter)
  
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
  
  population <- bind_rows(
    vaccine$add(pop_start, "no_vaccine", settings$vaccine_coverage, settings),
    vaccine$add(pop_start, "rsv_generic_vaccine", settings$vaccine_coverage, settings)
  ) |> 
    mutate(age_cat = case_when(
      age < 60 ~ "50-59",
      age < 70 ~ "60-69",
      age < 80 ~ "70-79",
      age >= 80 ~ "80+"
    ),
    date = case_when(
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
  
  
  ## Prep data ---------------------------------------------------------------
  rsv_prep <- prep_data(rsv_dat, settings, probabilistic, iter)
  
  
  
  ## Run model ---------------------------------------------------------------
  

  
  
  if(time_step == "monthly"){
    rsv_runmodel <- population |> 
      left_join(select(rsv_prep, age, probs, costs), by = "age") |> 
      mutate( costs = map2(costs, strategy, apply_strategy_costs, settings),
              probs = future_pmap(list(risk_group, probs, age, month, year), apply_riskstratification, pop_start, .options = furrr_opts),
              probs = future_map2(month, probs, add_seasonality, rsv_dat$seasonality, .options = furrr_opts),
              vac_effect = pmap(list(month, year, strategy, vaccine_status), add_vaccine_effect, rsv_dat)) |> 
      filter(!(month %in% settings$exluded_months)) |> 
      mutate( tree = future_pmap(list(population, probs, vac_effect, prop_nursing), run_tree, settings, .options = furrr_opts),
              results = future_pmap(list(month, year, age, tree, costs, strategy, vaccine_status, population, probs), calc_results, rsv_dat, rsv_prep, probabilistic, iter, settings,
                                    .options = furrr_opts)) |> 
      select(strategy, year, month, age_cat, age, risk_group, population, vaccine_status, date, season, results)
    
  } else if(time_step == "annual"){
    rsv_runmodel <- population |> 
      left_join(select(rsv_prep, age, probs, costs), by = "age") |> 
      mutate( costs = map2(costs, strategy, apply_strategy_costs, settings),
              probs = pmap(list(risk_group, probs, age, month, year), apply_riskstratification, pop_start),
              vac_effect = pmap(list(month, year, strategy, vaccine_status), add_vaccine_effect, rsv_dat), 
              tree = pmap(list(population, probs, vac_effect, prop_nursing), run_tree, settings),
              results = future_pmap(list(month, year, age, tree, costs, strategy, vaccine_status, population, probs), calc_results, rsv_dat, rsv_prep, probabilistic, iter, settings,
                                    .options = furrr_opts)) |> 
      select(strategy, year, month, age_cat, age, risk_group, population, vaccine_status, date, season, results)
  }
  

  
  if(n_cores > 1){
    plan(sequential)
  }
  
  return(rsv_runmodel)
}


#' Function to run decision tree
#'
#' @param n number of individuals
#' @param probs probabilities
#' @param settings settings
#' @param vac_effect vaccine effect
#' @param prop_nursing proportion of population in a nursing home
#'
#' @return list of outcomes
#' @export
#'
#' @examples
run_tree <- function(n, probs, vac_effect, prop_nursing, settings){
  l_probs <- probs$prob |>  as.list()
  names(l_probs) <- probs$param
  all_params <- append(l_probs, vac_effect)
  all_params <- append(all_params, list(p_nursing = prop_nursing))
  with(all_params, {
    # hospitalizations
    hosp <- p_hosp_inc_all * ve_hosp
    ed_only <- (1 - p_ed_hosp) * hosp
    ed_total <- hosp + ed_only
    hosp_icu <- hosp * p_hosp_icu
    hosp_nonicu <- hosp * (1 - p_hosp_icu)
    hosp_mort <- hosp * p_hosp_cfr
    hosp_surv <- hosp * (1 - p_hosp_cfr)
    
    # general practitioner visits
    gp <- p_gp_rsv
    gp_urti <- p_gp_rsv * p_gp_urti * ve_out_upper
    gp_lrti <- p_gp_rsv * (1 - p_gp_urti) * ve_out_lower
    gp <- gp_urti + gp_lrti
    
    # non medically attended cases
    nma <- p_gp_rsv * ((1-p_gp_visit)/p_gp_visit) * ve_non_medical
    
    # mortality in nursing homes
    if(settings$include_mortality_nursing == TRUE){
      mort_nursing <- prop_nursing * p_nurhom_mort * p_nurhom_mort_rsv * ve_hosp
    } else if(settings$include_mortality_nursing == FALSE){
      mort_nursing <- 0
    }
    
    
    return(list(
      hosp = hosp * n,
      hosp_mort = hosp_mort * n,
      hosp_surv = hosp_surv * n,
      hosp_icu = hosp_icu * n,
      hosp_nonicu = hosp_nonicu * n,
      ed = ed_total * n,
      ed_only = ed_only * n,
      gp = gp * n,
      gp_urti = gp_urti * n,
      gp_lrti = gp_lrti * n,
      nursing_mort = mort_nursing * n,
      total_mort = (mort_nursing + hosp_mort) * n,
      nma = nma * n
    ))
  })
}

#' Function to run decision tree for adverse events
#'
#' @param n number of individuals
#' @param probs probabilities
#' @param settings settings
#'
#' @returns
#' @export
#'
#' @examples
run_tree_adverse_events <- function(n, probs, settings){
  l_probs <- probs$prob |>  as.list()
  names(l_probs) <- probs$param
  with(l_probs, {
    # adverse events
    ae_local <- p_ae_local * n
    ae_syst <- p_ae_syst * n
    ae_gbs <- p_ae_gbs * n
    
    return(list(
      ae_local = ae_local,
      ae_syst = ae_syst,
      ae_gbs = ae_gbs
    ))
  })
}

#' Calculate outcomes after decision tree has run
#'
#' @param tree outcomes of run_tree()
#' @param costs prepped costs data
#' @param outcomes prepped outcomes data 
#' @param le prepped life expectancy data
#' @param strategy 
#' @param vaccine_status 
#' @param pop_size 
#' @param rsv_dat 
#' @param current_month Month to calculate results for
#' @param current_year 
#' @param iter number of iterations
#' @param probabilistic TRUE/FALSE for probabilistic
#' @param productivity 
#' @param le_nursing 
#' @param indirect_medical 
#' @param probs 
#' @param settings 
#'
#' @return
#' @export
#'
#' @examples
calc_results <- function(current_month, 
                         current_year,
                         age,
                         tree, 
                         costs, 
                         strategy, 
                         vaccine_status, 
                         pop_size, 
                         probs, 
                         rsv_dat, 
                         rsv_prep,
                         probabilistic, 
                         iter, 
                         settings){
  sel_age <- age
  rsv_prep_age <- dplyr::filter(rsv_prep, age == sel_age)
  
  
  if(strategy == "no_vaccine"){
    vac_dat <- rsv_dat$vaccines$no_vaccine
  } else if(strategy == "rsv_generic_vaccine"){
    vac_dat <- rsv_dat$vaccines$rsv_generic_vaccine
  } else{
    stop("Strategy should be either no_vaccine or rsv_generic_vaccine")
  }
  
  #vac_dat <- rsv_dat$vaccines |> pluck(strategy) #OLD WAY OF SELECTING VACCINE TO ANALYSE, GAVE BUGS


  #if annual time steps are used, change some settings
  if(is.na(current_month)){
    vac_dat$vaccination_month <- NA
    vac_dat$vaccination_year <- vac_dat$vaccination_year + 1
  }
  
  
  #add vaccination effects in months where the population is vaccinated
  if(vaccine_status == "vaccinated" & current_month %in% vac_dat$vaccination_month & current_year %in% vac_dat$vaccination_year){
    ae_tree <- run_tree_adverse_events(pop_size, probs, settings)
    vaccinate <- TRUE
  } else {
    ae_tree <- NA
    vaccinate <- FALSE
  }
  
  events <- calc_events(tree, ae_tree, pop_size, vaccinate)
  duration <- calc_duration(events, rsv_prep_age$outcomes[[1]])
  costs_out <- calc_costs(events, costs, rsv_prep_age$productivity[[1]], duration)
  
  # costs_out <- costs_out |> 
  #   calc_vaccination_costs(costs, events, productivity, duration, vac_dat, pop_size, vaccinate)
  
  #to calculate discounted le and qale, the data is dependent on the time step of the analysis
  #calculate the selected_year first, where the starting year == 1
  selected_year <- case_when(
    is.na(current_month) ~ current_year - settings$start_year,
    current_month >= settings$start_month ~ current_year - settings$start_year + 1,
    current_month < settings$start_month ~ current_year - settings$start_year,
  )
  
  #then calculate the output qale and le data
  mortality_out_hosp <- calc_mort(events, 
                                  filter(rsv_prep_age$le[[1]], year == selected_year), 
                                  selected_event = "mortality (hospital)")
  
  mortality_out_nursing <- calc_mort(events, 
                                     rsv_prep_age$le_nursing[[1]], 
                                  selected_event = "mortality (nursing homes)")
  
  
  
  #calculate indirect medical costs
  if(settings$indirect_medical == "exclude"){
    indirect_medical_out <- list(type = "excluded")
    #nothing, as indirect medical costs are excluded from the analysis
  } else if(settings$indirect_medical == "additive"){
    indirect_medical_out <- list(type = "excluded")
    warning("Additive indirect medical costs not yet implemented")
  } else if(settings$indirect_medical == "negative"){
    sel_age <- age
    indirect_med_costs_value <- rsv_dat$indirect_medical |>  
      filter(age == sel_age) |> 
      pull(indirect_medical_costs)
    
    indirect_med_costs_discounted <- rsv_dat$indirect_medical |>  
      filter(age == sel_age) |> 
      pull(discounted)
    
    mort_num <- unlist(mortality_out_hosp$number)
    
    indirect_medical_out <- list(
      type = "negative",
      indirect_med_costs = indirect_med_costs_value * -1 * mort_num,
      discounted = indirect_med_costs_discounted
    )
  }
  
  raw_out <- list(
    events = events,
    costs = costs_out,
    health = calc_health(events, rsv_prep_age$outcomes[[1]]),
    mortality_hosp = mortality_out_hosp,
    mortality_nursing = mortality_out_nursing,
    indirect_medical =  indirect_medical_out
  )
  
  
 
  return(raw_out)
}

#' Calculate number of events
#'
#' @param tree output of run_tree()
#' @param ae_tree output of run_ae_tree()
#' @param pop_size size of population
#' @param vaccinate TRUE or FALSE, depending on whether individuals are vaccinated
#'
#' @return
#' @export
#'
#' @examples
calc_events <- function(tree, ae_tree, pop_size, vaccinate){
  #create tibble with all events coming out of the decision tree
  #use list-columns to be able to incorporate probabilistic results
  events <- tibble(
    event = c("total RSV cases",
              "non-medically attended RSV",
              "gp visit",
              "emergency department visit",
              "emergency department visit (patient not hospitalized)",
              "hospitalization (non-ICU)", 
              "hospitalization (ICU)", 
              "hospitalization (surviving patients)",
              "hospitalization (total)",
              "mortality (hospital)",
              "mortality (nursing homes)",
              "mortality (total)"),
    number = c( list(map2_dbl(tree$nma, tree$gp, sum)),
                list(tree$nma),
                list(tree$gp),
                list(tree$ed),
                list(tree$ed_only),
                list(tree$hosp_nonicu), 
                list(tree$hosp_icu),
                list(tree$hosp_surv),
                list(tree$hosp), 
                list(tree$hosp_mort),
                list(tree$nursing_mort),
                list(tree$total_mort)
              )
  )
  
  if(is.list(ae_tree)){
    ae_events <- tibble(
      event = c("adverse event, grade 3 (local)", 
                "adverse event, grade 3 (systemic)", 
                "adverse event, grade 4 (gbs)"),
      number = c(list(ae_tree$ae_local),
                 list(ae_tree$ae_syst),
                 list(ae_tree$ae_gbs))
    )
    
    
  } else {
    ae_events <- tibble(
      event = c("adverse event, grade 3 (local)", 
                "adverse event, grade 3 (systemic)", 
                "adverse event, grade 4 (gbs)"),
      number = c(list(0),
                 list(0),
                 list(0))
    )
  }
  
  if(vaccinate == TRUE){
    vaccinations <- tibble(
      event = "vaccination",
      number = list(pop_size)
    )
    
    
  } else {
    vaccinations <- tibble(
      event = "vaccination",
      number = list(0)
    )
  }
  
  events <- bind_rows(events, ae_events, vaccinations)
  return(events)
}

#' Calculate duration of disease
#'
#' @param events outcomes of calc_events()
#' @param outcomes prepped outcomes data
#'
#' @return
#' @export
#'
#' @examples
calc_duration <- function(events, outcomes){
  dur <- outcomes |> 
    filter(category == "duration") |> 
    rowwise() |> 
    mutate(outc = list(outc)) |> 
    ungroup()
  
  productivity <- outcomes |> 
    filter(category == "productivity_factor") |> 
    rowwise() |> 
    mutate(outc = list(outc)) |> 
    ungroup()
  
  
  ret <- events |> 
    mutate(duration = case_when(
      event == "non-medically attended RSV" ~ 
        dur |>  filter(item == "non-gp attended care") |> pull(outc),
      event == "gp visit" ~ 
        dur |>  filter(item == "gp-attended care") |> pull(outc),
      event == "adverse event, grade 4 (gbs)" ~ 
        dur |>  filter(item == "severe adverse event") |> pull(outc),
      stringr::str_sub(event, 1, 4) == "hosp" ~ 
        dur |>  filter(item == "hospitalization") |> pull(outc),
      .default = list(0)
    ),
    prod_factor = case_when(
      event == "non-medically attended RSV" ~ 
        productivity |>  filter(item == "non-gp attended care") |> pull(outc),
      event == "gp visit" ~ 
        productivity |>  filter(item == "gp-attended care") |> pull(outc),
      .default = list(1)
    ),
    total_duration = map2(number, duration, ~.x * .y),
    prod_duration = map2(total_duration, prod_factor, ~.x * .y)) |> 
    select(event, total_duration, prod_duration)
  
  return(ret)
}

#' Calculate costs
#'
#' @param events outcomes of calc_events()
#' @param costs prepped costs data
#' @param duration outcomes of calc_duration()
#' @param productivity productivity input data
#'
#' @return
#' @export
#'
#' @examples
calc_costs <- function(events, costs, productivity, duration){
  ## Production losses
  mean_daily_prod_loss <- productivity |> 
    pull(mean_daily_prod_hours)
  
  mean_death_prod_loss <- productivity |> 
    pull(mean_lifetime_prod_hours)
  
  ###calculate the productivity losses due to morbidity
  prod_morbidity <- duration |> 
    filter(event %in% c(
      "non-medically attended RSV",
      "gp visit",
      "emergency department visit (patient not hospitalized)",
      "hospitalization (non-ICU)",
      "hospitalization (ICU)",
      # "adverse event, grade 3 (local)",
      # "adverse event, grade 3 (systemic)",
      "adverse event, grade 4 (gbs)"
    )) |> 
    mutate(productivity_hours = map(prod_duration, ~.x * mean_daily_prod_loss)) |> 
    pull(productivity_hours)
  
  
  prod_morb_trans <- list_transpose(prod_morbidity)
  
  prod_morb <- map_dbl(prod_morb_trans, ~sum(.x))
  
  ###calculate the productivity losses due to mortality
  prod_mortality <- events |> 
    filter(event == "mortality (hospital)") |> 
    mutate(productivity_hours = map(number, ~.x * mean_death_prod_loss)) |> 
    pull(productivity_hours) |> 
    unlist()
  
  
  
  #calculate costs associated with events
  #add productivity losses
  v_events <- stats::setNames(events$number, events$event)
  costs_events <- costs |> 
    mutate(
      linked_total_rsv_cases = map(linked_total_rsv_cases, ~.x * v_events$`total RSV cases`),
      linked_nma = map(linked_nma, ~.x * v_events$`non-medically attended RSV`),
      linked_gp_visit = map(linked_gp_visit, ~.x * v_events$`gp visit`),
      linked_ed_nonhosp = map(linked_ed_nonhosp, ~.x * v_events$`emergency department visit (patient not hospitalized)`),
      linked_hosp_nonicu = map(linked_hosp_nonicu, ~.x * v_events$`hospitalization (non-ICU)`),
      linked_hosp_icu = map(linked_hosp_icu, ~.x * v_events$`hospitalization (ICU)`),
      linked_ae3_local = map(linked_ae3_local, ~.x * v_events$`adverse event, grade 3 (local)`),
      linked_ae3_syst = map(linked_ae3_syst, ~.x * v_events$`adverse event, grade 3 (systemic)`),
      linked_ae4 = map(linked_ae4, ~.x * v_events$`adverse event, grade 4 (gbs)`),
      linked_vaccination = map(linked_vaccination, ~.x * v_events$`vaccination`),
      events = pmap(list(linked_total_rsv_cases, 
                         linked_nma, 
                         linked_gp_visit, 
                         linked_ed_nonhosp, 
                         linked_hosp_nonicu,
                         linked_hosp_icu,
                         linked_ae3_local,
                         linked_ae3_syst,
                         linked_ae4,
                         linked_vaccination),
                    ~..1 + ..2 + ..3 + ..4 + ..5 + ..6 + ..7 + ..8 + ..9 + ..10),
      events = case_when(
        cost_item == "productivity per hour (paid jobs)" ~ list(map2_dbl(prod_morb, prod_mortality, ~.x + .y)),
        .default = events
      ),
      total = map2(cost, events, ~.x * .y)) |> 
    select(cost_category, cost_item, perspective_soc, perspective_hc, total)
  
  return(costs_events)
}


#' Calculate health outcomes
#'
#' @param events outcomes of calc_events()
#' @param outcomes prepped outcomes data 
#'
#' @return
#' @export
#'
#' @examples
calc_health <- function(events, outcomes){
  # Utility losses related to hospitalization
  hosp_qaly_loss <- outcomes |> 
    filter(item == "QALY loss following hospitalization") |>  pull(outc)
  
  hosp <- events |> 
    filter(event == "hospitalization (surviving patients)") |> 
    mutate(qaly_loss = hosp_qaly_loss)
  
  # Utility losses related to patients visiting the GP or ED
  gp_qaly_loss <- map2(
    outcomes |> filter(item == "gp-attended care", category == "duration") |>  pull(outc),
    outcomes |> filter(item == "utility decrement (average)") |>  pull(outc),
    ~(.x * .y) / 365
  )
    
  gp <- events |> 
    filter(event == "gp visit") |> 
    mutate(qaly_loss = gp_qaly_loss)
  
  ed <- events |> 
    filter(event == "emergency department visit (patient not hospitalized)") |> 
    mutate(qaly_loss = gp_qaly_loss)
  
  # Utility losses related to NMA
  nma_qaly_loss <- map2(
    outcomes |> filter(item == "non-gp attended care", category == "duration") |>  pull(outc),
    outcomes |> filter(item == "utility decrement (mild)") |>  pull(outc),
    ~(.x * .y) / 365
  )
  
  nma <- events |> 
    filter(event == "non-medically attended RSV") |> 
    mutate(qaly_loss = nma_qaly_loss)
  

  ret <- bind_rows(
    nma, gp, ed, hosp
  )
  
  if(sum(stringr::str_detect(events$event, "adverse event"))>0){
    # adverse events 
    ae_qaly_loss_local <- filter(outcomes, category == "qaly_loss", item == "local adverse event") 
    ae_qaly_loss_syst <- filter(outcomes, category == "qaly_loss", item == "systemic adverse event") 
    ae_qaly_loss_sev <- filter(outcomes, category == "qaly_loss", item == "severe adverse event") 

    
    aes <- events |> 
      filter(stringr::str_detect(event, "adverse event")) |> 
      mutate(qaly_loss = list(
        unlist(ae_qaly_loss_local$outc), 
        unlist(ae_qaly_loss_syst$outc), 
        unlist(ae_qaly_loss_sev$outc)
      ))
    
    ret <- bind_rows(ret, aes)
  } 
  
  
  #combine and calculate total QALY loss
  ret <- ret |> 
    mutate(total_qaly_loss = map2(number, qaly_loss, ~.x * .y)) |> 
    select(event, total_qaly_loss)
  
  
  
  
  return(ret)
}

#' Calculate mortality
#'
#' @param events outcomes of calc_events()
#' @param le prepped life expectancy data
#'
#' @return
#' @export
#'
#' @examples
calc_mort <- function(events, le, selected_event = "mortality (hospital)"){
  # mortality
  mortality <- filter(events, event == selected_event)
  n_mort <- pull(mortality, number) |>  unlist()
  
  ## calculate expected LE
  le_out <- unlist(le$le) * n_mort
  
  ## calculate expected discounted LE
  disc_le_out <-unlist(le$disc_le) * n_mort
  
  ## calculate expected QALE
  qale_out <- unlist(le$qale) * n_mort
  
  ## calculate expected discounted QALE
  disc_qale_out <- unlist(le$disc_qale) * n_mort
  
  
  ret <- mortality |> 
    mutate(le = list(le_out),
           disc_le = list(disc_le_out),
           qale = list(qale_out),
           disc_qale = list(disc_qale_out))
    
  
  return(ret)
}



# Extract functions -------------------------------------------------------

#' Helper function to extract data
#'
#' @param model_run model_run object
#' @param columns columns to hoist
#'
#' @returns
#'
#' @examples
extract_core_helper <- function(model_run, columns){
  model_run |> 
    tidyr::hoist(.col = results,
                  columns) |> 
    select(strategy, year, month, age_cat, age, risk_group, population, vaccine_status, date, season, all_of(columns))
}

#' Helper function to extract iterations
#'
#' @param unnest_model_run unnested model_run object
#' @param n_iter number of iterations
#'
#' @returns
#'
#' @examples
extract_iterations_helper <- function(unnest_model_run, n_iter) {
  unnest_model_run |> 
    mutate(value = map(value, function(x, n_iter){
      if(length(x) == 1){
        value <- rep(x, n_iter)
      } else if(length(x) == n_iter){
        value <- x
      } else{
        l <- length(x)
        msg <- stringr::str_c("Length of value (",l,
                              ") does not correspond to iter (",
                              n_iter, ").")
        print(ret_pre)
        stop(msg)
      }
      
      tibble(iter = 1:n_iter,
             value = value)
    }, n_iter)) |> 
    tidyr::unnest(value)
}

#' Extract function for events
#'
#' @param model_run model_run object
#' @param n_iter number of iterations
#' @param settings settings
#' @param summarise setting whether to summarise results, TRUE or FALSE
#'
#' @returns
#' @export
#'
#' @examples
extract_events <- function(model_run, n_iter, settings, summarise = TRUE) {
  ret <- extract_core_helper(model_run, "events") |> 
    tidyr::unnest(events) |> 
    rename(value = number) |> 
    extract_iterations_helper(n_iter = n_iter)
  
  if(summarise == TRUE) {
    ret <- ret |> 
      group_by(strategy, year, month, age_cat, age, risk_group, population, vaccine_status, date, season, event) |> 
      summarise(mean = mean(value),
                median = stats::median(value),
                cri_low = stats::quantile(value, .025),
                cri_high = stats::quantile(value, .975),
                .groups = "drop")
  } 
  
  return(ret)
}

#' Extract function for costs
#'
#' @param model_run model_run object
#' @param n_iter number of iterations
#' @param settings settings
#' @param summarise setting whether to summarise results, TRUE or FALSE, output is not per iteration
#' @param simplify setting whether to simplify results, TRUE or FALSE, output is per iteration
#'
#' @returns
#' @export
#'
#' @examples
extract_costs <- function(model_run, n_iter, settings, summarise = TRUE, simplify = FALSE) {
  
  ret <- extract_core_helper(model_run, "costs") |> 
    tidyr::unnest(costs) |> 
    select(strategy, year, month, age_cat, age, risk_group, population, vaccine_status, date, season,
           item = cost_item, value = total, cost_category, perspective_hc, perspective_soc) |>
    mutate(discounted = FALSE)
  
  if(settings$indirect_medical != "exclude"){
    indirect_medical <- model_run |> 
      extract_core_helper("indirect_medical") |> 
      mutate(indirect_medical = map(indirect_medical,
                                    ~tibble(
                                      item = "indirect medical costs",
                                      perspective_hc = FALSE,
                                      perspective_soc = TRUE,
                                      value = list(.x$indirect_med_costs),
                                      discounted = .x$discounted,
                                      cost_category = "indirect medical"
                                    ))) |> 
      tidyr::unnest(indirect_medical)
    
    ret <- bind_rows(ret, indirect_medical)
    
  }
  
  if(summarise == TRUE & simplify == TRUE){
    stop("Not summarise AND simplify can be set to TRUE")
  }
  
  if(summarise == TRUE) {
    ret <- extract_iterations_helper(ret, n_iter) |> 
      group_by(strategy, year, month, age_cat, age, risk_group, population, vaccine_status, date, season, cost_category, item, discounted, perspective_hc, perspective_soc) |> 
      summarise(mean = mean(value),
                median = stats::median(value),
                cri_low = stats::quantile(value, .025),
                cri_high = stats::quantile(value, .975),
                .groups = "drop")
  } else if(simplify == TRUE){
    ret <- extract_iterations_helper(ret, n_iter) |> 
      group_by(strategy, year, month, age_cat, age, risk_group, iter, population, vaccine_status, date, season, discounted, perspective_hc, perspective_soc) |> 
      summarise(value = sum(value),
                .groups = "drop")
  } else {
    ret <- extract_iterations_helper(ret, n_iter)
  }
  
  return(ret)
}

#' Extract function for events
#'
#' @param model_run model_run object
#' @param n_iter number of iterations
#' @param settings settings
#' @param summarise setting whether to summarise results, TRUE or FALSE, output is not per iteration
#' @param simplify setting whether to simplify results, TRUE or FALSE, output is per iteration
#'
#' @returns
#' @export
#'
#' @examples
extract_health <- function(model_run, n_iter, settings, summarise = TRUE, simplify = FALSE) {
  
  #morbidity
  qalys <- extract_core_helper(model_run, "health") |> 
    tidyr::unnest(health) |> 
    select(strategy, year, month, age_cat, age, risk_group, population, vaccine_status, date, season,
           item = event, value = total_qaly_loss) |>
    mutate(discounted = FALSE,
           category = "qaly")
  
  #mortality
  mort_hosp <- extract_core_helper(model_run, "mortality_hosp") |>
    tidyr::unnest(mortality_hosp) 
  
  mort_nursing <- extract_core_helper(model_run, "mortality_nursing") |>
    tidyr::unnest(mortality_nursing) 
  
  mort <- bind_rows(mort_hosp, mort_nursing) |> 
    tidyr::pivot_longer(cols = c("le", "disc_le", "qale", "disc_qale"), names_to = "category") |> 
    rename(item = event) |> 
    select(-number) |> 
    mutate(discounted = if_else(stringr::str_sub(category, 1, 2) == "di", TRUE, FALSE),
           category = as.factor(if_else(stringr::str_detect(category, "qale"), "qale", "le")))
    
  
  ret <- bind_rows(qalys, mort)
  
  
  if(summarise == TRUE & simplify == TRUE){
    stop("Not summarise AND simplify can be set to TRUE")
  }

  if(summarise == TRUE) {
    ret <- extract_iterations_helper(ret, n_iter) |> 
      group_by(strategy, year, month, age_cat, age, risk_group, population, vaccine_status, date, season, item, discounted, category) |> 
      summarise(mean = mean(value),
                median = stats::median(value),
                cri_low = stats::quantile(value, .025),
                cri_high = stats::quantile(value, .975),
                .groups = "drop")
  } else if(simplify == TRUE){
    ret <- extract_iterations_helper(ret, n_iter) |> 
      group_by(strategy, year, month, age_cat, age, risk_group, iter, population, vaccine_status, date, season, discounted, category) |> 
      summarise(value = sum(value),
                .groups = "drop")
  } else {
    ret <- extract_iterations_helper(ret, n_iter)
  }
  
  return(ret)
}

#' Add incremental results
#'
#' @param extracted_data extracted data from a non-summarised extract_ function
#' @param reference_name name of reference
#' @param intervention_name name of intervention
#'
#' @returns
#' @export
#'
#' @examples
add_incremental <- function(extracted_data, reference_name = "no_vaccine", intervention_name = "rsv_generic_vaccine") {
  #return warning if extracted_data is summarised
  if((is.null(extracted_data$value))) {
    stop("extracted_data should not be summarised, change summarise to FALSE")
  }
  
  
  ## get all column names, except for the current strategy
  join_columns <- colnames(select(extracted_data, -strategy, -value, -population, -vaccine_status))
  join_columns_syms <- rlang::syms(join_columns)
  
  
  res_reference <- extracted_data |> 
    filter(strategy == reference_name) |> 
    rename(reference = value) |> 
    group_by(!!!join_columns_syms) |> 
    summarise(reference = sum(reference),
              reference_name = reference_name,
              .groups = "drop")
  
  res_intervention <- extracted_data |> 
    filter(strategy == intervention_name) |> 
    rename(intervention = value) |> 
    group_by(!!!join_columns_syms) |> 
    summarise(intervention = sum(intervention),
              intervention_name = intervention_name,
              .groups = "drop")
  
  combined <- full_join(res_reference, res_intervention, by = join_columns) |> 
    mutate( value = intervention - reference)
  
  return(combined)
}


#' Extract cost-effectiveness results
#'
#' @param model_run model_run object
#' @param n_iter number of iterations
#' @param settings settings
#' @param reference_name name of reference
#' @param intervention_name name of intervention
#' @param perspective "healthcare" or "societal"
#' @param discount TRUE or FALSE, whether to apply discounting to the results
#' @param summarise setting whether to summarise results, TRUE or FALSE, output is not per iteration
#'
#' @returns
#' @export
#'
#' @examples
extract_cost_effectiveness <- function(model_run, 
                                       n_iter, 
                                       settings, 
                                       reference_name = "no_vaccine", 
                                       intervention_name = "rsv_generic_vaccine",
                                       perspective = "societal", 
                                       discount = TRUE,
                                       summarise = FALSE){
  
  qalys_total <- extract_health(
    model_run = model_run,
    n_iter = n_iter,
    settings = settings,
    summarise = FALSE,
    simplify = TRUE
  ) |> 
    filter(category == "qaly" |
             (category == "qale" & discounted == TRUE))
  
  qalys_morbidity <- qalys_total |> 
    filter(category == "qaly", 
           discounted == FALSE)
  
  qalys_mortality <- qalys_total |> 
    filter(category == "qale",
           discounted == TRUE)
  
  costs <- extract_costs(
    model_run = model_run,
    n_iter = n_iter,
    settings = settings,
    summarise = FALSE,
    simplify = TRUE
  )
  
  if(discount == TRUE){
    start_date <- stringr::str_c(settings$start_year, "-",settings$start_month,"-","01")
    
    qalys_morbidity <- qalys_morbidity |> 
      discounting$discount_table(ref_date = start_date,
                                 col_value = "value",
                                 rate = settings$discount_rate$health)
    
    costs <- costs |> 
      discounting$discount_table(ref_date = start_date,
                                 col_value = "value",
                                 rate = settings$discount_rate$economic)
  }
  
  
  if(perspective == "societal"){
    costs <- costs |> 
      filter(perspective_soc == TRUE)
  } else if(perspective == "healthcare"){
    costs <- costs |> 
      filter(perspective_hc == TRUE)
  }
  
  qalys_inc <- bind_rows(qalys_morbidity, qalys_mortality) |> 
    add_incremental(reference_name, intervention_name) |> 
    group_by(iter) |> 
    summarise(reference_qalys = sum(reference),
              intervention_qalys = sum(intervention),
              incremental_qalys = sum(value) * -1)
  
  costs_inc <- costs |> 
    add_incremental(reference_name, intervention_name) |> 
    group_by(iter) |> 
    summarise(reference_costs = sum(reference),
              intervention_costs = sum(intervention),
              incremental_costs = sum(value))
  
  
  
  ce <- left_join(costs_inc, qalys_inc, by = "iter")
  
  if(summarise == TRUE) {
    ret <- ce |> 
      select(-iter) |> 
      summarise(across(.cols = everything(), list(mean = ~mean(.), 
                                                  median = ~stats::median(.),
                                                  cri_low = ~stats::quantile(., .025),
                                                  cri_high = ~stats::quantile(., .975))),
                icer = incremental_costs_mean / incremental_qalys_mean)
  }  else {
    ret <- ce
  }
  
  return(ret)
  
}


# Analyze functions -------------------------------------------------------




#' Calculate incremental results
#'
#' @param results rsv model results, unnested
#' @param reference_name name of reference
#' @param intervention_name name of intervention to be evaluated
#' @param return_population option to also return the size of the population, this is excluded by default
#' @param monthly TRUE of FALSE, depending on whether monthly data needs to be included
#' @param included_categories categories to be included in the resulting data, default is costs, life years, and qalys
#'
#' @return
#' @export
#'
#' @examples
calc_incremental <- function(results, reference_name, intervention_name, monthly = FALSE, included_categories = c("costs", "life years", "qalys"), return_population = FALSE){
  ## filter results to only include requested categories
  results <- filter(results, category %in% included_categories)
  
  ## if monthly == FALSE, summarise data to be annual data
  if(monthly == FALSE){
    results <- results |> 
      group_by(strategy, season, age_cat, age, population, category, item, iter, discounted, cost_category, perspective_hc, perspective_soc) |> 
      summarise(value = sum(value),
                .groups = "drop")
  }
  ## get all column names, except for the current strategy
  join_columns <- colnames(select(results, -strategy, -value, -population))
  join_columns_syms <- rlang::syms(join_columns)

  
  res_reference <- results |> 
    filter(strategy == reference_name) |> 
    rename(reference = value) |> 
    group_by(!!!join_columns_syms)
  
  res_intervention <- results |> 
    filter(strategy == intervention_name) |> 
    rename(intervention = value) |> 
    group_by(!!!join_columns_syms)
  
  if(return_population == TRUE){
    res_reference <- res_reference |> 
      summarise(reference = sum(reference),
                population_ref = sum(population),
                reference_name = reference_name,
                .groups = "drop") 
    
    
    res_intervention <- res_intervention |> 
      summarise(intervention = sum(intervention),
                population_int = sum(population),
                intervention_name = intervention_name,
                .groups = "drop")
  } else if(return_population == FALSE){
    res_reference <- res_reference |> 
      summarise(reference = sum(reference),
                reference_name = reference_name,
                .groups = "drop") 
    
    
    res_intervention <- res_intervention |> 
      summarise(intervention = sum(intervention),
                intervention_name = intervention_name,
                .groups = "drop")
  } else {
    return(print("return population should be either true or false"))
  }
  
  val_reference <- reference_name
  val_intervention <- intervention_name
  
  
  combined <- full_join(res_reference, res_intervention, by = join_columns) |> 
    mutate(reference = if_else(is.na(reference), 0, reference),
           intervention = if_else(is.na(intervention), 0, intervention),
           reference_name = val_reference,
           intervention_name = val_intervention,
          value = intervention - reference,
           date = as.Date(stringr::str_c(stringr::str_sub(season, 8, 11), "-01-01")))
  
  return(combined)
} 

#' Create CE table
#'
#' @param extracted_cost_effectiveness results from extract_cost_effectiveness()
#' @param settings settings
#'
#' @returns
#' @export
#'
create_cetable <- function(extracted_cost_effectiveness, settings){
  extracted_cost_effectiveness |> 
    mutate(rowname = case_when(
      category == "costs" ~ "Costs",
      category == "qalys" ~ "QALYs lost",
      category == "life years" ~ "Life years lost"
    )) |> 
    select(rowname, value_reference, value_intervention, value_incremental, icer) |> 
    gt::gt(rowname_col = "rowname") |> 
    gt::cols_label(
      value_reference = name_reference,
      value_intervention = name_intervention,
      value_incremental = "Incremental",
      icer = "ICER"
    ) |> 
    gt::fmt_currency(rows = 1, currency = settings$currency, decimals = 0) |> 
    gt::fmt_currency(columns = 5, currency = settings$currency, decimals = 0) |> 
    gt::fmt_number(rows = c(2,3), columns = 4, decimals = 0) |> 
    gt::fmt_number(rows = c(2,3), columns = c(2,3), scale_by = -1, decimals = 0) |> 
    gt::sub_missing(columns = 5, missing_text = " ") |> 
    gt::tab_source_note("ICER: incremental cost effectiveness ratio; QALY: quality adjusted life year")
}

#' Calculate cost effectiveness
#'
#' @param incremental_results results from calc_incremental()
#' @param perspective string with societal or healthcare
#' @param discount TRUE/FALSE
#' @param settings settings
#'
#' @return
#' @export
#'
#' @examples
calc_costeffectiveness <- function(incremental_results, 
                                   perspective = "societal", 
                                   discount = TRUE, 
                                   settings,
                                   nice_table = FALSE){
  n_iter <- max(incremental_results$iter)
  
  res_qaly_morbidity <- incremental_results |>
    filter(category %in% c("qalys"),
           item != "mortality (hospital)",
           item != "mortality (nursing homes)",
           item != "hospitalization (ICU)", #to prevent double counting
           item != "hospitalization (non-ICU)") #to prevent double counting
  
  res_mortality <- incremental_results |>
    filter(category %in% c("qalys", "life years"),
           item %in% c("mortality (hospital)", 
                       "mortality (nursing homes)", 
                       "discounted life years lost (hospital)", 
                       "life years lost (hospital)",
                       "discounted life years lost (nursing homes)", 
                       "life years lost (nursing homes)"),
           discounted == discount)
  
  if(discount == TRUE){
    start_date <- stringr::str_c(settings$start_year, "-",settings$start_month,"-","01")
    res_costs <- incremental_results |> 
      filter(category == "costs") |>  
      discounting$discount_table(ref_date = start_date,
                                 col_value = "reference",
                                 rate = settings$discount_rate$economic) |> 
      discounting$discount_table(ref_date = start_date,
                                 col_value = "intervention",
                                 rate = settings$discount_rate$economic) |> 
      discounting$discount_table(ref_date = start_date,
                                 col_value = "value",
                                 rate = settings$discount_rate$economic)
    
    res_qaly_morbidity <- res_qaly_morbidity |>  
      discounting$discount_table(ref_date = start_date,
                                 col_value = "reference",
                                 rate = settings$discount_rate$health) |> 
      discounting$discount_table(ref_date = start_date,
                                 col_value = "intervention",
                                 rate = settings$discount_rate$health) |> 
      discounting$discount_table(ref_date = start_date,
                                 col_value = "value",
                                 rate = settings$discount_rate$health)

  } else if(discount == FALSE){
    res_costs <- incremental_results |> 
      filter(category == "costs")
  }
  
  if(perspective == "societal"){
    res_costs <- res_costs |> 
      filter(perspective_soc == TRUE)
  } else if(perspective == "healthcare"){
    res_costs <- res_costs |> 
      filter(perspective_hc == TRUE)
  }
  
  res_costs <- res_costs |> 
    summarise(category = "costs",
              value_reference = sum(reference) / n_iter,
              name_reference = first(reference_name),
              value_intervention = sum(intervention) / n_iter,
              name_intervention = first(intervention_name),
              value_incremental = sum(value) / n_iter)
  
  res_mortality <- res_mortality |> 
    group_by(category) |> 
    summarise(value_reference = -1*sum(reference) / n_iter,
              name_reference = first(reference_name),
              value_intervention = -1*sum(intervention) / n_iter,
              name_intervention = first(intervention_name),
              value_incremental = -1*sum(value) / n_iter)
  
  res_qaly_morbidity <- res_qaly_morbidity |> 
    summarise(category = "qalys",
              value_reference = -1*sum(reference) / n_iter,
              name_reference = first(reference_name),
              value_intervention = -1*sum(intervention) / n_iter,
              name_intervention = first(intervention_name),
              value_incremental = -1*sum(value) / n_iter)
  
  res_total <- bind_rows(
    res_costs,
    res_mortality,
    res_qaly_morbidity
  ) |> 
    group_by(category, name_reference, name_intervention) |> 
    summarise(value_reference = sum(value_reference),
              value_intervention = sum(value_intervention),
              value_incremental = sum(value_incremental),
              .groups = "drop")
  
  inc_costs <- filter(res_total, category == "costs") |> pull(value_incremental)
  inc_lys <- filter(res_total, category == "life years") |> pull(value_incremental)
  inc_qalys <- filter(res_total, category == "qalys") |> pull(value_incremental)
  
  ret_tibble <- res_total |> 
    mutate(icer = c(NA, inc_costs / inc_lys, inc_costs / inc_qalys))
  
  if(nice_table == FALSE){
    return(ret_tibble)
  } else if(nice_table == TRUE){
    name_reference <- settings$included_strategies[[ret_tibble$name_reference[1]]]["nice_name"]
    name_intervention <- settings$included_strategies[[ret_tibble$name_intervention[1]]]["nice_name"]
    
    ret_gt <- ret_tibble |> 
      mutate(rowname = case_when(
        category == "costs" ~ "Costs",
        category == "qalys" ~ "QALYs lost",
        category == "life years" ~ "Life years lost"
      )) |> 
      select(rowname, value_reference, value_intervention, value_incremental, icer) |> 
      gt::gt(rowname_col = "rowname") |> 
      gt::cols_label(
        value_reference = name_reference,
        value_intervention = name_intervention,
        value_incremental = "Incremental",
        icer = "ICER"
      ) |> 
      gt::fmt_currency(rows = 1, currency = settings$currency, decimals = 0) |> 
      gt::fmt_currency(columns = 5, currency = settings$currency, decimals = 0) |> 
      gt::fmt_number(rows = c(2,3), columns = 4, decimals = 0) |> 
      gt::fmt_number(rows = c(2,3), columns = c(2,3), scale_by = -1, decimals = 0) |> 
      gt::sub_missing(columns = 5, missing_text = " ") |> 
      gt::tab_source_note("ICER: incremental cost effectiveness ratio; QALY: quality adjusted life year")
      
    return(ret_gt)

  }
  
}


#' Function to calculate cost-effective price
#'
#' @param incremental_results incremental results created with calc_incremental()
#' @param perpsective perspective, default is societal
#' @param wtp wtp threshold
#' @param settings settings list
#'
#' @return
#' @export
#'
#' @examples
calc_price <- function(incremental_results,
                       perspective = "societal",
                       wtp = 50000,
                       settings){
  
  vaccine_name <- incremental_results$intervention_name[[1]]
  vaccine_price <- settings$included_strategies[[vaccine_name]] |> pluck("price")
  

  ref_date <- lubridate::as_date(stringr::str_c(settings$start_year, "-", settings$start_month,"-1"))
  incremental_undiscounted <- incremental_results |> 
    filter(category == "costs",
           item == "vaccine",
           value != 0) |> 
    select(season, age, iter, date, value)
  
  scenario_vacprice_costs_vac <- incremental_undiscounted |>
    mutate(value_var = map(value, ~tibble(
      vac_price = (0:2000/500) * vaccine_price,
      vac_costs = .x * (0:2000/500)))) |> 
    tidyr::unnest(value_var) |> 
    discounting$discount_table(ref_date = ref_date,
                               rate = settings$discount_rate$economic) |> 
    summarise(vac_costs = sum(vac_costs), .by = c("iter", "vac_price")) |> 
    summarise(vac_costs = mean(vac_costs), .by = c("vac_price"))
  
  if(perspective == "healthcare"){
    scenario_vacprice_costs_other <- incremental_results |> 
      filter(perspective_hc == TRUE,
             category == "costs",
             item != "vaccine" | value == 0)
  } else if(perspective == "societal"){
    scenario_vacprice_costs_other <- incremental_results |> 
      filter(perspective_soc == TRUE,
             category == "costs",
             item != "vaccine" | value == 0)
  }
  
  
  scenario_vacprice_costs_other <- scenario_vacprice_costs_other |>
    discounting$discount_table(ref_date = ref_date,
                               rate = settings$discount_rate$economic) |> 
    summarise(other_costs = sum(value), .by = c("iter")) |> 
    summarise(other_costs = mean(other_costs)) |> 
    pull(other_costs)
  
  
  utilities <- calc_costeffectiveness(incremental_results, 
                         perspective = perspective, 
                         discount = TRUE, 
                         settings,
                         nice_table = FALSE) |> 
    filter(category == "qalys") |> 
    pull("value_incremental")
  
  scenario_vacprice <- scenario_vacprice_costs_vac |> 
    mutate( costs = vac_costs + scenario_vacprice_costs_other,
            qalys = utilities,
            icer = costs / qalys) |> 
    filter(icer <= wtp) |> 
    slice_tail(n = 1) |> 
    pull(vac_price)
  
  #create tibble with results per iteration
  incremental_results_vaccosts <- incremental_results |> 
    filter(category == "costs",
           item == "vaccine",
           value != 0) |> 
    mutate(value = value * (scenario_vacprice/vaccine_price),
           intervention = intervention * (scenario_vacprice/vaccine_price))
  
  incremental_results_other <- incremental_results |> 
    filter(!(category == "costs" &
           item == "vaccine" &
           value != 0))
  
  new_incremental <- bind_rows(
    incremental_results_vaccosts,
    incremental_results_other
  )
    
  
  return(list(
    price = scenario_vacprice,
    new_incremental = new_incremental
  ))
  
}


# CEAC functions ----------------------------------------------------------

#' Create CEAC
#'
#' @param incremental_results results from calc_incremental()
#' @param perspective string with societal or healthcare
#' @param discount TRUE/FALSE
#' @param settings settings
#'
#' @return
#' @export
#'
#' @examples
create_ceac <- function(incremental_results, 
                        perspective = "societal", 
                        discount = TRUE, 
                        settings,
                        nice_table = FALSE){
  
  ceac_data <- incremental_results |> 
    tidyr::nest(.by = iter) |> 
    ungroup() |> 
    mutate(ce = map(data,
                    function(x){
                      ce <- calc_costeffectiveness(
                        mutate(x, iter = 1),
                        perspective = perspective,
                        discount = discount,
                        settings = settings,
                        nice_table = FALSE
                      )
                      qalys <- filter(ce, category == "qalys") |> pull(value_incremental)
                      costs <- filter(ce, category == "costs") |> pull(value_incremental)
                      
                      return(list(
                        qalys = qalys,
                        costs = costs
                      ))
                    })) |> 
    tidyr::hoist(.col = ce, qalys = "qalys", costs = "costs") |> 
    mutate(nmb = map2(costs, qalys, ~tibble(wtp = 100 * (0:1500), nmb = (100 * (0:1500) * .y) - .x))) |> 
    tidyr::unnest(nmb) |> 
    mutate(ce = if_else(nmb >=0, 1, 0)) |> 
    group_by(wtp) |> 
    summarise(ce = sum(ce),
              n = n(),
              prob = ce / n)
  
  ceac_data |> 
    ggplot2::ggplot(ggplot2::aes(x = wtp, y = prob)) + 
    ggplot2::geom_line() +
    ggplot2::scale_y_continuous(name = "Probability RSV vaccination is cost effective", limits = c(0,1), labels = scales::label_percent()) +
    ggplot2::scale_x_continuous(name = "Willingness to pay", labels = scales::label_currency(prefix = "€")) +
    ggplot2::theme_minimal()
  
}

#' Create CE plane
#'
#' @param incremental_results results from calc_incremental()
#' @param perspective string with societal or healthcare
#' @param discount TRUE/FALSE
#' @param settings settings
#'
#' @returns
#' @export
#'
#' @examples
create_ce_plane <- function(incremental_results, 
                        perspective = "societal", 
                        discount = TRUE, 
                        settings){
  
  plane_data <- incremental_results |> 
    tidyr::nest(.by = iter) |> 
    mutate(ce = map(data,
                    function(x){
                      ce <- calc_costeffectiveness(
                        mutate(x, iter = 1),
                        perspective = perspective,
                        discount = discount,
                        settings = settings,
                        nice_table = FALSE
                      )
                      qalys <- filter(ce, category == "qalys") |> pull(value_incremental)
                      costs <- filter(ce, category == "costs") |> pull(value_incremental)
                      
                      return(list(
                        qalys = qalys,
                        costs = costs
                      ))
                    })) |> 
    tidyr::hoist(.col = ce, qalys = "qalys", costs = "costs") |> 
    select(iter, costs, qalys)
  
  plane_data |> 
    ggplot2::ggplot(ggplot2::aes(x = costs, y = qalys)) +
    ggplot2::geom_point() +
    ggplot2::scale_y_continuous(name = "Incremental QALYs", limits = c(0, NA)) +
    ggplot2::scale_x_continuous(name = "Incremental costs", labels = scales::label_currency(prefix = "€"), limits = c(0, NA))
  
}


# DSA functions -----------------------------------------------------------



#' Prepare DSA dataset
#'
#' @param settings settings
#' @param variation variation to use in the DSA
#'
#' @returns
#' @export
#'
#' @examples
prep_dsa <- function(settings,  variation = .2) {
  ## Load data ---------------------------------------------------------------
  rsv_dat <- load_data(settings, probabilistic = FALSE, iter = 1)
  demographic_interval <- "year"
  pop_start <- demographics$create(settings, time_interval = demographic_interval) |>  
    demographics$add_nursinghomes() |> 
    demographics$add_highrisk(settings)
  
  population <- bind_rows(
    vaccine$add(pop_start, "no_vaccine", settings$vaccine_coverage, settings),
    vaccine$add(pop_start, "rsv_generic_vaccine", settings$vaccine_coverage, settings)
  ) |> 
    mutate(age_cat = case_when(
      age < 60 ~ "50-59",
      age < 70 ~ "60-69",
      age < 80 ~ "70-79",
      age >= 80 ~ "80+"
    ),
    date = case_when(
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
  
  
  ## Prep data ---------------------------------------------------------------
  rsv_prep <- prep_data(rsv_dat, settings, probabilistic = FALSE, iter = 1)
  
  if(is.double(variation)){
    rsv_prep_prob <- NA
    v_offset <- c(1-variation, 1+variation)
    c_variation <- stringr::str_c(variation*100, "% increase and decrease")
    vaccine_eff_var <- tidyr::expand_grid(
      param = c("out_upper", "out_lower", "hosp", "non_medical"),
      offset = c(1-variation, 1+variation),
      variation = c_variation
    )
  } else if(variation == "CrI") {
    rsv_prep_prob <- prep_data(rsv_dat, settings, probabilistic = TRUE, iter = 1000)
    v_offset <- c("low", "high")
    c_variation <- stringr::str_c("CrI")
    
    vaccine_effectiveness_offset <-
      load_data(settings, probabilistic = TRUE, iter = 1000)$vaccines$rsv_generic_vaccine
    
    vaccine_eff_var <- 
      vaccine_effectiveness_offset$effectiveness |> 
      mutate(effectiveness_mean = map_dbl(effectiveness, mean),
             effectiveness_high = map_dbl(effectiveness, ~stats::quantile(.x, .975)),
             effectiveness_low = map_dbl(effectiveness, ~stats::quantile(.x, .025)),
             offset_high = effectiveness_high / effectiveness_mean,
             offset_low = effectiveness_low / effectiveness_mean) |> 
      group_by(outcome) |> 
      summarise(offset_high = mean(offset_high),
                offset_low = mean(offset_low)) |> 
      filter(!(is.na(offset_high))) |> 
      tidyr::pivot_longer(starts_with("offset"), names_prefix = "offset_") |> 
      rename(param = outcome,
             offset = value) |> 
      select(-name) |> 
      mutate(variation = c_variation)
    
    variation <- .2

  } else {
    stop("variation is not correct")
  }
  
  
  ## Get base case
  dsa_basecase <- tibble(
    category = "base_case",
    param = "base_case",
    offset = as.character(1),
    data = list(rsv_prep),
    variation = "base case"
  )
  
  ## Create scenarios
  ### Probabilities
  dsa_probs_params <- rsv_prep$probs[[1]] |>  pull(param)
  dsa_probs <- tidyr::expand_grid(
    category = "probs",
    param = dsa_probs_params,
    offset = v_offset,
    variation = c_variation
  ) |> 
    mutate(data = pmap(list(param, offset), calc_dsa_probs, rsv_prep, rsv_prep_prob),
           offset = as.character(offset))
  
  
  ### Costs
  dsa_cost_params <- rsv_prep$costs[[1]] |>  pull(cost_item) |>  unique()
  
  dsa_costs_det <- tidyr::expand_grid(
    category = "costs",
    param = dsa_cost_params,
    offset = c(1-variation, 1+variation),
    variation = stringr::str_c(variation*100, "% increase and decrease")
  ) |> 
    filter(param %in% (rsv_dat$costs |> filter(distribution == "none") |> pull("cost_item"))) |> 
    mutate(data = pmap(list(param, offset), calc_dsa_costs, rsv_prep, NA),
           offset = as.character(offset))
  
  dsa_costs_prob <- tidyr::expand_grid(
    category = "costs",
    param = dsa_cost_params,
    offset = v_offset,
    variation = c_variation
  ) |> 
    filter(!(param %in% (rsv_dat$costs |> filter(distribution == "none") |> pull("cost_item")))) |> 
    mutate(data = pmap(list(param, offset), calc_dsa_costs, rsv_prep, rsv_prep_prob),
           offset = as.character(offset))
  
  dsa_costs <- bind_rows(dsa_costs_det, dsa_costs_prob) 
  
  ### Outcomes
  ### filter out adverse events as bug fixes
  dsa_outc_params <- rsv_prep$outcomes[[1]] |>  
    #filter(stringr::str_detect(item, "adverse event", negate = TRUE)) |> 
    pull(item)
  
  outc_det <- rsv_dat$outcomes |> filter(distribution == "none") |> pull(item)
  outc_prob <- rsv_dat$outcomes |> filter(distribution != "none") |> pull(item)
  dsa_outc_det <- rsv_prep$outcomes[[1]] |> 
    select(outcome_cat = category, param = item) |> 
    filter(param %in% outc_det) |> 
    mutate(offset = list(c(1-variation, 1+variation)),
           category = "outcomes") |> 
    tidyr::unnest(offset) |> 
    mutate(data = pmap(list(param, outcome_cat, offset), calc_dsa_outc, rsv_prep, NA),
           param = stringr::str_c(outcome_cat, " | ", param),
           offset = as.character(offset),
           variation = stringr::str_c(variation*100, "% increase and decrease")) |> 
    select(-outcome_cat)
  
  dsa_outc_prob <- rsv_prep$outcomes[[1]] |> 
    select(outcome_cat = category, param = item) |> 
    filter(!(param %in% outc_det)) |> 
    mutate(offset = list(v_offset),
           category = "outcomes") |> 
    tidyr::unnest(offset) |> 
    mutate(data = pmap(list(param, outcome_cat, offset), calc_dsa_outc, rsv_prep, rsv_prep_prob),
           param = stringr::str_c(outcome_cat, " | ", param),
           offset = as.character(offset),
           variation = c_variation) |> 
    select(-outcome_cat)
  
  
  dsa_outc <- bind_rows(dsa_outc_det, dsa_outc_prob)
  
  # vaccine effectiveness
  dsa_vaccine <- vaccine_eff_var |> 
    mutate(category = "vaccine_effectiveness",
           rsv_dat = list(rsv_dat),
           rsv_dat = pmap(list(rsv_dat, param, offset), function(rsv_dat, param, offset){
             effectiveness_data <- rsv_dat$vaccines$rsv_generic_vaccine$effectiveness 
             
             effectiveness_change <- effectiveness_data |> 
               filter(outcome == param) |> 
               mutate(effectiveness = map(effectiveness, ~.x * offset))
             
             rsv_dat$vaccines$rsv_generic_vaccine$effectiveness  <- effectiveness_data |> 
               filter(outcome != param) |> 
               add_row(effectiveness_change)
             
             
             return(rsv_dat)
             
           }),
           data = list(rsv_prep),
           offset = as.character(offset))
  
  

  ### Productivity
  dsa_prod_params <- rsv_prep$productivity[[1]] |> colnames()
  
  dsa_prod <- tidyr::expand_grid(
    category = "productivity",
    param = dsa_prod_params,
    offset = c(1-variation, 1+variation),
    variation = stringr::str_c(variation*100, "% increase and decrease")) |> 
    mutate(data = pmap(list(param, offset), calc_dsa_productivity, rsv_prep),
           offset = as.character(offset)) 
  
  dsa_fulldata <- bind_rows(
    dsa_basecase,
    dsa_probs,
    dsa_costs,
    dsa_outc,
    dsa_prod
  ) |> 
    mutate(rsv_dat = list(rsv_dat)) |> 
    bind_rows(dsa_vaccine) |> 
    mutate(
      population = list(population),
      pop_start = list(pop_start)
    )
  
  return(dsa_fulldata)
           
}
  
  

#' Title
#'
#' @param prepped_dsa output from prep_dsa()
#' @param settings settings
#' @param n_cores number of cores
#'
#' @returns
#' @export
#'
#' @examples
run_dsa <- function(prepped_dsa, settings, n_cores = 1){
  probabilistic <- FALSE
  iter <- 1
  
  plan(multisession, workers = n_cores)
  furrr_opts <- furrr_options(
    globals = FALSE,
    packages = NULL,
    seed = TRUE
  )
  # calc_dsa <- prepped_dsa |> 
  #   ungroup() |> 
  #   mutate(
  #     model_run = pmap(list(data, population, pop_start, rsv_dat), function(rsv_prep, population, pop_start, rsv_dat, probabilistic, settings){
  #       ce_result <- population |> 
  #         left_join(select(rsv_prep, age, probs, costs), by = "age") |> 
  #         mutate( costs = map2(costs, strategy, apply_strategy_costs, settings),
  #                 probs = pmap(list(risk_group, probs, age, month, year), apply_riskstratification, pop_start),
  #                 vac_effect = pmap(list(month, year, strategy, vaccine_status), add_vaccine_effect, rsv_dat), 
  #                 tree = pmap(list(population, probs, vac_effect, prop_nursing), run_tree, settings),
  #                 results = future_pmap(list(month, year, age, tree, costs, strategy, vaccine_status, population, probs), calc_results, rsv_dat, rsv_prep, probabilistic, iter, settings,
  #                                       .options = furrr_opts)) |> 
  #         extract_cost_effectiveness(n_iter = iter, settings = settings, discount = TRUE, summarise = TRUE)
  #       
  #       
  # 
  #       return(tibble(
  #         icer = ce_result$icer,
  #         incremental_costs = ce_result$incremental_costs_mean,
  #         incremental_qalys = ce_result$incremental_qalys_mean
  #       ))
  #       
  #     }, 
  #     probabilistic, settings)
  #   ) 
  
  calc_dsa <- prepped_dsa |> 
    ungroup() |> 
    mutate(
      model_run = future_pmap(list(data, population, pop_start, rsv_dat), 
                               function(rsv_prep, population, pop_start, rsv_dat, probabilistic, settings){
        ce_result <- population |> 
          left_join(select(rsv_prep, age, probs, costs), by = "age") |> 
          mutate( costs = map2(costs, strategy, apply_strategy_costs, settings),
                  probs = pmap(list(risk_group, probs, age, month, year), apply_riskstratification, pop_start),
                  vac_effect = pmap(list(month, year, strategy, vaccine_status), add_vaccine_effect, rsv_dat), 
                  tree = pmap(list(population, probs, vac_effect, prop_nursing), run_tree, settings),
                  results = pmap(list(month, year, age, tree, costs, strategy, vaccine_status, population, probs), calc_results, rsv_dat, rsv_prep, probabilistic, iter, settings)) |> 
          extract_cost_effectiveness(n_iter = iter, settings = settings, discount = TRUE, summarise = TRUE)
        
        
        
        return(tibble(
          icer = ce_result$icer,
          incremental_costs = ce_result$incremental_costs_mean,
          incremental_qalys = ce_result$incremental_qalys_mean
        ))
        
      }, 
      probabilistic, settings,
      .options = furrr_opts)
    ) 
  
  plan(sequential)
  
  ret <- calc_dsa |> 
    tidyr::unnest(model_run) |> 
    mutate(
      direction = case_when(
        offset == "low" ~ "Low value",
        offset == "high" ~ "High value",
        offset < 1 ~ "Low value",
        offset > 1 ~ "High value",
        offset == 1 & category == "base_case" ~ "Base case",
        offset == 1 ~ "ERROR, OFFSET SHOULD NOT BE 1"
      )
    ) |> 
    select(
      category, param, direction, variation,  offset, icer, incremental_costs, incremental_qalys
    )
  
  
  
  return(ret)
  
  
}

#' Create Tornado diagram of DSA
#'
#' @param dsa_data data generated using run_dsa()
#' @param settings settings
#' @param parameter can be either: "icer", "incremental_costs" or "incremental_qalys"
#' @param cutoff cutoff value (integer), to select only the top impactfull parameters
#'
#' @returns
#' @export
#'
#' @examples
create_tornado <- function(dsa_data, settings, parameter = "icer", cutoff = NA){
  base_case_value <- dsa_data |> 
    filter(direction == "Base case") |> 
    pull(parameter)
  
  dsa_tidy <- dsa_data |> 
    filter(direction != "Base case") |> 
    select(category, param, direction, variation, value = all_of(parameter)) |> 
    mutate( # category = case_when(
           #   category == "probs" ~ "Probabilities",
           #   category == "outcomes" ~ "Outcomes",
           #   category == "costs" ~ "Costs",
           #   category == "vaccine effectiveness" ~ "Vaccine effectiveness"
           # ),
           param = stringr::str_c(category, " | ", param),
           param = case_when(
             variation == "CrI" ~ stringr::str_c(param, "*"),
             variation == "20% increase and decrease" ~ stringr::str_c(param, "^"),
             .default = stringr::str_c(param, " - ", variation)
           ),
           param = as.factor(param))
  
  importance <- dsa_tidy |> 
    mutate(diff = value - base_case_value) |> 
    select(category, param, direction, diff) |> 
    tidyr::pivot_wider(names_from = "direction",
                       values_from = diff) |> 
    mutate(delta = abs(`High value` - `Low value`)) |> 
    arrange(-delta) |> 
    mutate(location = -1 * row_number()) |> 
    select(param, location)
  
  if(!(is.na(cutoff))){
    selected_params <- importance |> 
      slice(1:cutoff) |> 
      pull(param)
  } else{
    selected_params <- importance |> 
      pull(param)
  }
  
  
  
  dsa_tidy |> 
    filter(param %in% selected_params) |> 
    left_join(importance, by = "param") |> 
    mutate(valuemin = case_when(
      value < base_case_value ~  value,
      value > base_case_value ~ base_case_value,
      value == base_case_value ~  value
    ),
    valuemax = case_when(
      value < base_case_value ~  base_case_value,
      value > base_case_value ~ value,
      value == base_case_value ~  value
    )) |> 
    ggplot2::ggplot() +
    ggplot2::geom_rect(ggplot2::aes(xmin = valuemin, xmax = valuemax, ymin = location-.4, ymax = location+.4, fill = direction)) +
    ggplot2::geom_vline(xintercept = base_case_value) +
    ggplot2::scale_y_continuous(breaks = importance$location, labels = importance$param) +
    ggplot2::scale_x_continuous(name = "Incremental cost-effectiveness ratio (ICER)", labels = scales::label_currency(prefix = "€")) +
    ggplot2::xlab(parameter)
    
}

#' Helper function to generate DSA parameters for probabilities
#'
#' @param sel_param specific selected parameter name
#' @param offset offset, or factor to multiply the parameter by
#' @param prepped_data prepped data set from prep_data()
#'
#' @returns
#'
calc_dsa_probs <- function(sel_param, offset, prepped_data, prepped_data_prob = NA){
  sel_data <- prepped_data$probs

  if(is.double(offset)){
    ret <- map(sel_data, function(x, sel_param, offset){
      out <- x |> 
        mutate(prob = case_when(
          param == sel_param ~ map(prob, ~.x * offset),
          .default = prob
        ))
      return(out)
      
    }, sel_param, offset)
  } else if(offset %in% c("low", "high")){
    sel_data_prob <- prepped_data_prob$probs
    ret <- map2(sel_data, sel_data_prob, function(x, x_prob, sel_param, offset){
      vec <- x_prob |> filter(param == sel_param) |> pull(prob) |> unlist()
      new_value <- stats::quantile(vec, if_else(offset == "high", .975, .025))
      
      
      out <- x |> 
        mutate(prob = case_when(
          param == sel_param ~ as.list(new_value),
          .default = prob
        ))
      
      return(out)
      
    }, sel_param, offset)
  } else{
    stop("input for offset not correct, should be a double or low or high")
  }
  
  
  prepped_data$probs <- ret
  return(prepped_data)
}

#' Helper function to generate DSA parameters for costs
#'
#' @param sel_param specific selected parameter name
#' @param offset offset, or factor to multiply the parameter by
#' @param prepped_data prepped data set from prep_data()
#'
#' @returns
#'
calc_dsa_costs <- function(sel_param, offset, prepped_data, prepped_data_prob = NA){
  sel_data <- prepped_data$costs
  
  if(is.double(offset)){
    ret <- map(sel_data, function(x, sel_param, offset){
      out <- x |> 
        mutate(cost = case_when(
          cost_item == sel_param ~ map(cost, ~.x * offset),
          .default = cost
        ))
      return(out)
  }, sel_param, offset)
    } else if(offset %in% c("low", "high")){
    sel_data_prob <- prepped_data_prob$costs
    ret <- map2(sel_data, sel_data_prob, function(x, x_prob, sel_param, offset){
      vec <- x_prob |> filter(cost_item == sel_param) |> pull(cost) |> unlist()
      new_value <- stats::quantile(vec, if_else(offset == "high", .975, .025))
      
      
      out <- x |> 
        mutate(cost = case_when(
          cost_item == sel_param ~ as.list(new_value),
          .default = cost
        ))
      
      return(out)
      
    }, sel_param, offset)
  } else{
    stop("input for offset not correct, should be a double or low or high")
  }
  
  
  prepped_data$costs <- ret
  return(prepped_data)
  
}

#' Helper function to generate DSA parameters for outcomes
#'
#' @param sel_param specific selected parameter name
#' @param offset offset, or factor to multiply the parameter by
#' @param prepped_data prepped data set from prep_data()
#'
#' @returns
#'
calc_dsa_outc <- function(sel_param, outc_cat, offset, prepped_data, prepped_data_prob = NA){
  sel_data <- prepped_data$outcomes

  if(is.double(offset)){
    ret <- map(sel_data, function(x, sel_param, offset){
      out <- x |> 
        mutate(cost = case_when(
          item == sel_param & category == outc_cat ~ map(outc, ~.x * offset),
          .default = outc
        ))
      return(out)
    }, sel_param, offset)} else if(offset %in% c("low", "high")){
      sel_data_prob <- prepped_data_prob$outcomes
      ret <- map2(sel_data, sel_data_prob, function(x, x_prob, sel_param, offset){
        vec <- x_prob |> filter(item == sel_param & category == outc_cat) |> pull(outc) |> unlist()
        new_value <- stats::quantile(vec, if_else(offset == "high", .975, .025))
        
        
        out <- x |> 
          mutate(cost = case_when(
            item == sel_param & category == outc_cat ~ as.list(new_value),
            .default = outc
          ))
        
        return(out)
        
      }, sel_param, offset)
    } else{
      stop("input for offset not correct, should be a double or low or high")
    }

  
  
  prepped_data$outcomes <- ret
  return(prepped_data)
}

#' Helper function to generate DSA parameters for productivity
#'
#' @param sel_param specific selected parameter name
#' @param offset offset, or factor to multiply the parameter by
#' @param prepped_data prepped data set from prep_data()
#'
#' @returns
#'
calc_dsa_productivity <- function(sel_param, offset, prepped_data){
  sel_data <- prepped_data$productivity
  ret <- map(sel_data, function(x, var_name, offset){
    new_value  <- x[[1, var_name]]*offset
    
    x[[1, var_name]] <- new_value
    
    return(x)
    
  }, sel_param, offset)
  
  prepped_data$productivity <- ret
  return(prepped_data)
}


# Other helper functions --------------------------------------------------



#' Add seasonality to probabilities
#'
#' @param month affected month (double)
#' @param probs  probabilities
#' @param seasonality_data seasonality data input
#'
#' @return
#' @export
#'
#' @examples
add_seasonality <- function(month, probs, seasonality_data) {
  sel_month <- month
  affected_params <- c("p_hosp_inc_all", 
                       "p_hosp_inc_urti",
                       "p_gp_rsv",
                       "p_symp",
                       "p_nurhom_mort")
  
  seasonality_factor <- seasonality_data |> 
    dplyr::filter(month == sel_month) |> 
    dplyr::pull(prop_cases)
  
  corr_probs <- probs |> 
    dplyr::filter(param %in% affected_params) |> 
    dplyr::mutate(prob = purrr::map(prob, ~.x * seasonality_factor))
  
  new_probs <- probs |> 
    dplyr::filter(!(param %in% affected_params)) |> 
    dplyr::bind_rows(corr_probs)
  
  return(new_probs)
}

#' Apply risk stratification
#'
#' @param risk_group risk group, either "average", "low", or "high"
#' @param probs probabilities
#' @param sel_age age
#' @param sel_month month in analysis
#' @param sel_year year in analysis
#' @param pop_start starting population created with demographics$create() and demographics$add_highrisk()
#'
#' @returns
#' @export
#'
#' @examples
apply_riskstratification <- function(risk_group, probs, sel_age, sel_month, sel_year, pop_start) {
  if(is.na(sel_month)){
    pop_start <- pop_start |> 
      mutate(month = 1)
    
    sel_month <- 1
  }
  
  
  if(risk_group == "average"){
    # if no correction for risk stratification, return probs as is.
    ret <- probs
  } else {
    risk_lrtd <- 1.245 # increased risk LRTD
    params_lrtd <- c("p_hosp_inc_all", "p_gp_rsv") #Affected params LRTD
    risk_ari <- 1.16 # increased risk URTD
    params_urtd <- c("p_gp_urti", "p_hosp_inc_urti") #Affected params LRTD
    
    
    if(risk_group == "low"){
      # if risk group is low, apply reduced risk of RSV GP consult and hospitalization
      
      sel_pop <- pop_start |> 
        dplyr::filter(year == sel_year,
               month == sel_month,
               age == sel_age) |> 
        dplyr::select(population, risk_group)
      
      #proportion of population with higher risk
      prop_hr <- dplyr::filter(sel_pop, risk_group == "high")$population / 
        (dplyr::filter(sel_pop, risk_group == "high")$population + dplyr::filter(sel_pop, risk_group == "low")$population)
      
      #reduced risk is calculated here
      low_risk_lrtd <- (1 - (prop_hr * risk_lrtd)) / (1 - prop_hr)
      low_risk_ari <- (1 - (prop_hr * risk_ari)) / (1 - prop_hr)
      
      ret <- probs |> 
        dplyr::mutate(prob = dplyr::case_when(
          param %in% params_lrtd ~ purrr::map(prob, ~.x * low_risk_lrtd),
          param %in% params_urtd ~ purrr::map(prob, ~.x * low_risk_ari),
          .default = prob
        ))
      
    } else if(risk_group == "high"){
      # if risk group is low, apply increased risk of RSV GP consult and hospitalization
      ret <- probs |> 
        dplyr::mutate(prob = dplyr::case_when(
          param %in% params_lrtd ~ purrr::map(prob, ~.x * risk_lrtd),
          param %in% params_urtd ~ purrr::map(prob, ~.x * risk_ari),
          .default = prob
        ))
    }
  }
  
  
  return(ret)
}

#' Apply strategy-specific costs
#'
#' @param costs prepped costs
#' @param strategy name of strategy
#'
#' @returns
#' @export
#'
#' @examples
apply_strategy_costs <- function(costs, strategy, settings){
  strat <- pluck(settings$included_strategies, as.character(strategy))
  vaccine_costs <- tibble(
    cost_category = "vaccination",
    cost_item = "vaccine",
    cost = list(strat$price),
    perspective_hc = TRUE,
    perspective_soc = TRUE
  )
  
  costs |> 
    add_row(vaccine_costs) |> 
    mutate(linked_total_rsv_cases = FALSE,
           linked_nma = case_when(
             cost_category == "nma" ~ TRUE,
             .default = FALSE
           ),
           linked_gp_visit = case_when(
             cost_category == "gp" ~ TRUE,
             cost_item == "medication following gp visit" ~ TRUE,
             .default = FALSE
           ),
           linked_ed = FALSE,
           linked_ed_nonhosp = case_when(
             cost_category == "ed" ~ TRUE,
             cost_item == "patient transport to hospital by car" ~ TRUE,
             .default = FALSE
           ),
           linked_hosp_nonicu = case_when(
             cost_item == "hospitalization, general ward"  ~ TRUE,
             cost_item == "patient transport to hospital by car" ~ TRUE,
             .default = FALSE
           ),
           linked_hosp_icu = case_when(
             cost_item == "hospitalization, including ICU"  ~ TRUE,
             cost_item == "patient transport to hospital by car" ~ TRUE,
             .default = FALSE
           ),
           linked_hosp_surv = FALSE,
           linked_hosp_total = FALSE,
           linked_mort_hosp = FALSE,
           linked_mort_nurse = FALSE,
           linked_mort_total = FALSE,
           linked_ae3_local = case_when(
             cost_item == "adverse event, grade 3"  ~ TRUE,
             .default = FALSE
           ),
           linked_ae3_syst = linked_ae3_local,
           linked_ae4 = case_when(
             cost_item == "adverse event, severe"  ~ TRUE,
             .default = FALSE
           ),
           linked_vaccination = case_when(
             cost_category == "vaccination"  ~ TRUE,
             .default = FALSE
           ))
  
  
}

#' Add vaccine effect (old)
#'
#' @param month current month
#' @param strategy selected strategy
#' @param vaccine_status vaccination status, either vaccinated or unvaccinated
#' @param rsv_dat imported data for RSV
#'
#' @return
#'
add_vaccine_effect_old <- function(month, year, strategy, vaccine_status, rsv_dat){
  if(vaccine_status == "unvaccinated"){
    strategy <- "no_vaccine"
  } 

  
  if(is.na(month)){
    sel_month <- 1
  } else {
    sel_month <- month
  }
  
  sel_year <- year
  

  eff_vac <- rsv_dat$vaccines |>  
    pluck(strategy) |> 
    pluck("effectiveness") |> 
    filter(month == sel_month,
           year == sel_year) |> 
    mutate(effectiveness = map(effectiveness, ~1 - .x),
           outcome = stringr::str_c("ve_", outcome))
  
  
  out_eff <- as.list(eff_vac$effectiveness)
  names(out_eff) <- eff_vac$outcome
      
  return(out_eff)
}

#' Add vaccine effect
#'
#' @param month current month
#' @param strategy selected strategy
#' @param vaccine_status vaccination status, either vaccinated or unvaccinated
#' @param rsv_dat imported data for RSV
#'
#' @return
#' @export
#'
#' @examples
add_vaccine_effect <- function(month, year, strategy, vaccine_status, rsv_dat){
  if(is.na(month)){
    sel_month <- 1
  } else {
    sel_month <- month
  }
  
  sel_year <- year
  
  if(vaccine_status == "unvaccinated"){
    strat <- rsv_dat$vaccines$no_vaccine
  } else if(strategy == "no_vaccine"){
    strat <- rsv_dat$vaccines$no_vaccine
  } else if(vaccine_status == "vaccinated" & strategy == "rsv_generic_vaccine"){
    strat <- rsv_dat$vaccines$rsv_generic_vaccine
  } else {
    stop("Vaccine not recognized")
  }
  
  
  
  
  
  eff_vac <- strat |> 
    pluck("effectiveness") |> 
    filter(month == sel_month,
           year == sel_year) |> 
    mutate(effectiveness = map(effectiveness, ~1 - .x),
           outcome = stringr::str_c("ve_", outcome))
  
  
  out_eff <- as.list(eff_vac$effectiveness)
  names(out_eff) <- eff_vac$outcome
  
  return(out_eff)
}





