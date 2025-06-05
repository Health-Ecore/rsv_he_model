box::use(dplyr[...])
#' Import RSV vaccine
#'
#' @param vaccine name of the vaccine
#' @param probabilistic TRUE or FALSE
#' @param administration 
#' @param waning 
#' @param n_years 
#' @param iter 
#' @param vaccine_settings_file file in "II.Data/vaccines/"
#' @param settings 
#'
#' @return a list with information on the vaccine
#' @export
#'
#' @examples
import_rsv_vaccine <- function(vaccine, vaccine_settings_file, administration, waning, n_years, probabilistic, iter, settings){
  # read in yaml file with vaccine information
  input <- yaml::read_yaml(stringr::str_c("II.Data/vaccines/", vaccine_settings_file, ".yaml"))
  # calculate the effectiveness over time (waning effect)
  effectiveness <- purrr::map2(names(input$effect), input$effect, ~calc_rsv_vaccine_effect(
    name = .x,
    dat = .y,
    administration = administration,
    waning = waning,
    n_years = n_years,
    probabilistic = probabilistic,
    iter = iter
  )) |> 
    purrr::list_rbind() |> 
    mutate(date = lubridate::as_date(stringr::str_c(settings$start_year, "-", settings$start_month, "-01")) + base::months(month - 1),
           month = lubridate::month(date),
           year = lubridate::year(date)) |> 
    select(-date)
  
  # calculate the years in which there will be a vaccination
  vac_years <- c(settings$start_year, settings$start_year + c(administration*1:settings$n_years))
  
  rsv_vaccine <- list(
    name = input$name,
    cost = input$cost,
    effectiveness = effectiveness,
    vaccination_month = settings$start_month,
    vaccination_year = vac_years
  )
  
  
  return(rsv_vaccine)
}

#' Calculate RSV vaccine effectiveness over time
#'
#' @param name name of the outcome
#' @param dat effectiveness related to the specific outcome
#' @param administration how many times each year is the vaccine administered, 1 for annual, 2 for every two years, etcetera
#' @param waning waning method, currently either "monthly" or "none"
#' @param n_years time horizon in years
#' @param probabilistic TRUE or FALSE for probabilistic mode
#' @param iter number of iterations, only used if probabilistic == TRUE
#'
#' @return tibble with vaccine effectiveness over time
#'
#' @examples
calc_rsv_vaccine_effect <- function(name, dat, administration, waning, n_years, probabilistic = FALSE, iter){
  #calculate the initial effectiveness, either probabilistic or deterministic
  if(probabilistic == TRUE & dat$initial$point != 0){
    lower <- dat$initial$low
    upper <- dat$initial$high
  }else{
    #set lower and upper bounds of probabilistic curve to NA
    lower <- upper <- NA
  }
  #get point estimate of effectiveness
  initial <- dat$initial$point
  
  #depending on the waning type, calculate the development of the effectiveness over time
   if(waning == "linear"){
     waning_effect <- calculate_waning_linear(initial, dat$linear_waning_stable, dat$linear_waning_zero,
                                               lower = lower, upper = upper, iter = iter)
  } else if(waning == "sigmoid"){
    waning_effect <- calculate_waning_sigmoid(initial, dat$sigmoid_waning_stable, dat$sigmoid_waning_zero,
                                              lower = lower, upper = upper, iter = iter)
  } else if(waning == "none"){
    waning_effect <- calculate_waning_linear(initial, n_years*12, (n_years*12)+24,
                                             lower = lower, upper = upper, iter = iter)
  } else {
    stop("waning type not supported")
  }
  
  #calculate the months to use in the analysis, taking into account the time horizon and repeated administrations
  analysis_months <- rep(1:(administration*12), ceiling(n_years / administration))[1:(n_years*12)]
  
  #combine into a tibble, with a varying vaccine effectiveness each month
  monthly_effectiveness <- waning_effect |> 
    slice(analysis_months) |> 
    mutate(outcome = name, 
           month = 1:(n_years*12)) |> 
    select(outcome, month, effectiveness)
  
  return(monthly_effectiveness)
  
  
}

#' Calculate sigmoid function
#'
#' @param x Time in months
#' @param L Maximum value of effectiveness (Vaccine effectiveness at t=0).
#' @param x0 The point where the function reaches half of its maximum value
#' @param k The steepness or slope of the curve, controlling how sharply the function moves from high to low values around the midpoint
#'
#' @return
#'
#' @examples
calculate_sigmoid_fun <- function(x, L, x0, k) {
  L / (1 + exp(k * (x - x0)))
}

#' Function to calculate waning effect (sigmoid)
#'
#' @param effectiveness initial effectiveness of the vaccine
#' @param months_stable initial number of months to keep the vaccine effectiveness stable
#' @param months_zero number of months where the effectiveness reaches 0
#' @param lower lower value of vaccine effectiveness
#' @param upper upper value of vaccine effectiveness
#' @param iter number of iterations for psa
#'
#' @return
#'
#' @examples
calculate_waning_sigmoid <- function(effectiveness, months_stable, months_zero, lower, upper, iter){
  #calculate parameters for sigmoid function
  x <- 0:(months_zero-1)
  x0 <- (months_stable + (0.5 * (months_zero - months_stable)))
  k <- months_stable / months_zero
  
  
  if(!is.na(lower) & !is.na(upper)){
  
    effectiveness_start <- calculate_prob_ve(effectiveness, lower, upper, iter)
  } else {
    effectiveness_start <- effectiveness
  }
  
  ret <- purrr::map(effectiveness_start, ~tibble(month = 1:months_zero, 
                                   effectiveness = calculate_sigmoid_fun(x = x, L = .x, x0 = x0, k = k))) |> 
    purrr::list_rbind() |> 
    group_by(month) |> 
    tidyr::nest(.key = "effectiveness") |> 
    mutate(effectiveness = purrr::map(effectiveness, ~.x$effectiveness)) |> 
    ungroup()
  
  return(ret)
}

#' Function to calculate waning effect (linear)
#'
#' @param effectiveness initial effectiveness of the vaccine
#' @param months_stable initial number of months to keep the vaccine effectiveness stable
#' @param months_zero number of months where the effectiveness reaches 0
#' @param lower lower value of vaccine effectiveness
#' @param upper upper value of vaccine effectiveness
#' @param iter number of iterations for psa
#'
#' @return
#'
#' @examples
calculate_waning_linear <- function(effectiveness, months_stable, months_zero, lower, upper, iter){
  if(effectiveness == 0){
    effectiveness_start <- effectiveness
  } else if(!is.na(lower) & !is.na(upper)){
    
    effectiveness_start <- calculate_prob_ve(effectiveness, lower, upper, iter)
  } else {
    effectiveness_start <- effectiveness
  }

  #calculate parameters for linear decline of vaccine effectiveness
  n_months_decline <- months_zero - months_stable
  monthly_decline <- effectiveness_start / n_months_decline
  
  effectiveness_time <- purrr::map2(effectiveness_start, monthly_decline, 
                             ~tibble(month = 1:months_zero,
                                     effectiveness = c(rep(.x, months_stable), .x - ((1:n_months_decline)*.y))))
  
  ret <- purrr::list_rbind(effectiveness_time) |> 
    group_by(month) |>  
    tidyr::nest() |> 
    ungroup() |>  
    mutate(effectiveness = purrr::map(data, ~.x |> pull(effectiveness))) |> 
    select(month, effectiveness)
  
  return(ret)
}

#' Function to calculate waning effect (linear, depreciated)
#'
#' @param effectiveness initial effectiveness of the vaccine
#' @param months_stable initial number of months to keep the vaccine effectiveness stable
#' @param months_zero number of months where the effectiveness reaches 0
#'
#' @return a tibble with the vaccine effectiveness over time
#'
#' @examples
calculate_waning_linear_old <- function(effectiveness, waning, months, lower, upper, iter){
  if(!is.na(lower) & !is.na(upper)){
    effectiveness <- calculate_prob_ve(effectiveness, lower, upper, iter)
  }
  
  
  tibble(month = 1:months) |> 
    mutate(waning = waning * (month - 1),
           effectiveness = list(effectiveness),
           effectiveness = purrr::map2(effectiveness, waning, function(eff, wan){
             ret <- eff - wan
             ret <- purrr::map_dbl(ret, function(x){
               if(x < 0){
                 return(0)} 
               else{
                 return(x)
               }
             })
             
             return(ret)
           }))
}

#' Calculate probabilistic vaccine efficacy
#'
#' @param point point estimate (mean)
#' @param lower lower estimate of the CI
#' @param upper upper estimate of the CI
#' @param iter number of iterations 
#'
#' @return the probabilistic value of the vaccine efficacy
#'
#' @examples
calculate_prob_ve <- function(point, lower, upper, iter = 1){
  log_sd <- (log(upper) - log(lower)) / (2*1.96)
  log_mean <- log(point)
  out <- exp(truncnorm::rtruncnorm(iter, a = log(0), b = log(1), mean = log_mean, sd = log_sd))
  return(out)
  #stats::rlnorm(iter, log_mean, log_sd)
}

#' Add vaccine
#'
#' @param population population created with demographics$create()
#' @param vaccine_name name of the vaccine
#' @param settings settings imported
#' @param coverage coverage scenario, will default to 0 if vaccine_name == "no_vaccine"
#'
#' @return
#' @export
#'
#' @examples
add <- function(population, vaccine_name, coverage, settings){
  if(vaccine_name != "no_vaccine"){
    data_coverage <- tibble::as_tibble(utils::read.csv(
      stringr::str_c("II.Data/coverage/", 
                     coverage,
                     ".csv")
    )) |> mutate(age = purrr::map2(age_start, age_end, ~as.list(.x:.y))) |> 
      tidyr::unnest(age) |> 
      filter(age >= settings$min_age,
             age <= (settings$max_age + settings$start_year - 1)) |> 
      mutate(age = as.integer(age),
             neg_coverage = 1 - coverage) |> 
      rename(vaccinated = coverage, 
             unvaccinated = neg_coverage,
             risk_group = risk.group) |> 
      select(age, risk_group, vaccinated, unvaccinated)
    
   pop_out <- population |> 
      left_join(data_coverage, by = c("age", "risk_group")) |> 
      mutate(vaccinated = population * vaccinated,
             unvaccinated = population * unvaccinated,
             strategy = vaccine_name) |> 
      select(strategy, year, month, age, risk_group, vaccinated, unvaccinated, prop_nursing) |> 
      tidyr::pivot_longer(c("vaccinated", "unvaccinated"), 
                          names_to = "vaccine_status", 
                          values_to = "population")
  } else if(vaccine_name == "no_vaccine"){
    pop_out <- population |> 
      mutate(strategy = vaccine_name,
             vaccine_status = "unvaccinated") |> 
      select(strategy, year, month, age, risk_group, population, prop_nursing, vaccine_status)
  }
  
  return(pop_out)
  
}


