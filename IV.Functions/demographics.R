box::use(dplyr[...])


#' Function to create population dataset based on CBS dataset
#'
#' @param settings settings list with population and time period in which to run the analysis
#' @param time_interval "month" (default) or "year"
#'
#' @return a tibble with the population in the requested years for the specified age groups
#' @export
#'
create <- function(settings, time_interval = "month"){
  #check whether age does not exceed 98
  if(settings$max_age + (settings$n_years - 1) > 98) {
    stop(error("Maximum age in analysis exceeds 98, this is not supported."))
  }
  
  #years to include in analysis
  analysis_years <- (settings$start_year + 1) : (settings$start_year + settings$n_years) # +1 to use data from Januari for the autumn
  
  if(settings$cohort_type == "closed"){
    #ages to include in the analysis
    analysis_ages <- settings$min_age:settings$max_age
    
    #create tibble
    init_population <- tibble(year = integer(),
                              age = integer())
    
    for(i in 1:length(analysis_years)){
      year_population <- tibble(year = analysis_years[i],
                                age = analysis_ages + (i - 1))
      init_population <- bind_rows(init_population, year_population)
    }
    
  }
  
  
  if(settings$cohort_type == "open"){
    #create tibble
    init_population <- tibble(year = integer(),
                              age = integer())
    
    for(i in 1:length(analysis_years)){
      year_population <- tibble(year = analysis_years[i],
                                age = settings$min_age:(settings$max_age + (i-1)))
      init_population <- bind_rows(init_population, year_population)
    }
    
  }

  
  demographic_data <- arrow::open_dataset("II.Data/demographics/demographics_saved") |> 
    collect() |> 
    filter(age >= settings$min_age,
           year %in% analysis_years) |> 
    group_by(year, age) |> 
    summarise(population = sum(population),
              .groups = "drop")
  
  if(time_interval == "month"){
    full_population <- init_population |> 
      left_join(demographic_data, by = c("year", "age")) |> 
      mutate(month = list(1:12)) |> 
      tidyr::unnest(month) |> 
      mutate(year = if_else(month >= settings$start_month, year - 1, year)) |> 
      arrange(year, month) |> 
      select(year, month, age, population)
  } else if(time_interval == "year"){
    full_population <- init_population |> 
      left_join(demographic_data, by = c("year", "age")) |> 
      mutate(month = NA) |> 
      arrange(year, month) |> 
      select(year, month, age, population)
  }
  
  
    
  return(full_population)
}

#' Get sex distribution
#'
#' @param start_age starting age in resulting tibble
#' @param data_year year to use for CBS data
#'
#' @return
#' @export
#'
#' @examples
sex_distribution <- function(data_year, start_age){
  arrow::open_dataset("II.Data/demographics/demographics_saved") |> 
    collect() |> 
    filter(age >= start_age,
           year %in% data_year) |> 
    tidyr::pivot_wider(names_from = "sex", values_from = population) |> 
    mutate(prop_male = male / (male + female)) |> 
    select(age, prop_male)
}

#' Add nursing home data
#'
#' @param x Demographic set created with create() function
#'
#' @returns
#' @export
#'
#' @examples
add_nursinghomes <- function(x){
  data_import <- arrow::open_dataset("II.Data/demographics/nursing_home") |> 
    collect() |> 
    select(min_age, prop_nursing)
  
  ret <- x |> 
    mutate(min_age = case_when(
      age >= 95 ~ as.integer(95),
      age < 95 ~ as.integer(age)
    )) |> 
    left_join(data_import, by = "min_age") |> 
    select(-min_age)
  
  return(ret)
}

#' Add risk stratification
#'
#' @param x population tibble
#' @param settings list with settings
#'
#' @returns
#' @export
#'
#' @examples
add_highrisk <- function(x, settings){
  if(settings$risk_stratification == TRUE){
    data_import <- arrow::open_dataset("II.Data/demographics/high_risk_population")  |> 
      collect() |> 
      mutate(age = purrr::map2(min_age, max_age, ~.x:.y)) |> 
      select(age, prop) |> 
      tidyr::unnest(age)

    
    pop_high <- x |> 
      left_join(data_import, by = "age") |> 
      mutate(risk_group = "high",
             population = population * prop) |> 
      select(-prop)
    
    pop_low <- x |> 
      left_join(data_import, by = "age") |> 
      mutate(risk_group = "low",
             population = population * (1 - prop)) |> 
      select(-prop)
    
    ret <- bind_rows(
      pop_high,
      pop_low
    ) |> 
      arrange(year, month, age, risk_group)
  } else {
    ret <- x |> 
      mutate(risk_group = "average")
  }
  
  
  
  return(ret)
}



         