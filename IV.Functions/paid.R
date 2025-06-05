box::use(dplyr[...])
box::use(IV.Functions/currency_cbs)
box::use(IV.Functions/demographics)
box::use(IV.Functions/discounting)

#' Import paid
#'
#' @param currency_year currency year
#'
#' @return
#' @export
#'
#' @examples
import <- function(currency_year){
  paid_currency_year <- 2017
  
  import <- readxl::read_excel(path = "II.Data/PAID/PAID__Future costs.xlsx", sheet = "Unrelated costs") |> 
    rename(age = Age,
           men_lastyear = Last.year.of.life.men,
           women_lastyear = Last.year.of.life.women,
           men_otheryears = Other.years.of.life.men,
           women_otheryears = Other.years.of.life.women) |> 
    tidyr::pivot_longer(cols = 2:5, values_to = "value", names_to = "colname") |> 
    mutate(sex = if_else(stringr::str_sub(colname, 1, 1) == "m", "men", "women"),
           category = if_else(stringr::str_sub(colname, -1, -1) == "s", "Other years", "Last year"),
           value = currency_cbs$convert_cpi(value, year_in = paid_currency_year, year_out = currency_year)) |> 
    select(age, category, sex, value)
  
  
  
  return(import)
}

#' Title
#'
#' @param indirect_medical data from PAID
#' @param le data from LE
#' @param min_age minimum age in the analysis
#' @param year starting year in the analysis
#' @param d_rate discount rate
#'
#' @return
#' @export
#'
#' @examples
prep_paid <- function(indirect_medical, le, min_age, year, d_rate){
  sex_distr <- demographics$sex_distribution(data_year = year, start_age = min_age)
  
  corr_indirect_medical <- indirect_medical |> 
    filter(age >= min_age) |> 
    left_join(sex_distr, by = "age") |> 
    mutate(value = case_when(
      sex == "men" ~ value * prop_male,
      sex == "women" ~ value * (1-prop_male)
    )) |> 
    group_by(age, category) |> 
    summarise(value = sum(value),
              .groups = "drop") |> 
    tidyr::pivot_wider(names_from = "category", values_from = "value")
  
  ret <- le |> select(-disc_le) |> filter(year == 1) |> 
    mutate(indirect_medical_costs = purrr::map2_dbl(age, le, ~calc_disc_paid(.x, .y, corr_indirect_medical, d_rate)),
           discounted = if_else(d_rate > 0, TRUE, FALSE)) |> 
    select(-le, -year)
  
  return(ret)

  
}

#' Calculate indirect medical costs per age
#'
#' @param age age
#' @param le life expectancy at age
#' @param data_paid paid data
#' @param d_rate discount rate
#'
#' @return
#'
#' @examples
calc_disc_paid <- function(age, le, data_paid, d_rate){
  a <- age
  i <- le
  y <- 0
  c <- 0

  while (i > 1){
    #calculate applicable discount rate
    v <- discounting$discount_value(1, y, d_rate)

    #calculate indirect medical costs for current year
    if(a <= 98){
      c_y <- data_paid |> filter(age == a) |> pull(`Other years`)
    } else {
      c_y <- data_paid |> filter(age == 98) |> pull(`Other years`)
    }
    
    
    #calculate cumulative indirect medical costs
    c <- c + v * c_y
    
    #increase age, year
    a <- a + 1
    y <- y + 1
    
    #subtract one from life expectency 
    i <- i - 1
  }
  
  #correction factor for partial year
  c_corr_last <- v * c_y * (1 - i)
  
  #calculate final year, which is a partial year
  ##calculate applicable discount rate
  v <- discounting$discount_value(1, y, d_rate)
  
  ##calculate indirect medical costs for current year
  if(a <= 98){
    c_y <- data_paid |> filter(age == a) |> pull(`Last year`)
  } else {
    c_y <- data_paid |> filter(age == 98) |> pull(`Last year`)
  }
  
  ##calculate cumulative indirect medical costs, subtract correction factor partial years
  c <- c + v * c_y - c_corr_last
  
  return(c)

}
