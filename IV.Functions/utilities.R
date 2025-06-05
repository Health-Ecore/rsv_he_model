box::use(dplyr[...])
box::use(IV.Functions/discounting)

#' Import population utility values for Quality of Life
#'
#' @param geo geo in lower case iso2c code
#' @param value_set either "TTO" (default), "VAS" or "EUVAS"
#' @param sex either "total" (default), "male" or "female"
#'
#' @return tibble with utility values
#' @export
#'
#' @examples
import_qol <- function(geo, value_set = "TTO", sex = "total"){
  selected_geo <- geo
  selected_value_set <- value_set
  selected_sex <- sex
  import <- readRDS("II.Data/uti/base_utilities.RDS") |> 
    dplyr::filter(geo == selected_geo,
                  value_set == selected_value_set,
                  sex == selected_sex) |> 
    dplyr::select(sex, minage, maxage, mean, se)
  
  return(import)
}

#' Import population life expectancy
#'
#' @return tibble with life expectancy
#' @export
#'
#' @examples
import_le <- function(){
  data <- arrow::open_dataset("II.Data/demographics/le") |> 
    collect()
}

#' prepare Quality of Life data
#'
#' @param data data from import_qol() function
#' @param probabilistic TRUE/FALSE for probabilistic
#'
#' @return tibble with QoL data by age
#' @export
#'
#' @examples
prep_qol <- function(data, probabilistic = FALSE, iter = 1000){
  if(probabilistic == TRUE){
    qol_cont <- data |> 
      mutate(age = purrr::map2(minage, maxage, ~as.list(.x:.y)),
             qol = purrr::map2(mean, se, 
                               ~truncnorm::rtruncnorm(iter, a = 0, b = 1, 
                                                      mean = .x, sd = .y)))
  } else if(probabilistic == FALSE){
    qol_cont <- data |> 
      mutate(age = purrr::map2(minage, maxage, ~as.list(.x:.y)),
             qol = mean)
  }
  
  qol_byage <- qol_cont |> 
    tidyr::unnest(age) |> 
    mutate(age = as.integer(age)) |> 
    select(age, qol)
  
  return(qol_byage)
}

#' Calculate discounted life expectancy
#'
#' @param data tibble with age and le
#' @param d_rate discount rate
#'
#' @return
#' @export
#'
#' @examples
calculate_discounted_le <- function(data, d_rate, n_years){
  #first, create a row for all years included in the analysis
  le_comb <- tibble()
  for(i in 1:n_years){
    le_rep <- data |> 
      mutate(year = i)
    
    le_comb <- bind_rows(le_comb, le_rep)
  }
  
  #then calculate the discount rate for all years
  v_disc_le <- purrr::pmap_dbl(list(le_comb$age, le_comb$le, le_comb$year),
                         calc_disc_qale_by_age,
                         d_rate = d_rate, qol_adj = FALSE)
  
  
  le_comb |>
    mutate(disc_le = v_disc_le)
}

#' Calculate (discounted) quality-adjusted life expectancy
#'
#' @param le_data tibble with age and le
#' @param qol_data data from prep_qol()
#' @param d_ratediscount rate
#'
#' @return
#' @export
#'
#' @examples
calculate_qale <- function(le_data, qol_data, d_rate, n_years) {
  # le_comb <- tibble()
  # for(i in 1:n_years){
  #   le_rep <- le_data |> 
  #     mutate(year = i)
  #   
  #   le_comb <- bind_rows(le_comb, le_rep)
  # }
  v_qale <- purrr::pmap(list(le_data$age, le_data$le, le_data$year),
                               calc_disc_qale_by_age,
                               d_rate = 0, qol_adj = TRUE, qol_data)
  
  v_disc_qale <- purrr::pmap(list(le_data$age, le_data$le, le_data$year),
                               calc_disc_qale_by_age,
                               d_rate = d_rate, qol_adj = TRUE, qol_data)
  
  
  le_data |>
    mutate(qale = v_qale,
           disc_qale = v_disc_qale)
}

#' Helper function to calculate discounted QALE per age
#'
#' @param le life expectancy (double)
#' @param age starting age
#' @param qol_adj TRUE/FALSE depending on whether the life exp should be corrected for QoL
#' @param qol_data QoL data from prep_qol()
#' @param current_year year to calculate discount for, starting with year 1 (which is not discounted further)
#'
#' @return the discounted life expectancy
#'
#' @examples
calc_disc_qale_by_age <- function(age, le, current_year, d_rate, qol_adj = FALSE, qol_data = NA){
  if(d_rate >= 1){
    stop("discount rate not valid")
  } 
  
  if(age < 18 & qol_adj == TRUE){
    return(NA)
  }
  
  a <- age
  i <- le
  y_d <- as.integer(current_year) - 1 #year used for discount rate
  y_le <- 0 #year for current loop
  t <- 0
  
  if(qol_adj == TRUE){
    qol <- qol_data |> 
      dplyr::filter(age >= a)
    
    #repeat final qol score 30 times to be able to calculate qale for people that are expected to be older than 99
    qol_last <- qol_data |> 
      dplyr::slice(rep(n(),30))
    
    qol <- dplyr::bind_rows(qol, qol_last)
  } else {
    qol <- tibble::tibble(
      age = 0:200,
      qol = 1
    )
  }
  while (i > 1){
    #calculate applicable discount rate
    v <- discounting$discount_value(1, y_d, d_rate)
    #increase year number
    y_le <- y_le + 1
    y_d <- y_d + 1
    
    #read qol data for current year
    qol_current <- dplyr::slice(qol, y_le) |> 
      dplyr::pull(qol) |> 
      unlist()
    
    #calculate cumulative QALE
    t <- t + v * qol_current
    
    #increase age
    a <- a + 1
    
    #subtract one from life expectency 
    i <- i - 1
  }
  
  #calculate final year, which is a partial year
  ##calculate applicable discount rate
  v <- discounting$discount_value(i, y_d, d_rate)
  #increase year number
  y_le <- y_le + 1
  y_d <- y_d + 1
  
  #read qol data for current year
  qol_current <- dplyr::slice(qol, y_le) |> 
    dplyr::pull(qol) |> 
    unlist()
  
  #calculate cumulative QALE, corrected for remaining le
  t <- t + v * qol_current * i
  
  return(t)
  
  
}
