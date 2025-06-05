box::use(lubridate[...])
box::use(dplyr[...])

#' Discount value
#'
#' @param x any (vector) of values
#' @param n_years Number of years to discount
#' @param rate discount rate
#'
#' @return the discounted values
#' @export
#'
discount_value <- function(x, n_years, rate) {
  if(rate >= 1 | rate < 0){
    stop("discount rate not valid")
  } 
  
  x * (1 / ((1+rate)^n_years))
}

#' Apply discounting to a table
#'
#' @param x a tibble/table
#' @param ref_date reference date to be used for the discounting
#' @param rate discount rate
#' @param col_value column containing the value that needs to be discounted
#' @param col_date column containing the date to be compared to the reference date
#'
#' @return a tibble
#' @export
#'
#' @examples
discount_table <- function(x, ref_date, rate, col_value = "value", col_date = "date") {
  ref_date <- as_date(ref_date)
  out <- x |> 
    rename(discount_var = all_of(col_value),
           date_var = all_of(col_date)) |> 
    mutate(discount_n_years = floor(as.duration(date_var - ref_date) / dyears()),
           "{col_value}" := discount_value(discount_var, discount_n_years, rate),
           "{col_date}" := date_var,
           discounted = TRUE) |> 
    select(-discount_var, -discount_n_years, -date_var)
  
  return(out)
}


