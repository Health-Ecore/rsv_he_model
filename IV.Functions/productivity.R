box::use(dplyr[...])
box::use(IV.Functions/reference_prices)

#' Import productivity data
#'
#' @param minimum_age starting age
#' @param maximum_age ending age
#'
#' @return tibble with labour participation and duration
#' @export
#'
#' @examples
import_labour <- function(minimum_age, maximum_age) {
  participation_over75 <- tibble::tibble(
    min_age = 75,
    max_age = 98,
    participation = 0
  )
  
  duration_over75 <- tibble::tibble(
    min_age = 75,
    max_age = 98,
    labourduration = 0
  )
  
  participation <- arrow::open_dataset("II.Data/demographics/labour_participation") |> 
    select(min_age, max_age, participation) |> 
    collect() |> 
    add_row(participation_over75) |> 
    mutate(indicator = "participation",
           value = participation) |> 
    select(indicator, min_age, max_age, value)
  
  
  duration <- arrow::open_dataset("II.Data/demographics/labour_duration") |> 
    collect() |> 
    add_row(duration_over75) |> 
    mutate(indicator = "duration",
           value = labourduration) |> 
    select(indicator, min_age, max_age, value)
  
  friction_period <- tibble(
    indicator = "friction_period",
    min_age = 0,
    max_age = 100,
    value = reference_prices$get_value("Frictieperiode (weken)") * 7,
    reference = "Dutch costing manual 2024"
  ) 
  
  ret <- bind_rows(participation, duration) |> 
    mutate(reference = "CBS") |> 
    add_row(friction_period)
  
  return(ret)
  
}