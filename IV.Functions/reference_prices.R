box::use(IV.Functions/currency_cbs)

dataset <- readxl::read_excel("II.Data/reference_prices/Kostenhandleiding_2024.xlsx")


#' Easily explore the data from the kostenhandleiding
#'
#' @return start a reactable instance
#' @export
#'
explore <- function(){
  reactable::reactable(dataset, filterable = TRUE, groupBy = "Categorie")
}

#' Get value from Dutch reference prices
#'
#' @param item the item (or "Eenheid") that is requested
#' @param currency_year_out the currency year of the output for prices
#'
#' @return the requested item
#' @export
#'
get_value <- function(item, currency_year_out = NA){
  data_row <- dplyr::filter(dataset, Eenheid == item)
  
  if(is.na(currency_year_out)){
    return(data_row$`Standaard rekenwaarden en referentieprijzen`)
  } else if(data_row$Meeteenheid == "euro"){
    return(currency_cbs$convert_cpi(data_row$`Standaard rekenwaarden en referentieprijzen`,
                             year_in = 2022,
                             year_out = currency_year_out))
  } else {
    stop("The currency_year_out should only be specified for prices")
  }
    
}

