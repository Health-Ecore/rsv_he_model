#' Currency convert using cpi
#'
#' @param x vector of costs
#' @param year_in input year(s)
#' @param year_out output year(s)
#'
#' @return inflation-corrected prices
#' @export
#'
convert_cpi <- function(x, year_in, year_out){
  # load cpi dataset
  cpi_data <- arrow::read_csv_arrow("II.Data/cpi/cpi_data.csv")
  
  # value to convert year to position in dataset
  pos <- min(cpi_data$year)
  
  
  #check if all the required years are available in the dataset
  if(year_in %in% cpi_data$year & year_out %in% cpi_data$year){
    #wrap the "single" version in pmap to be able to perform calculation on vectors
    out <- purrr::pmap_dbl(list(x, year_in - pos, year_out - pos),
                    function(x, pos_in, pos_out, cpi_values){
                      x * (cpi_values[pos_out] / cpi_values[pos_in])
                    },
                    cpi_data$cpi)
    
    return(out)
  } else{
    #send error message if years are not available
    msg <- stringr::str_c("Requested years not available in dataset, the year in and year out should be between: ",
                 min(cpi_data$year), " and ",
                 max(cpi_data$year))
    stop(msg)
  }
  
  
}


