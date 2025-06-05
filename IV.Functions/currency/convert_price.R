box::use(utils[...])
box::use(dplyr[...])
box::use(stringr[...])
box::use(purrr[...])
box::use(./update_currency_data[...])

data_location <- str_c(box::file(),"/data/")
available_files <- list.files(data_location)

if(sum(c("cpi.rds", "ppp.rds", "exc_rate.rds", "curr_labels.rds") %in% available_files) != 4){
  month <- lubridate::month(Sys.Date() - lubridate::dmonths(2))
  update_currency_data(month)
}

eurostat_data <- list(
  cpi = readRDS(stringr::str_c(data_location, "cpi.rds")),
  ppp = readRDS(stringr::str_c(data_location, "ppp.rds")),
  exchange_rate = readRDS(stringr::str_c(data_location, "exc_rate.rds")),
  country_currency = readRDS(stringr::str_c(data_location, "country_currency.rds"))
)

#' Helper function to calculate inflation factor
#'
#' @param x price to be converted
#' @param year_in vector of input years
#' @param year_out required output year (maximum 1)
#' @param country  country input in iso2c format
#'
#' @return a factor to correct for inflation
#'
calc_inflation <- function(x, year_in, year_out, country) {
  helper_function <- function(x, year_in, country, year_out, data_cpi){
    if(is.na(x)){
      return(NA)
    }
    
    level_in <- data_cpi %>% filter(geo == str_to_lower(country), year == year_in) %>% pull(values)
    level_out <- data_cpi %>% filter(geo == str_to_lower(country), year == year_out) %>% pull(values)
    
    #inflation:
    return(level_out/level_in)
  }
  
  inf_x <- x * purrr::pmap_dbl(list(x = x, year_in = year_in, country = country),
                      helper_function, year_out = year_out, data_cpi = eurostat_data$cpi)
  
  return(inf_x)
}

#' Helper function to check whether requested years are available in the dataset
#'
#' @param x year input
#' @param avail_years years available in the dataset
#'
#' @return the year input, or the most recent year if the requested year is not available (in this case a warning will be included)
#'
check_year_avail <- function(x){
  avail_years <- unique(eurostat_data$ppp$year)
  if(is.na(x)){
    out <- NA
  } else if(!(x %in% avail_years)){
    out <- as.double(max(avail_years))
    warning(str_c("The year ", x, " is not available in the dataset - using the year ", out, " instead."))
  } else{
    out <- x
  }
  
  return(out)
}

#' Helper function to calculate the PPP
#'
#' @param x cost to be converted in local currency
#' @param country country, iso2c code
#' @param year year of conversion
#' @param output one of the following: the local currency "local", "europpp" for PPP in Euro, "dollarppp" for ppp in USD
#'
#' @return
#'
calc_ppp <- function(x,
                     country,
                     year,
                     output){
  sel_country <- country
  sel_year <- year
  output <- str_to_upper(output)
  
  fct_ppp <- eurostat_data$ppp |> 
    filter(geo == sel_country,
           year == sel_year) |> 
    pull(value)
  
  if(output == "LOCAL"){
    #convert to local currency
    ret <- x * fct_ppp
  } else if(output == "EUROPPP"){
    #convert to PPP euro
    ret <- x / fct_ppp
  } else if(output == "DOLLARPPP"){
    fct_usd <- eurostat_data$ppp %>% filter(geo == "us", year == sel_year) %>% pull(value)
    #convert to PPP USD
    ret <- x / (fct_ppp / fct_usd)
  } else{
    return(warning("The requested calc_ppp output is not available"))
  }
  
  return(ret)
}

#' Convert PPPs from euro to dollar and viceversa
#'
#' @param x cost input
#' @param input either EUROPPP or DOLLARPPP
#' @param year year in which to convert PPPs
#'
#' @return
#'
#' @examples
convert_ppp_eurodollar <- function(x, year, input){
  sel_year <- year
  fct_usd <- eurostat_data$ppp %>% filter(geo == "us", year == sel_year) %>% pull(value)
  if(input == "EUROPPP"){
    ret <- x / (1 / fct_usd)
  } else if(input == "DOLLARPPP"){
    ret <- x / fct_usd
  }
  
  return(ret)
}

#' Helper function to find local currency
#'
#' @param country country, iso2c code
#'
#' @return
#'
#' @examples
find_currency <- function(country){
  eurostat_data$country_currency |> 
    filter(geo == country) |> 
    pull(curr)
}

#' Function to correct price for inflation only
#'
#' @param x price to be converted
#' @param year_in vector of input years
#' @param year_out required output year (maximum 1)
#' @param country  country input in iso2c format
#'
#' @return the inflated price
#' @export
#'
#' @examples
convert_inflation <- function(x,
                              year_in,
                              year_out,
                              country){
  #this function uses the convert_price function
  convert_price(x,
                country = country,
                year_in = year_in,
                currency_in = "LOCAL",
                year_out = year_out,
                currency_out = "LOCAL")
}

#' Convert currency
#'
#' @param x price to be converted 
#' @param currency_in input currency
#' @param currency_out output currency
#' @param year year for currency conversion
#' @param country country in iso2c format
#'
#' @return
#'
#' @examples
convert_currency <- function(x,
                             currency_in,
                             currency_out,
                             year,
                             country = NA){
  
  helper_function <- function(set_currency, set_year, country){
    if(set_currency == "LOCAL"){
      use_currency <- find_currency(country)
    } else {
      use_currency <- set_currency
    }
    
    if(use_currency == "EUR"){
      rate <- 1
    } else{
      rate <- eurostat_data$exchange_rate |> 
        filter(currency == use_currency,
               year == set_year) |> 
        pull(values)
      
      
    }
    return(rate)
  }
  
  #rate in
  in_rate <- helper_function(currency_in, year, country)
  out_rate <- helper_function(currency_out, year, country)
  
  conv_rate <- out_rate / in_rate
  
  return(x * conv_rate)
}

convert_to_EURPPP <- function(x,
                                 country,
                                 year_in = NA,
                                 currency_in = NA){
  # first convert input to local currency
  x <- convert_currency(x,
                   currency = currency_in,
                   currency_out = find_currency(country),
                   year = year_in,
                   country = country
  )
  
  # convert to EUR ppp
  x <- calc_ppp(x,
           country = country,
           year = year_in,
           output = "europpp")
  
  return(x)
}

#' Price convert function
#'
#' @param x price to be converted
#' @param country country input in iso2c format
#' @param year_in vector of input years
#' @param currency_in vector of input currencies
#' @param year_out required output year (maximum 1)
#' @param currency_out required output currency (maximum 1)
#' 
#' 
#' Currency overview can be visualized with the show_priceconvert_currency() function
#' Currencies can be provided as the abbreviated name, but also "local", "europpp" for PPP in Euro, "dollarppp" for ppp in USD
#'
#' @return vector of converted prices
#' @export
#'
#'
#' @examples
convert_price <- function(x,
                          country,
                          year_in = NA,
                          currency_in = NA,
                          year_out = NA,
                          currency_out = NA){
  if(length(x) != length(year_in) && length(year_in) > 1){
    return(warning("x and year_in should be of the same length, or year_in should be of length 1"))
  }
  if(length(year_out) + length(currency_out) != 2){
    return(warning("Please make sure that the year_out and currency_out are of length 1"))
  }
  
  year_in <- as.integer(year_in)
  year_out <- as.integer(year_out)
  year_in <- check_year_avail(year_in)
  year_out <- check_year_avail(year_out)
  country <- str_to_lower(country)
  currency_in <- str_to_upper(currency_in)
  currency_out <- str_to_upper(currency_out)
  
  #if only an inflation correction is requested, skip the long function and just return the price corrected for inflation
  if(currency_in == currency_out){
    return(calc_inflation(x, year_in, year_out, country) )
  }
  
  # first convert input to EURO PPP
  if(currency_in != "EUROPPP"){
    x <- pmap_dbl(list(x, country, year_in, currency_in), 
                  convert_to_EURPPP)
  }
  
  # then convert to requested currency
  if(currency_out == "EUROPPP"){
    x <- x
  } else if(currency_out == "DOLLARPPP"){
    x <- map2_dbl(x, year_in, convert_ppp_eurodollar, "EUROPPP")
  } else{
    x <- pmap_dbl(list(x, country, year_in, "LOCAL"), 
                  ~calc_ppp(..1, ..2, ..3, ..4))
  }

  
  # if necessary convert amount from local to another currency
  if(!(currency_out %in% c("DOLLARPPP", "EUROPPP", "LOCAL"))){
    x <- pmap_dbl(list(x, "LOCAL", currency_out, year_in, country),
              convert_currency)
  } else{
    x <- x
  }
  
  # inflation correction
  
  x <- calc_inflation(x, year_in, year_out, country)
  
  return(x)
  
  
}

#' Calculate one multiplication factor to convert price
#' This can be used for large datasets with the same currency input to speed up performance
#'
#' @param country countrycode
#' @param country country input in iso2c format
#' @param year_in vector of input years
#' @param currency_in vector of input currencies
#' @param year_out required output year (maximum 1)
#' @param currency_out required output currency (maximum 1)
#' 
#' Currency overview can be visualized with the show_priceconvert_currency() function
#' Currencies can be provided as the abbreviated name, but also "local", "europpp" for PPP in Euro, "dollarppp" for ppp in USD
#' 
#' @return a conversion factor, double
#' @export
#'
#' @examples
convert_price_conversion_factor <- function(country,
                                            year_in = NA,
                                            currency_in = NA,
                                            year_out = NA,
                                            currency_out = NA){
  
  #warnings when too many or too little arguments are entered
  if(length(country) != 1){return(warning("Please provide 1 country for convert_price_conversion_factor()"))}
  if(length(year_in) != 1 | length(year_out) != 1){return(warning("Please provide 1 input and output year for convert_price_conversion_factor()"))}
  
  conv_x <- convert_price(
    x = 1,
    country = country,
    year_in = year_in,
    currency_in = currency_in,
    year_out = year_out,
    currency_out = currency_out
  )
  
  return(conv_x)
}

show_priceconvert_currency <- function(){
  eurostat_data$country_currency |> 
    View()
}
  
