box::use(utils[...])
box::use(dplyr[...])

#' Update input data for currency_convert function
#'
#' @param selected_month month to be looked for in online datasets (double) 
#'
#' @export
#'
#' @examples run update_currency_data(6) to update all data with the month June as reference
update_currency_data <- function(selected_month){
  print("Start proces of updating source data for use within currency_convert module")
  
  
  
  ### get file locatation
  save_location <- box::file()

  ### Harmonized index of consumer prices
  
  hicp <- eurostat::get_eurostat(id = "prc_hicp_midx") %>%
    filter(!(geo %in% c("EA20", "XK", "EA", "EEA", "EU", "EU15", "EU25", "EU27_2020", "EU28", "EA19", "EA16", "EA17", "EA18", "EU27_2007", "CPC1", "EA11", "EA12", "EA13", "EA15"))) %>%
    mutate(year = lubridate::year(TIME_PERIOD), 
           month = lubridate::month(TIME_PERIOD),
           geo = stringr::str_to_lower(countrycode::countrycode(geo, origin = "eurostat", destination = "iso2c"))) %>%
    filter(coicop == "CP00", unit == "I15", month == selected_month) %>%
    select(geo, year, values)
  
  saveRDS(hicp, stringr::str_c(save_location, "/data/cpi.rds"))
  
  print(stringr::str_c("Updated inflation (cpi) data to: ",
        hicp %>% pull(year) %>% max()))
  
  ### PPP
  
  ppp_oecd <- OECD::get_dataset("SNA_TABLE4") %>%
    mutate(Time = as.double(Time)) %>%
    filter(TRANSACT == "PPPGDP",
            Time >= 2000,
            !(LOCATION %in% c("EA19", "EU28", "EU27_2020"))) %>%
    mutate(geo = stringr::str_to_lower(countrycode::countrycode(LOCATION, origin = "iso3c", destination = "iso2c"))) %>%
    select(geo, unit = UNIT, year = Time, value = ObsValue)

  ppp_currencies <- ppp_oecd %>%
     arrange(year) %>%
     group_by(geo) %>%
     summarise(curr = last(unit))
  
  
  
  ppp_euro <- eurostat::get_eurostat(id = "prc_ppp_ind") %>%
    rename(time = TIME_PERIOD) %>%
    filter(na_item == "PPP_EU27_2020",
           ppp_cat == "GDP",
           time > "1999-12-31") %>%
    filter(!(geo %in% c("EA20", "EU15", "EU25", "EU27_2020", "EU28", "EA19", "EA16", "EA17", "EA18", "EU27_2007", "CPC1", "EA11", "EA12", "EA13", "EA15"))) %>%
    mutate(geo = stringr::str_to_lower(countrycode::countrycode(geo, origin = "eurostat", destination = "iso2c")),
           year = lubridate::year(time)) %>%
    select(geo, year, value = values) %>%
    inner_join(ppp_currencies, by = "geo")

  saveRDS(ppp_euro, stringr::str_c(save_location, "/data/ppp.rds"))
  
  country_currency <- ppp_euro |> 
    group_by(geo, curr) |> 
    arrange(year) |> 
    summarise(curr = last(curr))
  
  saveRDS(country_currency, stringr::str_c(save_location, "/data/country_currency.rds"))
  
  print(stringr::str_c("Updated ppp data to: ",
        ppp_euro %>% pull(year) %>% max()))
  
  ### Exchange rates
  
  exc_rate <- eurostat::get_eurostat(id = "ert_bil_eur_m") %>%
    mutate(year = lubridate::year(TIME_PERIOD),
           month = lubridate::month(TIME_PERIOD)) %>%
    filter(month == selected_month,
           statinfo =="AVG") %>%
    select(currency, year, values)
  
  labels_currency <- eurostat::get_eurostat(id = "ert_bil_eur_m") %>%
    eurostat::label_eurostat(lang = "en", code = c("currency")) %>%
    group_by(currency_code) %>%
    summarise(currency_name = first(currency))
  
  saveRDS(exc_rate, stringr::str_c(save_location, "/data/exc_rate.rds"))
  saveRDS(labels_currency, stringr::str_c(save_location, "/data/curr_labels.rds"))
  
  print(stringr::str_c("Updated currency data to: ",
        exc_rate %>% pull(year) %>% max()))
  
}