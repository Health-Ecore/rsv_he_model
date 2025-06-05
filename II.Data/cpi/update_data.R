box::use(cbsodataR[...])
box::use(dplyr[...])
box::use(stringr[...])

cbs_cpi <- cbs_get_data("83131NED")
cbs_cpi_meta <- cbs_get_meta("83131NED")

# set reference month, available data can be found on: https://opendata.cbs.nl/#/CBS/nl/dataset/83131NED/table
# to use the annual cpi estimate, set to 0
ref_month <- 6



tidy_cpi <- cbs_cpi |> 
  mutate(year = as.double(str_sub(Perioden, 1, 4)),
         month = as.double(str_sub(Perioden, 7, 8))) |> 
  filter(Bestedingscategorieen == "T001112  ",
         month == ref_month) |> 
  select(year, cpi = CPI_1)

arrow::write_csv_arrow(tidy_cpi, "II.Data/cpi/cpi_data.csv")
