box::use(cbsodataR[...])
box::use(dplyr[...])

cbs_popproj <- cbs_get_data("85743NED")
cbs_popproj_meta <- cbs_get_meta("85743NED")


# import population projections
tidy_popproj <- cbs_popproj |> 
 filter( Geslacht %in% c("3000   ", "4000   "),
         Herkomstland == "T001040",
         Geboorteland == "T001638") |> 
 left_join(cbs_popproj_meta$Leeftijd, by = c("Leeftijd" = "Key")) |> 
 mutate(sex = case_when(
                        Geslacht == "3000   " ~ "male",
                        Geslacht == "4000   " ~ "female",
                        ),
        sex = as.factor(sex),
        year = as.integer(stringr::str_sub(Perioden, 1, 4)),
        age = round((as.double(Leeftijd) - 10000)/100), digits = 0) |> 
 filter(Leeftijd > 10000,
        Leeftijd <= 19800) |> 
 select( year,
         sex,
         age,
         population = BevolkingOp1Januari_1)



arrow::write_dataset(tidy_popproj, path = "II.Data/demographics/demographics_saved")


# import life expectancy

cbs_le <- cbs_get_data("37360ned")
cbs_le_meta <- cbs_get_meta("37360ned")

tidy_le <- cbs_le |> 
  filter(Geslacht == "T001038",
         Perioden == "2016TM21",
         LeeftijdOp31December != "22100") |> 
  mutate(age = round((as.double(LeeftijdOp31December) - 10000)/100,0)) |> 
  select(age, le = Levensverwachting_4)

arrow::write_dataset(tidy_le, path = "II.Data/demographics/le")


# import labour participation
period <- "2024KW01"
cbs_labpar <- cbs_get_data("85264ned")
cbs_labpar_meta <- cbs_get_meta("85264ned")

tidy_labpar <- cbs_labpar |> 
  left_join(cbs_labpar_meta$Leeftijd, by = c("Leeftijd" = "Key")) |> 
  mutate(Key = as.double(Leeftijd)) |> 
  filter(Geslacht == "T001038",
         HoogstBehaaldOnderwijsniveau == "T001143",
         Key >= 70400,
         Key <= 71500,
         Perioden == period) |> 
  mutate(min_age = as.double(stringr::str_sub(Title, 1, 3)),
         max_age = as.double(stringr::str_sub(Title, 8, 9)) - 1,
         participation = NettoArbeidsparticipatie_24 / 100) |> 
  select(
    age = Title,
    min_age,
    max_age,
    participation
  ) 

arrow::write_dataset(tidy_labpar, path = "II.Data/demographics/labour_participation")


# import labour duration
cbs_labdur <- cbs_get_data("85275ned")
cbs_labdur_meta <- cbs_get_meta("85275ned")

tidy_labdur <- cbs_labdur |> 
  left_join(cbs_labdur_meta$Persoonskenmerken, by = c("Persoonskenmerken" = "Key")) |> 
  filter(Geslacht == "T001038",
         Perioden == period,
         CategoryGroupID == 2) |> 
  mutate(min_age = as.double(stringr::str_sub(Title, 11, 12)),
         max_age = as.double(stringr::str_sub(Title, 18,19)) - 1,
         age_interval = max_age - min_age) |>
  filter(age_interval == 9) |> 
  select(min_age, max_age, labourduration = GemiddeldeArbeidsduurInUren_10)

arrow::write_dataset(tidy_labdur, path = "II.Data/demographics/labour_duration")


# import nursing home data
cbs_nursing <- cbs_get_data("82887NED")
cbs_nursing_meta <- cbs_get_meta("82887NED")

pop_nursing <- tidy_popproj |> 
  filter(year == 2024) |>
  mutate(min_age = case_when(
    age >= 95 ~ 95,
    age < 95 ~ age
  )) |> 
  group_by(min_age) |> 
  summarise(pop = sum(population))

tidy_nursing <- cbs_nursing |> 
  left_join(cbs_nursing_meta$Leeftijd, by = c("Leeftijd" = "Key")) |> 
  filter(Perioden == max(Perioden),
         Geslacht == "T001038",
         CategoryGroupID == 3) |> 
  mutate(min_age = case_when(
    Title == "95 jaar of ouder" ~ as.integer(95),
    .default = as.integer(stringr::str_extract(Title, "(\\d)+"))
  ),
  max_age = case_when(
    Title == "95 jaar of ouder" ~ as.integer(100),
    .default = as.integer(stringr::str_extract(Title, "(\\d)+"))
  )) |> 
  left_join(pop_nursing, by = "min_age") |> 
  mutate(prop_nursing = VerzorgingsEnVerpleeghuis_7 / pop) |> 
  select(min_age, max_age, prop_nursing)

arrow::write_dataset(tidy_nursing, path = "II.Data/demographics/nursing_home")

# import data risk stratification
pop_edit <- tidy_popproj |> 
  filter(year == 2024,
         age >= 60) |> 
  mutate(min_age = case_when(
    a
  ))
# data from Nivel Monitor Vaccinatiegraad NPG 2023 v20240916.pdf
high_risk <- tibble(
  min_age = c(60, 65, 70, 75, 80),
  max_age = c(64, 69, 74, 79, 99),
  n_med_ind = c(24789, 27285, 28640, 31826, 33553),
  n_no_med_ind = c(54319, 38828, 28815, 21796, 14429)
) |> 
  mutate(total = n_med_ind + n_no_med_ind,
         prop = n_med_ind / total)

arrow::write_dataset(high_risk, path = "II.Data/demographics/high_risk_population")
  
