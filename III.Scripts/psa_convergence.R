## Load packages -----------------------------------------------------------
box::use(dplyr[...])
box::use(purrr[...])
box::use(gt[...])
box::use(stringr[...])
box::use(ggplot2[...])

## Load custom modules -----------------------------------------------------
options(box.path = getwd())
box::purge_cache()
box::use(IV.Functions/currency_cbs)
box::use(IV.Functions/demographics)
box::use(IV.Functions/reference_prices)
box::use(IV.Functions/discounting)
box::use(IV.Functions/vaccine)
box::use(IV.Functions/rsv_dectree)
box::use(IV.Functions/record_results)

set.seed(2025)

save_name <- "convergence_feb21"
save_path <- stringr::str_c("V. Output/results/", save_name)
## Core settings -----------------------------------------------------------
probabilistic <- TRUE
n_cores <- 20
settings <- rsv_dectree$create_settings()



batch <- 1:10

for(b in batch){
  rsv_dectree$run(
    settings = settings,
    probabilistic = probabilistic, 
    iter = 1000,
    n_cores = n_cores,
    ret = "summarise"
  ) |> record_results$save(save_path, stringr::str_c(b))
  
}

res_ref <- arrow::open_dataset(save_path) |> 
  filter(strategy == "no_vaccine") |> 
  mutate(batch = as.double(vac_strategy),
         iter = iter + (batch-1)*1000) |> 
  select(iter, c_ref = costs, q_ref = qalys) |> 
  collect()
res_vac <- arrow::open_dataset(save_path) |> 
  filter(strategy == "rsv_generic_vaccine") |> 
  mutate(batch = as.double(vac_strategy),
         iter = iter + (batch-1)*1000) |> 
  select(iter, c_int = costs, q_int = qalys) |> 
  collect()

res_tot <- left_join(res_ref, res_vac, by = "iter") |> 
  mutate(c_dif = c_int - c_ref,
         q_dif = q_int - q_ref,
         icer = c_dif / q_dif) |> 
  select(iter, q_dif, c_dif)




results <- tibble(n_iterations = c(1:10000)) |> 
  mutate(ce = map(n_iterations, function(n_it, data, settings){
    data <- data |> 
      filter(iter <= n_it) |> 
      summarise(
        c_mean = mean(c_dif),
        q_mean = mean(q_dif),
        icer = c_mean / q_mean
      )
    
    return(data)
        
    
  }, res_tot, settings))


conv <- results |> 
  tidyr::unnest(ce)

conv |> 
  ggplot(aes(x = n_iterations, y = icer)) +
  geom_line()

conv |> 
  ggplot(aes(x = n_iterations, y = c_mean)) +
  geom_line()

conv |> 
  ggplot(aes(x = n_iterations, y = q_mean)) +
  geom_line()
