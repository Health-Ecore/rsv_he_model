# Zorg ervoor dat de benodigde bibliotheken zijn geladen
library(ggplot2)
library(dplyr)

# Gegevens inladen
VE_Zero <- 48
Duration_VE_first_interval <- 12

data_non_specific <- data.frame(
  RSV_Outcome = c("ARI", "ARI", "Moderate LRTI", "Moderate LRTI", "Hospitalization", "Hospitalization", "Severe LRTI", "Severe LRTI"),
  median_follow_up = c(Duration_VE_first_interval, VE_Zero, Duration_VE_first_interval, VE_Zero, Duration_VE_first_interval, VE_Zero, Duration_VE_first_interval, VE_Zero),
  vaccine_efficacy_mid = c(68.0, 0, 78.3, 0, 79, 0, 86.5, 0),
  lower_ci = c(58.5, 0, 65.6, 0, 56, 0, 68.3, 0),
  upper_ci = c(75.3, 0, 86.3, 0, 90, 0, 94.3, 0)
)

# Sigmoidale functie
sigmoid <- function(x, L, x0, k) {
  L / (1 + exp(k * (x - x0)))
}

# Lijst voor plots
plot_list <- list()
ci_list <- list()

# Loop over elke unieke uitkomst
for (outcome in unique(data_non_specific$RSV_Outcome)) {
  # Filter data voor de huidige uitkomst
  outcome_data <- data_non_specific %>% filter(RSV_Outcome == outcome)
  
  # Bereken L en x0 voor vaccine efficacy
  L <- max(outcome_data$vaccine_efficacy_mid)  # Beginwaarde is de maximale VE
  x0 <- (Duration_VE_first_interval + (0.5 * (VE_Zero - Duration_VE_first_interval))) # Berekening voor x0
  
  # Parameter k
  k <- (Duration_VE_first_interval / VE_Zero) # Steilheid van de curve (kan worden aangepast)
  
  # time (maanden)
  time <- seq(0, VE_Zero, by = 1)  # Stappen van 1 maand
  
  # Bereken de sigmoidale waarden voor vaccine efficacy
  VE <- sigmoid(time, L, x0, k)
  
  # Dataframe maken voor plotten
  data <- data.frame(time, VE)
  data$RSV_Outcome <- outcome  # Voeg de uitkomst toe aan de dataframe
  plot_list[[outcome]] <- data
  
  # Bereken L en x0 voor lower CI
  L_lower <- max(outcome_data$lower_ci)  # Beginwaarde voor lower CI
  VE_lower <- sigmoid(time, L_lower, x0, k)  # Sigmoidale waarden voor lower CI
  
  # Bereken L en x0 voor upper CI
  L_upper <- max(outcome_data$upper_ci)  # Beginwaarde voor upper CI
  VE_upper <- sigmoid(time, L_upper, x0, k)  # Sigmoidale waarden voor upper CI
  
  # Dataframe maken voor CI
  ci_data <- data.frame(time, VE_lower, VE_upper)
  ci_data$RSV_Outcome <- outcome  # Voeg de uitkomst toe aan de dataframe
  ci_list[[outcome]] <- ci_data
}

# Combineer alle dataframes in één
combined_data <- do.call(rbind, lapply(names(plot_list), function(outcome) {
  df <- plot_list[[outcome]]
  return(df)
}))

# Combineer CI dataframes
combined_ci_data <- do.call(rbind, lapply(names(ci_list), function(outcome) {
  df <- ci_list[[outcome]]
  return(df)
}))

# Voeg CI data toe aan de gecombineerde data
combined_data <- combined_data %>%
  left_join(combined_ci_data, by = c("time", "RSV_Outcome"))

# Bereken SE per maand
combined_data <- combined_data %>%
  mutate(SE = (VE_upper - VE_lower) / (2 * 1.96))  # Aannemende dat SE kan worden berekend uit de CI

# Plot de sigmoidale curves met onzekerheid
p <- ggplot() +
  geom_line(data = combined_data, aes(x = time, y = VE, color = RSV_Outcome)) +
  geom_ribbon(data = combined_data, aes(x = time, ymin = VE_lower, ymax = VE_upper, fill = RSV_Outcome), alpha = 0.2) +
  geom_point(data = data_non_specific, aes(x = median_follow_up, y = vaccine_efficacy_mid), color = "black", size = 1) +
  labs(title = "Vaccine effectiveness non-specific RSV vaccine",
       x = "Time (months)",
       y = "Vaccine Efficacy (%)")+
  theme_minimal() +
  ylim(0, 100)  # Limiet voor y-as

print(p)

# Print de SE per maand
print(combined_data %>% select(time, RSV_Outcome, VE, SE))
