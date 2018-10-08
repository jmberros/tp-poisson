## ------------------------------------------------------------------------
library(MASS, pos = "package:base") # WARNING: Import before tidyverse to keep dplyr's `select`
library(tidyverse)
library(lubridate)
library(glue)
library(here)
library(knitr)

## ------------------------------------------------------------------------
read_dataset <- function(fn) {
  read_csv(fn) %>%
  rename(
    incident_type = CFD_INCIDENT_TYPE_GROUP,
    incident_time = CREATE_TIME_INCIDENT) %>%
    # Reemplazamos NA con "NULL" para poder seleccionar los incident_type con
    # valor faltante más tarde:
    mutate(incident_type = map_chr(incident_type,
                                   ~ifelse(is.na(.x), "NULL", .x)))  
}

count_incidents_per_type <- function(df) {
  df %>% count(incident_type) %>% arrange(-n)
}

fn <- here("data/Cincinnati_Fire_Incidents__CAD___including_EMS__ALS_BLS_.csv")
full_incidents <- read_dataset(fn)
n_per_incident_type <- count_incidents_per_type(full_incidents)

# Uncomment to print the list of incident types and their frequency:
# kable(n_per_incident_type) 

## ------------------------------------------------------------------------
keep_incidents_of_type <- function(chosen_type) {
  full_incidents %>%
    select(incident_type, incident_time) %>%
    filter(incident_type == chosen_type) %>%
    select(incident_time) %>%
    mutate(incident_time = as.POSIXct(incident_time,
                                      format = "%m/%d/%Y %I:%M:%S %p",
                                      tz = "UTC")) %>%
    mutate(incident_day = floor_date(incident_time, "day"))
}

compare_poisson_and_bn_for_incident_type <- function(chosen_incident_type) {
  chosen_incidents <- keep_incidents_of_type(chosen_incident_type)

  daily_incidents_count <-
    chosen_incidents %>%
    count(incident_day) %>%
    select(n)

  # Agregamos los días sin llamadas (días que no aparecen en el dataset)
  # como filas de ceros:
  max_day <- max(chosen_incidents$incident_day)
  min_day <- min(chosen_incidents$incident_day)
  total_days <- max(as.integer(max_day - min_day), nrow(daily_incidents_count))
  missing_days <- total_days - nrow(daily_incidents_count)
  
  daily_incidents_count <-
    rbind(tibble(n = rep(0, missing_days)), daily_incidents_count) %>%
    mutate(n = as.integer(n))

  # Frecuencia de frecuencias: contamos cuántos días tienen 0 llamadas,
  # cuántos días tienen 1 llamada, ...
  daily_incidents_freq <-
    daily_incidents_count %>%
      count(n) %>%
      rename(count = nn) %>%
      mutate(
        density = count / nrow(daily_incidents_count),
        density_type = "empirical") %>%
      select(n, density_type, density)

  # Safety check:
  assertthat::assert_that((near(sum(daily_incidents_freq$density), 1)))
  
  # Asumiendo distribución Poisson, estimamos el parámetro como la media muestral:
  lambda_estimate <- mean(daily_incidents_count$n)

  # Ajustamos el número de incidentes diario con la distribución Binomial Negativa
  # para comparar con la Poisson:
  fit <- fitdistr(daily_incidents_count$n, densfun = "negative binomial")
  bn_size_estimate <- fit$estimate[["size"]]
  # Obs: El parámetro "mu" de la parametrización alternativa de la distribución
  # BN se estima igual que el lambda de Poisson, con la media muestral.
  bn_mu_estimate <- lambda_estimate

  max_n <- max(daily_incidents_count$n)
  min_n <- min(daily_incidents_count$n)

  # Juntamos las densidades esperadas por Poisson y BN para cada n observado
  # para luego comparar con la densidad empírica:
  compute_density <- function(n, density_type, density_function) {
    if (density_type == "poisson") {
      density_function(n, lambda = lambda_estimate)
    } else if (density_type == "negative_binomial") {
      density_function(n, size = bn_size_estimate, mu = bn_mu_estimate)
    } else {
      stop(glue("I don't know how to compute density for type: {density_type}"))
    }
  }
  
  probabilities_per_n <-
    cross_df(list(
    n = seq(min_n, max_n),
    density_type = c("poisson", "negative_binomial"))) %>%
    mutate(
      density_function = map(density_type,
                             ~ifelse(.x == "poisson", dpois, dnbinom)),
      density = pmap_dbl(list(n, density_type, density_function),
                         compute_density)) %>%
    select(n, density_type, density)
  
  probabilities_per_n <- rbind(probabilities_per_n, daily_incidents_freq)
  
  # Sumamos las curvas de densidad Poisson y BN al gráfico de la empírica:
  poisson_color <- "ForestGreen"
  bn_color <- "IndianRed"
  empirical_color <- "#444444"
  
  fig <-
    probabilities_per_n %>%
    ggplot() +
    aes(x = n, y = density, color = density_type) +
    geom_point(alpha = .7) +
    geom_line() +
    scale_color_manual(values = c(empirical_color, bn_color, poisson_color)) +
    labs(
      title = glue("Número de '{chosen_incident_type}' por día"),
      subtitle = "Ajuste de la distribución empírica con Poisson y Binomial Negativa",
      y = "Densidad",
      x = glue("Número de llamadas"))
  
  return (list(
    probabilities_per_n = probabilities_per_n,
    fig = fig
  ))
}

## ------------------------------------------------------------------------
for (incident_type in n_per_incident_type$incident_type) {
  print(glue("Working with: {incident_type}"))
  comparison <- compare_poisson_and_bn_for_incident_type(incident_type)
  
  # Sanitize the incident type for the plot filename:
  name <- str_to_lower(incident_type)
  name <- str_replace_all(name, " ", "_")
  name <- str_replace_all(name, "/", "-")
  
  filename <- here(glue("results/{name}.png"))
  ggsave(filename, comparison$fig, width = 10, height = 6)
  
  print(comparison$fig)
}

