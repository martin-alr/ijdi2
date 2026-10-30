###
# revison
###

#load libraries
# 2. Instalar el backend de CmdStan (descarga el ejecutable ligero de C++)
#check_cmdstan_toolchain(fix = TRUE)
#install_cmdstan()

# 3. Le decimos a brms que use cmdstanr en lugar de rstan (¡Adiós al error de sink!)
library(tidyverse)
library(DBI)
library(duckdb)
library(brms)
library(cmdstanr)

# backend
options(brms.backend = "cmdstanr")

#load data
db_path <- "data/results.db"

# 2. Conectarse en modo lectura a DuckDB
con <- 
  dbConnect(duckdb::duckdb(), 
            dbdir = db_path, 
            read_only = TRUE)

# 3. Traer la tabla completa a un data.frame (tibble) en la memoria de R
df_panel <- 
  dbGetQuery(con, "SELECT * FROM panel_maestro") 

# 4. Cerrar la conexión correctamente
dbDisconnect(con, shutdown = TRUE)

# 5. Verificamos que ya es un data.frame listo para analizar
calories <-
  df_panel |> 
  tibble() |> 
  mutate(iso3c = factor(iso3c),
  country = factor(area),
  income_group = factor(income_group)) |> 
  select(1, country, 3:ncol(df_panel))

#glimpse(calories)

### models
#year and country FE
m1 <- 
  brm(
   formula = log(agr) ~ log(pop) * income_group + 
                        log(cal_cer) * income_group + 
                        log(cal_ns) * income_group + 
                        (1 | country) + (1 | year),
   data = calories, 
   family = gaussian(),
   chains = 4,           # Ponle 2 cadenas mientras pruebas para que sea más rápido
   iter = 4000, 
   warmup = 2000,
   seed = 85,
   cores = 4,
   control = list(max_treedepth = 12)
 )

#no year
m2 <- brm(
  formula = log(agr) ~ log(pop) * income_group + 
                       log(cal_cer) * income_group + 
                       log(cal_ns) * income_group + 
                       (1 | country),
  data = calories, 
  family = gaussian(),
  chains = 4,       
  iter = 4000, 
  warmup = 2000,
  seed = 85,
  cores = 4,
  control = list(max_treedepth = 12)
)
# Para ver el resumen de los coeficientes por grupo de ingreso
summary(m2)

#save model coefficents
broom::tidy(m2)

### results data frame
#
draws <- 
  as_draws_df(m2) |> 
  as_tibble()

#pop
pop <- 
  draws %>%
  transmute(
    .draw = 1:n(),
    ig1 = b_logpop,
    ig2 = b_logpop + `b_logpop:income_group2`,
    ig3 = b_logpop + `b_logpop:income_group3`,
    ig4 = b_logpop + `b_logpop:income_group4`,
    ig5 = b_logpop + `b_logpop:income_group5`,
    ig6 = b_logpop + `b_logpop:income_group6`,
    ig7 = b_logpop + `b_logpop:income_group7`,
    ig8 = b_logpop + `b_logpop:income_group8`,
    ig9 = b_logpop + `b_logpop:income_group9`
  ) %>%
  pivot_longer(-.draw, names_to = "income_group", values_to = "effect")

#pop sumarise
pop |> 
  group_by(income_group) |> 
  summarise(
    variable = "pop",
    mean = mean(effect),
    lower = quantile(effect, 0.05),
    upper = quantile(effect, 0.95)
  )

#staples
staples <- 
  draws %>%
  transmute(
    .draw = 1:n(),
    ig1 = b_logcal_cer,
    ig2 = b_logcal_cer + `b_income_group2:logcal_cer`,
    ig3 = b_logcal_cer + `b_income_group3:logcal_cer`,
    ig4 = b_logcal_cer + `b_income_group4:logcal_cer`,
    ig5 = b_logcal_cer + `b_income_group5:logcal_cer`,
    ig6 = b_logcal_cer + `b_income_group6:logcal_cer`,
    ig7 = b_logcal_cer + `b_income_group7:logcal_cer`,
    ig8 = b_logcal_cer + `b_income_group8:logcal_cer`,
    ig9 = b_logcal_cer + `b_income_group9:logcal_cer`
  ) %>%
  pivot_longer(-.draw, names_to = "income_group", values_to = "effect")

staples |> 
  group_by(income_group) |> 
  summarise(
    variable = "staples",
    mean = mean(effect),
    lower = quantile(effect, 0.05),
    upper = quantile(effect, 0.95)
  )

## non staples
non_staples <- 
  draws %>%
  transmute(
    .draw = 1:n(),
    ig1 = b_logcal_ns,
    ig2 = b_logcal_ns + `b_income_group2:logcal_ns`,
    ig3 = b_logcal_ns + `b_income_group3:logcal_ns`,
    ig4 = b_logcal_ns + `b_income_group4:logcal_ns`,
    ig5 = b_logcal_ns + `b_income_group5:logcal_ns`,
    ig6 = b_logcal_ns + `b_income_group6:logcal_ns`,
    ig7 = b_logcal_ns + `b_income_group7:logcal_ns`,
    ig8 = b_logcal_ns + `b_income_group8:logcal_ns`,
    ig9 = b_logcal_ns + `b_income_group9:logcal_ns`
  ) %>%
  pivot_longer(-.draw, names_to = "income_group", values_to = "effect")

#summarise df
non_staples |> 
  group_by(income_group) |> 
  summarise(
    variable = "non_staples",
    mean = mean(effect),
    lower = quantile(effect, 0.05),
    upper = quantile(effect, 0.95)
  )

#data frame results
# 1. Opcional pero recomendado: unificar los tres data frames de iteraciones en uno solo usando una columna de tipo/variable
pop_long <- pop |> mutate(variable = "pop")
staples_long <- staples |> mutate(variable = "staples")
non_staples_long <- non_staples |> mutate(variable = "non_staples")

# Combinar todo en una gran tabla "tidy" con los 8,000 draws por cada grupo y variable
res <- bind_rows(pop_long, staples_long, non_staples_long)

# 2. Guardar el archivo maestro de iteraciones (y si quieres también el modelo m2)
save(res, m2, file = "res.RData")
