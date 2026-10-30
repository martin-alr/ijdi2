###
# EDA
###

#load packages
library(tidyverse)
library(DBI)
library(duckdb)
library(ggpubr)

# load data
#load data
db_path <- "data/results.db"

# 2. Conectarse en modo lectura a DuckDB
con <- dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE)

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

#custom color
#set theme
theme_set(theme_classic())

# custom colors
custom_colors <- 
  c("#D55E00", 
    "#0072B2", 
    "#CC79A7")

## figure 1 ##
calories |> 
  ggplot(aes(log(pop), log(agr))) + 
  geom_point(alpha = 0.25,
             color = custom_colors[1]) +
  facet_wrap(vars(income_group)) +
  #geom_smooth(method = "lm", se = F) +
  scale_y_continuous(limits = c(0, 15),
                     breaks = seq(0, 15, by = 5)) +
  labs(x = "Population (millions) log scale",
       y = expression(paste("Agricultural land (", km^{2},") log scale"))) +
  stat_cor(aes(label = after_stat(r.label)),
         label.x.npc = 0.6,
         label.y.npc = 0.15,
         size = 3) +
  #scale_color_manual(values = viridis(3)[1]) +
  easy_all_text_size(size = 10)

ggsave("f1.jpg",units="cm", width=12, height=12)

## figure 2 ##
#data frame
calories |> 
  select(1, 2, income_group, 4, cal_cer, cal_ns) |> 
  pivot_longer(starts_with("cal"),
               names_to = "source",
               values_to = "cal") |> 
  ggplot(aes(log(cal), log(agr), col = source)) +
  geom_point(alpha = 0.25) +
  facet_wrap(vars(income_group)) +
  scale_y_continuous(limits = c(0, 15), 
                     breaks = seq(0, 15, by = 5)) +
  #geom_smooth(method = "lm", se = F) +
  labs(x = "Calorie intake (cal/person/day) log scale",
       y = expression(paste("Agricultural land (", km^{2},") log scale"))) +
  stat_cor(aes(label = ..r.label..),
           label.x.npc = 0.10,
           label.y.npc = 0.30,
           size = 2,
           show.legend = F) +
  scale_colour_manual(labels = c("Staples", "Non-staples"),
                      values = custom_colors[2:3]) +
  theme(legend.position = "bottom") +
  easy_all_text_size(size = 10) +
  guides(colour = guide_legend(override.aes = list(alpha = 0.8)))
