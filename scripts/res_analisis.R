###
#Results
##

#libraries
library(tidyverse)
library(tidybayes)
library(ggeasy)

#load data
load("res.RData")
glimpse(res)

#data cleaning
results <-
  res |> 
  mutate(ig = factor(income_group),
         var = factor(variable)) |> 
  select(1, ig, var, effect)

#plot theme
theme_set(theme_classic())

## fig:3 population elasticities ##
#data frame
pop <-
  results |> 
  filter(var == "pop")

#pop plot
pop |> 
  ggplot(aes(ig, effect,
             fill = after_stat(y > 0))) +
  stat_halfeye(.width = c(.80, .95)) +
  geom_hline(yintercept = 0, lty = 2) +
  labs(#title = "Population elasticity",
       x = "Income group",
       y = "Elasticity (%)") +
  scale_fill_manual(values = c("gray80", "skyblue")) +
  theme(legend.position = "none") +
  easy_all_text_size(size = 10)

#save figure
ggsave("f3.jpg", units="cm", width = 6, height= 6)

## fig:4 staples and nonstaples ##
#data frame
food_df <-
  results |> 
  filter(var %in% c("staples", "non_staples"))

#plot
food_df |> 
  ggplot(aes(ig, effect,
             fill = after_stat(y > 0))) +
  facet_wrap(vars(var)) +
  stat_halfeye(.width = c(.80, .95)) +
  geom_hline(yintercept = 0, lty = 2) +
  labs(x = "Income group",
       y = "Elasticity (%)") +
  scale_fill_manual(values = c("gray80", "skyblue")) +
  theme(legend.position = "none") +
  easy_all_text_size(size = 15)

#save image
ggsave("f4.jpg", units="cm", width = 6, height= 6)

## f5: deforestation sources

#df growth rates
growth_rates <- 
  readRDS("growth_rates.rds")

#groth long
df_growth_long <- 
  growth_rates %>%
  pivot_longer(
    cols = c(mean_pgr, mean_sgr, mean_nsgr),
    names_to = "tipo_crecimiento",
    values_to = "growth_rate"
  ) %>%
  mutate(
    var = case_when(
      tipo_crecimiento == "mean_pgr"  ~ "pop",
      tipo_crecimiento == "mean_sgr"  ~ "staples",
      tipo_crecimiento == "mean_nsgr" ~ "non_staples"
    )
  ) %>%
  select(ig, var, growth_rate)

#join data frame
df <-
  results |> 
  left_join(df_growth_long, by = c("ig", "var")) |> 
  mutate(
    effect_s = effect * growth_rate,
    source = factor(var, 
                    levels = c("pop", "staples", "non_staples"))
  )

#custom colors
custom_colors <- 
  c("#D55E00", 
    "#0072B2", 
    "#CC79A7")
#plot
df |> 
  # plot axis
  ggplot(aes(effect_s, 
         #col = source, 
         fill = source)) +
  #facets
  facet_wrap(vars(ig), 
             scales = "free_y") +
  #geometry
  geom_histogram(alpha = 0.5, 
                 bins = 300) +
  #reference line
  geom_vline(xintercept = 0,
             lty = 2) +
  #custom color
  scale_fill_manual(labels = c("population","staples","non-staples"),
                    values = custom_colors) +
  # axis and legend appeareance
  labs(y = " ",
       x = "Agricultural land expansion (%)") +
  easy_remove_y_axis() +
  easy_legend_at("bottom")

#save image
ggsave("f5.jpg",units="cm", width = 12, height = 12)
