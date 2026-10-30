#Figures

#Results
#save data set
#saveRDS(def_rate, file = "results.Rds")
#Load growth_rates and  results

#clean workspace
#rm(list=ls())

#load libraries
library(tidyverse)
library(ggeasy)
library(tidybayes)
library(bayesplot)
library(ggpubr)
library(viridis)

#load data (file: results.RDS, growth_rates.rds, calories)
load("calories.RData")
results <- readRDS("results.rds")
growth_rates <- readRDS("growth_rates.rds")

#set theme
theme_set(theme_classic())

# custom colors
custom_colors <- 
  c("#D55E00", 
    "#0072B2", 
    "#CC79A7")

#labels for incpme groups
label_ig <- c(
  "1" = "ig1",
  "2" = "ig2",
  "3" = "ig3",
  "4" = "ig4",
  "5" = "ig5",
  "6" = "ig6",
  "7" = "ig7",
  "8" = "ig8",
  "9" = "ig9"
)

#Figure 1
calories |> 
  ggplot(aes(log(pop), log(agr))) + 
  geom_point(alpha = 0.25,
             color = custom_colors[1]) +
  facet_wrap(~ income_group, labeller = as_labeller(label_ig)) +
  #geom_smooth(method = "lm", se = F) +
  scale_y_continuous(limits = c(0, 15),
                     breaks = seq(0, 15, by = 5)) +
  labs(x = "Population (millions) log scale",
       y = expression(paste("Agricultural land (", km^{2},") log scale"))) +
  stat_cor(aes(label = after_stat(r.label)),
         label.x.npc = 0.5,
         label.y.npc = 0.15,
         size = 3) +
  #scale_color_manual(values = viridis(3)[1]) +
  easy_all_text_size(size = 10)

ggsave("f1.jpg",units="cm", width=12, height=12)

#diet (Figure 2)
calories |> 
  select(1, 2, income_group, 4, cal_cer, cal_ns) |> 
  pivot_longer(starts_with("cal"),
               names_to = "source",
               values_to = "cal") |> 
  ggplot(aes(log(cal), log(agr), col = source)) +
  geom_point(alpha = 0.25) +
  facet_wrap(~ income_group, labeller = as_labeller(label_ig)) +
  scale_y_continuous(limits = c(0, 15), 
                     breaks = seq(0, 15, by = 5)) +
  #geom_smooth(method = "lm", se = F) +
  labs(x = "Calorie intake (cal/person/day) log scale",
       y = expression(paste("Agricultural land (", km^{2},") log scale"))) +
  stat_cor(aes(label = ..r.label..),
           label.x.npc = 0.10,
           label.y.npc = 0.30,
           size = 1.5,
           show.legend = F) +
  scale_colour_manual(labels = c("Staples", "Non-staples"),
                      values = custom_colors[2:3]) +
  theme(legend.position = "bottom") +
  easy_all_text_size(size = 10) +
  guides(colour = guide_legend(override.aes = list(alpha = 0.8)))
  
ggsave("f2.jpg",units="cm", width=12, height=12)

# Figure 3: Population elasticity
results |> 
  ggplot(aes(ig, el_pop,
             fill = stat(y > 0))) +
  stat_halfeye(.width = c(.80, .95)) +
  labs(#title = "Population elasticity",
       x = "Income group",
       y = "Elasticity (%)") +
  scale_fill_manual(values = c("skyblue", "gray80")) +
  theme(legend.position = "none") +
  easy_all_text_size(size = 10)

#ggsave("f3.jpg",units="cm", width=6, height=6)


# Figure 4: staples and nonstaples elasticities
results |> 
  select(ig, el_staples, el_nonstaples) |> 
  rename("Staples" = el_staples,
         "Non-staples" = el_nonstaples) |> 
  pivot_longer(-ig,
               names_to = "source",
               values_to = "elasticity") |> 
  mutate(source = factor(source,
                         levels = c("Staples",
                                    "Non-staples"))) |> 
  arrange(ig, source) |> 
  ggplot(aes(ig, elasticity, fill = stat(y > 0))) +
  stat_halfeye(.width = c(.80, .95)) +
  facet_wrap(~source) +
  hline_0(lty = 2) +
  labs(x = "Income group",
       y = "Elasticity (%)") +
  scale_fill_manual(values = c("gray80", "skyblue")) +
  theme(legend.position = "none") +
  easy_all_text_size(size = 15)

#ggsave("f4.jpg", units="cm", width = 6, height= 6)

#compute deforestation by source 
df <- 
  results |> 
  left_join(growth_rates) |> 
  mutate(population = el_pop * mean_pgr,
         staples = el_staples * mean_sgr,
         nonstaples = el_nonstaples + mean_nsgr,
         total = population + staples + nonstaples)

#plot deforestation drivers
label_ig <- c(
  ig1="1",
  ig2="2",
  ig3="3",
  ig4="4",
  ig5="5",
  ig6="6",
  ig7="7",
  ig8="8",
  ig9="9"
)

#plot f5
df |> 
  select(ig, population, staples, nonstaples, total) |> 
  #mutate(dr_tot = dr_pop + dr_diet) |> 
  pivot_longer(-ig, names_to = "source", values_to = "def_rate") |> 
  mutate(source = factor(source, 
                         levels = c("population",
                                    "staples",
                                    "nonstaples",
                                    "total"))) |> 
  filter(source != "total") |> 
  ggplot(aes(def_rate, fill = source)) +
  geom_histogram(alpha = 0.5, 
                 bins = 300) +
  labs(y = " ",
       x = "Agricultural land expansion (%)") +
  facet_wrap(~ ig,
             scales = "free_y",
             labeller = as_labeller(label_ig)) + 
  geom_vline(xintercept = 0, lty=2) +
  theme(legend.position = "bottom",
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        text=element_text(size=10)) +
  scale_fill_manual(labels = c("population","staples","non-staples"),
                    values = custom_colors) +
  easy_all_text_size(size = 10)
  
ggsave("f5.jpg",units="cm", width = 12, height = 12)

# probabilities
results |> 
  group_by(ig) |> 
  mean_qi(el_staples, .width = 0.8)

df |> 
  mutate(prop = dr_tot > 0) |> 
  group_by(ig) |> 
    summarise(pr = mean(prop))

# diet composition vs income
calories |> 
  select(income_group, area, gdp, cal_cer, cal_ns) |> 
  pivot_longer(c(cal_cer, cal_ns)) |> 
  ggplot(aes(gdp, value, col = income_group)) +
  geom_point(alpha = 0.5) +
  scale_color_viridis_d() +
  facet_wrap(~name)

#countries per group
low_income <-
  calories |> 
  mutate(ig = income_group |> as.double()) |> 
  filter(income_group %in% c(7,8,9)) |> 
  group_by(area) |> 
  summarise(year_min = min(year),
            year_max = max(year),
            min_ig = min(ig),
            max_ig = max(ig)) |> 
  mutate(period = if_else(year_min == 2002 & year_max == 2018,
                          "The whole period",
                          paste0(year_min, "-", year_max))) |> 
  select(1,6,4,5)

write.table(low_income, "income_h.txt",
            sep = ",", quote = FALSE, row.names = F)

#mean income 
calories |> 
  group_by(income_group) |> 
  summarise(mean_gdp = mean(gdp))

#Population per income group
calories |> 
  filter(year == max(year)) |> 
  group_by(income_group) |> 
  summarise(pop = sum(pop)) |> 
  mutate(pop_pr = (pop / sum(pop)) * 100,
         cum_pop = cumsum(pop_pr)) |> 
  select(3,4) |> 
  round(1)

#income growth rate
calories |> 
  group_by(area) |>   
  mutate(egr = (gdp/lag(gdp) -1) * 100) |> 
  select(income_group, area, year, egr) |> 
  ungroup() |> 
  group_by(income_group) |> 
  summarise(mean_egr = mean(egr, na.rm = T, trim = 0.1) |> round(2)) 
  

