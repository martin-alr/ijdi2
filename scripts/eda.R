###
# EDA
###

#load packages
library(tidyverse)
library(ggpubr)
library(ggeasy)

## 2. load data ##
calories_raw <-
  readRDS("data/data_updated.rds")


#data types and variables names
calories <-
  calories_raw |> 
  #tibble() |> 
  mutate(iso3c = factor(iso3c),
  country = factor(area),
  income_group = factor(income_group)) |> 
  select(1, country, 3:ncol(calories)) |> 
  drop_na()

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

ggsave("f1.jpg",
        units="cm",
        width=12,
        height=12)

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

#save figure
ggsave("f2.jpg",
       units="cm",
       width=12,
       height=12)

