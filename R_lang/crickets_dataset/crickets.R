library(tidyverse)
library(modeldata)

glimpse(crickets)


ggplot(crickets, aes(x=temp, y=rate, color=species)) +
  geom_boxplot() +
  labs(
    x = "Temperature",
    y = "Chirp Rate",
    title = "Chirp Rate to Temperature",
    color = "Species"
  )+
  scale_color_brewer(palette="Dark2")+
  theme_minimal()

ggplot(crickets, aes(x=temp, y=rate, color=species))+
  geom_point(size = 1.5, alpha = 1, shape = "circle")+
  geom_smooth(method = "lm", se = FALSE,linewidth = 0.5)+
  labs(
    x = "Temperature",
    y = "Chirp Rate",
    title = "Chirp Rate to Temperature",
    color = "Species"
  )+
  scale_color_brewer(palette="Dark2")+
  theme_minimal()


ggplot(crickets, aes(x=rate, fill=species))+
  geom_histogram()+
  labs(
    x = "Chirp Rate",
    y = "Count",
    title = "Chirp Rate count",
    fill = "Species",
  )+
  scale_color_brewer(palette="Dark2")+
  theme_minimal()

ggplot(crickets, aes(x=temp, fill=species))+
  geom_histogram(bins = 15)+
  labs(
    x = "Temp",
    y = "Count",
    title = "Temp count",
    fill = "Species",
  )+
  facet_wrap(~species, ncol=1)+
  scale_color_brewer(palette="Dark2")+
  theme_minimal()

ggplot(crickets, aes(x=species, fill=species))+
  geom_bar(width = 0.5)+
  labs(
    x = "species",
    y = "Count",
    title = "Data count of Species",
    fill = "Species",
  )+
  scale_color_brewer(palette="Dark2")+
  theme_minimal()

