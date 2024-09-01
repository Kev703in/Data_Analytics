library(tidyverse)

data()
view(mpg)
?mpg
glimpse(mpg)

mpg_20 <- filter(mpg , cty >= 20)
view(mpg_20)

mpg_ford <- filter(mpg, manufacturer == "ford")
view(mpg_ford)

mpg_20_ford <- filter(mpg, manufacturer == "ford" & cty >= 20)
view(mpg_20_ford)
# no output so delete the table data
rm(mpg_20_ford)

mpg_15_ford <- filter(mpg, manufacturer == "ford" & cty >= 20)
view(mpg_15_ford)

# add new column 
mpg_metric <- mutate(mpg, cty_metric = 0.425144 * cty)
glimpse(mpg_metric)


# pipe multiple cmds (basically does not need first argument) ( ctrl+ shift + m)
mpg_piped <- mpg %>%
  mutate(cty_metric = 0.425144 * cty) %>% 
  filter(manufacturer == "ford") %>% 
  filter(cty >= 15)

view(mpg_piped)

mpg %>% 
  group_by(class) %>% 
  summarise(mean(cty), median(cty))

ggplot(mpg, aes(x=cty)) +
  geom_histogram() +
  labs(x = "City mileage")

ggplot(mpg, aes(x=cty)) +
  geom_histogram() +
  geom_freqpoly() +
  labs(x = "City mileage")

ggplot(mpg, aes(x=cty, y=hwy, colour = class)) +
  geom_point() +
  labs(x = "City mileage")

ggplot(mpg, aes(x=cty, y=hwy, colour = class)) +
  geom_point() +
  geom_smooth() +
  scale_color_brewer(palette = "Dark2")
  labs(x = "City mileage")

ggplot(mpg, aes(x=cty, colour = class)) +
  geom_bar() +
  labs(x = "City mileage")


mpg_trans <- mpg %>%
  select(trans, cty, hwy) %>% 
  group_by(trans) %>%
  arrange(trans) %>% 
  summarize(mean_cty = mean(cty), mean_hwy = mean(hwy))

ggplot(mpg_trans)+
  geom_bar( aes(x=trans, y=mean_hwy, fill="hwy" ), stat = "identity", position = "dodge", width = 0.4)+
  geom_bar( aes(x=trans, y=mean_cty, fill ="cty"), stat = "identity", position = "dodge", width = 0.4)+
  labs(title = "Mean City and Highway Mileage by Transmission Type",
       x = "Transmission",
       y = "Mean Mileage",
       fill = "Metric") +
  scale_fill_manual(values = c("cty" = "blue", "hwy" = "red")) +
  theme_minimal()


mpg_classes <- mpg %>% 
  group_by(class, manufacturer) %>%
  count() %>% 
  rename(count_number = n)


ggplot(mpg_classes) +
  geom_bar( aes(x=manufacturer, y=count_number, fill=class), stat="identity", position = "stack" )+
  labs(title = "Count of Cars by Manufacturer and Class",
       x = "Manufacturer",
       y = "Count",
       fill = "Class") +
  theme_minimal()
            

ggplot(mpg_classes, aes(x = manufacturer, y = count_number, fill = class)) +
  geom_bar(stat = "identity", position = "stack") +  # Stacked bar plot
  labs(title = "Count of Cars by Manufacturer and Class",
       x = "Manufacturer",
       y = "Count",
       fill = "Class") +
  theme_minimal()


            
cat("\014")