library(tidyverse)
library(tidytuesdayR)
tuesdata <- tidytuesdayR::tt_load(2021, week = 41)
nurses <- tuesdata$nurses
theme_set(theme_minimal())

?geom_bar

View(nurses)
glimpse(nurses)

unique(nurses$Year)

#### Nurses agregate from 1998 to 2020

nurses_agregate_national <- nurses %>% 
  group_by(Year) %>% 
  summarize(Total_Employed_Healthcare_national_Aggregate=mean(`Total Employed (Healthcare, National)_Aggregate`, na.rm=TRUE))

view(nurses_agregate_national)

ggplot(nurses_agregate_national, aes(x=Year, y=Total_Employed_Healthcare_national_Aggregate))+
  geom_point()+
  geom_line(color="blue")+
  theme_minimal()

ggplot(nurses, aes(x=Year, y=`Total Employed (Healthcare, State)_Aggregate`, color=State))+
  geom_point(alpha=0.3)+
  geom_smooth( se=FALSE )+
  theme_minimal()+
  theme(legend.position = "bottom")+
  guides(color = guide_legend(nrow = 2, byrow = TRUE))

ggplot(nurses, aes(x=Year, y=`Hourly Wage Avg`, color=State))+
  geom_point(alpha=0.3)+
  geom_smooth( se=FALSE )+
  theme_minimal()+
  theme(legend.position = "bottom")+
  guides(color = guide_legend(nrow = 2, byrow = TRUE))


########State

#### Nurses across states in 2020

nurses_2020 <- nurses %>% 
  filter(Year == 2020)
View(nurses_2020)

ggplot(nurses_2020, aes(y = State, x = `Total Employed RN`)) +
  geom_col()

nurses_2020 <- nurses %>% 
  filter(Year == 2020) %>% 
  slice_max(order_by = `Total Employed RN`, n = 20)

ggplot(nurses_2020, aes(y = fct_reorder(State, `Total Employed RN`),
                        x = `Total Employed RN`)) +
  geom_col() +
  labs(y = "State")+
  theme_minimal()

####


