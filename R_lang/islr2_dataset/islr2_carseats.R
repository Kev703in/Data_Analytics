library(tidyverse)
library(ISLR2)
theme_set(theme_minimal())

?geom_bar

glimpse(Carseats)
?Carseats
view(Carseats)

###############

### Infering Price difference between own and competion
xy_min <- min(Carseats$CompPrice, Carseats$Price, na.rm=TRUE)

ggplot(Carseats,aes(x=Price, y=CompPrice))+
  geom_point()+
  geom_smooth(method="lm")+
  scale_x_continuous(limits = c(xy_min, NA))+
  scale_y_continuous(limits = c(xy_min, NA))

### Shevling Location to sales in USA and other places

ggplot(Carseats,aes(x=ShelveLoc, y=Sales))+
  geom_violin(aes(fill=ShelveLoc))+
  geom_boxplot(aes(fill=US))+
  scale_x_discrete(limits = c("Bad", "Medium", "Good"))+
  scale_color_brewer(palette = "Dark2")+
  theme_minimal()

### Checking correlation between Advertising and sales
ggplot(Carseats,aes(x=Advertising, y=Sales))+
  geom_point()+
  geom_smooth(method="lm")

# No Advertisement
sum(Carseats$Advertising==0)
# yes Advertisement
sum(Carseats$Advertising>0)

## Can not infer meaningful data from advertisement data


###############
# Basic bar chart based on shelveing Locations:
ggplot(Carseats, aes(x = ShelveLoc,
                     fill = ShelveLoc)) + 
  geom_bar(width = .60,
           show.legend = FALSE) +
  scale_fill_brewer(palette = "Dark2") # added for accessibility

ggplot(Carseats, aes(x = US,
                     fill = ShelveLoc)) + 
  geom_bar() +
  scale_fill_brewer(palette = "Dark2")

ggplot(Carseats, aes(x = US,
                     fill = ShelveLoc)) + 
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "Dark2")

ggplot(Carseats, aes(x = US,
                     fill = ShelveLoc)) + 
  geom_bar(position = "dodge") +
  scale_fill_brewer(palette = "Dark2")

ggplot(Carseats, aes(fill = US,
                     x = ShelveLoc)) + 
  geom_bar(position = "dodge") +
  scale_fill_brewer(palette = "Dark2")


#### Income vs Education

ggplot(Carseats,aes(x=Education, y=Income))+
  geom_point()+
  theme_minimal()
