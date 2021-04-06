# Data on tags over time

install.packages("dplyr")
install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
by_tag_year <- read_csv("C:/Users/Anjan/Downloads/iris/New folder/by_tag_year.csv")
head(by_tag_year)
by_tag_year_fraction <- by_tag_year %>% 
  select(year, tag, number,year_total) %>%
  mutate(fraction = (number / year_total))

head(by_tag_year_fraction)

#Has R been growing or shrinking?
r_over_time <- filter(by_tag_year_fraction, tag == "r") 
ggplot(data = r_over_time, mapping = aes(x= year,y = fraction))+geom_line()

#How about r,dplyr and ggplot2?
selected_tags <- c("r","dplyr","ggplot2")
selected_tags_over_time <- filter(by_tag_year_fraction, tag %in% selected_tags)
ggplot(data = selected_tags_over_time, mapping = aes(x= year,y = fraction,color = as.factor(tag)))+geom_line()

#What are the most asked-about tags?
sorted_tags <- by_tag_year %>% 
  group_by(tag) %>%
  summarize(tag_total = sum(number)) %>%
  arrange(desc(tag_total))

#How have large programming languages changed over time? - Top  10
top_10_tags <- sorted_tags %>%
  top_n(10)

top_10_subset <- by_tag_year_fraction %>%
  filter(tag %in% top_10_tags$tag)

head(top_10_subset)
ggplot(data = top_10_subset, mapping = aes(x= year,y = fraction,color = as.factor(tag)))+geom_line()


#let's check out how three big mobile operating systems (Android, iOS, and Windows Phone)
os_tags <- c("android","ios,", "windows-mobile","windows-phone","windows-phone-7","windows-phone-7.1","windows-phone-8","windows-phone-8.1")

os_subset <- filter(by_tag_year_fraction, tag %in% os_tags)
ggplot(data = os_subset, mapping = aes(x= year,y = fraction,color = as.factor(tag)))+geom_line()
