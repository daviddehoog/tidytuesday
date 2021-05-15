library(dplyr)
library(lubridate)
library(ggplot2)

# Which issues has the UN focused on over time?
temp <- roll_calls %>%
  inner_join(issues, by = "rcid") %>%
  mutate(year = year(date)) %>%
  group_by (year, issue) %>%
  summarise (total = n())

# total votes in each calendar year, faceted by six key issues
ggplot(temp, aes(x = year, y = total)) + 
        geom_bar(stat = "identity") +
        ylim(0, 42) +
        facet_wrap(~issue)

# total votes in each calendar year, broken down by each of the six key issues
ggplot(temp, aes(x = year, y = total, fill = issue)) + 
        geom_bar(position = "stack", stat = "identity")

# percentage of votes in each calendar year by each of the six key issues
ggplot(temp, aes(x = year, y = total, fill = issue)) + 
  geom_bar(position = "fill", stat = "identity")

# line graph of issues over time
ggplot(temp, aes(x = year, y = total, color = issue)) +
  geom_line()

# Permanent members' voting records
temp1 <- unvotes %>%
  inner_join(roll_calls, by = "rcid") %>%
  inner_join(issues, by = "rcid")

temp2 <- temp1 %>%
  mutate(year = year(date)) %>%
  filter ((country %in% c("United States", "France", "China", "Russia", "United Kingdom"))) %>%
  group_by (country, issue, year, vote) %>%
  summarise (total = n())

ggplot(temp2, aes(x = year, y = total, fill = issue)) + 
  geom_bar(position = "stack", stat = "identity") +
  facet_wrap (~country)

temp3 <- temp1 %>%
  filter(importantvote == 1) %>%
  group_by(country, vote) %>%
  summarise(total = n())