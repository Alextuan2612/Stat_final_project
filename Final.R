library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)



seg = read.csv("solgen-individuals-in-seg-2018.csv")

seg = seg %>% rename(Seg_length = Segregation.placement.length..in.days.) %>% 
  rename(Reason = Primary.reason.for.segregation)
# Graph 1
mental_suicide = seg %>% group_by(Mental.health.alert, Suicide.risk.alert) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Mental.health.alert) %>% mutate(percentage = count/sum(count) * 100)

ggplot(mental_suicide, aes(x = factor(Mental.health.alert, labels = c("No MH Alert", "MH Alert")), 
                           y = percentage, 
                           fill = factor(Suicide.risk.alert, labels = c("No Suicide Risk", "Suicide Risk")))) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Proportion of Suicide Risk Alerts by Mental Health Status",
       x = "Mental Health Alert", 
       y = "Proportion",
       fill = "Suicide Risk") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Graph 2
ggplot(seg, aes(x = factor(Suicide.risk.alert, labels = c("No", "Yes")), 
                          y = Seg_length)) +
  geom_boxplot(fill = c("lightblue", "salmon"), outlier.shape = NA) +
  coord_cartesian(ylim = c(0, quantile(seg$Seg_length, 0.95))) +
  labs(title = "Segregation Placement Length by Suicide Risk Status",
       x = "Suicide Risk Alert", 
       y = "Days in Segregation") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Graph 3
gender_suicide = seg %>% group_by(Gender, Suicide.risk.alert) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Gender) %>% mutate(percentage = count / sum(count) * 100)

ggplot(gender_suicide, aes(x = factor(Gender, labels = c("Female", "Male")), 
                           y = percentage, 
                           fill = factor(Suicide.risk.alert, labels = c("No", "Yes")))) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Proportion of Suicide Risk Alerts by Gender",
       x = "Gender", 
       y = "Proportion",
       fill = "Suicide Risk") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Graph 4

race_suicide = seg %>% group_by(Race.group, Suicide.risk.alert) %>%
  summarise(count = n(), .groups = "drop") %>% group_by(Race.group) %>%
  mutate(percentage = count / sum(count) * 100)

ggplot(race_suicide, aes(x = Race.group, 
                         y = percentage, 
                         fill = factor(Suicide.risk.alert, labels = c("No", "Yes")))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Distribution of Suicide Risk Alerts by Race",
       x = "Race Group", 
       y = "Count",
       fill = "Suicide Risk") +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


