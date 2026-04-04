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
getting_outliers = function(seg_length){
  Q1 = quantile(seg_length, 0.25, na.rm = TRUE)
  Q3 = quantile(seg_length, 0.75, na.rm = TRUE)
  IQR = Q3 - Q1
  lower_bound = Q1 - 1.5 * IQR
  upper_bound = Q3 + 1.5 * IQR
  outliers = seg_length[seg_length > upper_bound | seg_length < lower_bound]
  return(list(outliers = outliers, upper_bound = upper_bound))
  
}

seg_no_risk = seg$Seg_length[seg$Suicide.risk.alert == "No"]
seg_yes_risk = seg$Seg_length[seg$Suicide.risk.alert == "Yes"]

outlier_no = calculate_outliers(seg_no_risk)
outlier_yes = calculate_outliers(seg_yes_risk)


outlier_table = data.frame(
  Suicide_Risk = c("No", "Yes"),
  Total_N = c(length(seg_no_risk), length(seg_yes_risk)),
  Outlier_Count = c(length(outlier_no$outliers), length(outlier_yes$outliers)),
  Outlier_Percent = c(round(length(outlier_no$outliers) / length(seg_no_risk) * 100, 1),
                      round(length(outlier_yes$outliers) / length(seg_yes_risk) * 100, 1)),
  Outlier_Range = c(paste(round(min(outlier_no$outliers), 1), "-", round(max(outlier_no$outliers), 1)),
                    paste(round(min(outlier_yes$outliers), 1), "-", round(max(outlier_yes$outliers), 1)))
)







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
prop.table(table(seg$Mental.health.alert, seg$Suicide.risk.alert), margin = 1)


{r}

seg_regression = seg %>% 
  mutate(Suicide_binary = ifelse(Suicide.risk.alert == "Yes",1,0),
         Mental_binary = ifelse(Mental.health.alert == "Yes",1,0),
         Gender_binary = ifelse(Gender == "Male",1,0))
m = glm(Suicide_binary ~ Gender_binary+Mental_binary + Seg_length + Reason, family = binomial, data = seg_regression)
summary(m)
library(pROC)
p = predict(m, type = "response")
roc_logit = roc(seg_regression$Suicide_binary ~ p)
ggroc(roc_logit, color="red", size=2)
auc(roc_logit)

{r}
library(rpart)
m2 = rpart(Suicide_binary ~ Gender_binary + Mental_binary + Seg_length +Reason,
           data = seg_regression, method="class")
library(rattle)
fancyRpartPlot(m2)



