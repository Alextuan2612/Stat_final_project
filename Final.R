library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)



seg = read.csv("Final-project/solgen-individuals-in-seg-2018.csv")

seg = seg %>% rename(Seg_length = Segregation.placement.length..in.days.) %>% 
  rename(Reason = Primary.reason.for.segregation)

#1st Graph

seg_reason = seg %>% group_by(Reason) %>% summarise(n = n())

seg_reason = seg_reason%>%arrange(n)

seg_reason = seg_reason %>% mutate(Percentage = round(n/sum(seg_reason$n)*100,1))

seg_reason = seg_reason %>%
  mutate(Reason = case_when(
    Reason == "inmate request" ~ "Inmate Request",
    Reason == "inmate needs protection" ~ "Protect Inmate",
    Reason == "inmate needs protection: medical" ~ "Protect Inmate: Medical",
    Reason == "security of institution/safety of others" ~ "Security/Safety",
    Reason == "security of institution/safety of others: medical" ~ "Security/Safety: Medical",
    Reason == "alleged misconduct" ~ "Alleged Misconduct",
    Reason == "close confinement" ~ "Close Confinement",
    Reason == "multiple reasons" ~ "Multiple Reasons",
    TRUE ~ "Other"
  ))



ggplot(seg_reason, aes(x = reorder(Reason, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") + coord_flip() +
  labs(title = "Figure 1: Primary Reasons for Segregation Placement",
       subtitle = "Ontario Correctional Facilities, 2018", x = "Reason for Segregation", y = "Number of Individuals") +
  theme_minimal() +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  geom_text(aes(label = paste0(Percentage, "%")), 
            hjust = -0.2,  
            size = 3.5)


# 2nd graph

hist_data = ggplot_build(
  ggplot(seg, aes(x = Seg_length)) + 
    geom_histogram(bins = 30, fill = "steelblue", color = "white")
)$data[[1]]

ggplot(seg, aes(x = Seg_length)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white", alpha = 0.7) +
  labs(title = "Distribution of Segregation Placement Length",
       x = "Days in Segregation", 
       y = "Number of Individuals") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.title.position = "plot") +   geom_text(data = hist_data, aes(x = (xmin + xmax)/2, y = count, label = count),
                                                    vjust = -0.5, 
                                                    size = 2.5)

