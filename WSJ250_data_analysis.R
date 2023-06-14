install.packages("tidyverse")
install.packages("ggplot2")
install.packages("forcats")
library(tidyverse)
library(ggplot2)
library(forcats)

setwd("/Users/sijanikbal/Documents/JUST Capital/Data Project 1")
df <- read_csv("wsj_250_best-managed_companies.csv")

head(df)
names(df)

top_ten <- df %>% 
  subset(rank <=10)

df %>% 
  group_by(sector) %>% count()

# Does the overall_score differ across various industries?
table(df$sector)

industry <- df %>% 
  group_by(sector) %>% 
  summarise(industry_average_overall_score = mean(overall_score)) %>% 
  arrange(desc(industry_average_overall_score))

industry %>% 
  mutate(sector = fct_reorder(sector, industry_average_overall_score)) %>% 
  ggplot(aes(industry_average_overall_score, sector)) +
  geom_point()

# Is there an industry-wide trend for various ratings?
# customer_satisfaction
cus_sat <- df %>% 
  group_by(sector) %>% 
  summarise(customer_satisfaction_mean = mean(customer_satisfaction)) %>% 
  arrange(desc(customer_satisfaction_mean))
head(cus_sat, 5)

cus_sat %>% 
  ggplot(aes(fct_rev(fct_reorder(sector, customer_satisfaction_mean)), customer_satisfaction_mean)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90))

# employee_engagement_and_development

# innovation

# social_respnsibility

# financial strength

# What is the industry average and standard deviation for various rating-scales?
industry_average <- df %>% 
  group_by(sector) %>% 
  summarise(employee_engagement_and_development_average = mean(employee_engagement_and_development), 
            employee_engagement_and_development_sd = sd(employee_engagement_and_development),
            innovation_mean = mean(innovation), innovation_sd = sd(innovation),
            customer_satisfaction_mean = mean(customer_satisfaction),
            customer_satisfaction_sd = sd(customer_satisfaction),
            social_responsibility_mean = mean(social_responsibility),
            social_responsibility_sd = sd(social_responsibility),
            financial_strength_mean = mean(financial_strength),
            financial_strength_sd = sd(financial_strength),
            overall_score_mean = mean(overall_score),
            overall_score_sd = sd(overall_score))

#djg adg