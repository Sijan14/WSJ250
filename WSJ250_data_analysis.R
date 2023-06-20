install.packages("tidyverse")
install.packages("ggplot2")
install.packages("forcats")
install.packages("data.table")
library(tidyverse)
library(ggplot2)
library(forcats)
library(stringr)
library(data.table)

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

# What is the industry average and standard deviation for various rating-scales?
industry_average <- df %>% 
  group_by(sector) %>% 
  summarise(employee_engagement_and_development_mean = mean(employee_engagement_and_development), 
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

# Is there an industry-wide trend for various ratings?
# customer_satisfaction
cus_sat <- df %>% 
  group_by(sector) %>% 
  summarise(customer_satisfaction_mean = mean(customer_satisfaction)) %>% 
  arrange(desc(customer_satisfaction_mean))
head(cus_sat, 5)

cus_sat %>% 
  mutate(sector = fct_rev(fct_reorder(sector, customer_satisfaction_mean))) %>% 
  ggplot(aes(sector, customer_satisfaction_mean)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90))

# employee_engagement_and_development
industry_average %>% 
  mutate(sector = reorder(sector, employee_engagement_and_development_mean, decreasing = TRUE)) %>% 
  ggplot(aes(sector, employee_engagement_and_development_mean)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90))

# innovation
industry_average %>% 
  mutate(sector = reorder(sector, innovation_mean, decreasing = TRUE)) %>% 
  ggplot(aes(sector, innovation_mean)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90))

# social_respnsibility
industry_average %>% 
  mutate(sector = reorder(sector, social_responsibility_mean, decreasing = TRUE)) %>% 
  ggplot(aes(sector, social_responsibility_mean)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90))

# financial strength
industry_average %>% 
  mutate(sector = reorder(sector, financial_strength_mean, decreasing = TRUE)) %>% 
  ggplot(aes(sector, financial_strength_mean)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90))

# Which companies had the highest growth or decline in ratings from previous year?
# Highest growth
growth <- df[df$change_from_2019 %like% "up", ] # 142 companies improved their ratings from last year

growth$change_from_2019 <- 
  str_replace_all(growth$change_from_2019, "up ", "") %>% 
  as.numeric()

growth <- growth %>% 
  arrange(desc(change_from_2019))

growth <- rename(growth, growth_from_2019 = change_from_2019)
head(growth, 10)

growth %>%
  top_n(10, growth_from_2019) %>%
  arrange(desc(growth_from_2019)) %>%
  ggplot(aes(reorder(company, growth_from_2019, decreasing = TRUE), growth_from_2019)) +
  geom_col() +
  xlab("Company") +
  ylab("Growth from 2019") +
  ggtitle("Top 10 Companies by Growth from 2019") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Highest decline
decline <- df[df$change_from_2019 %like% "down", ] # 104 companies had a decline in their overall rating

decline$change_from_2019 <- 
  str_replace_all(decline$change_from_2019, "down ", "") %>% 
  as.numeric()

decline <- decline %>% 
  arrange(desc(change_from_2019))

decline <- rename(decline, decline_from_2019 = change_from_2019)
head(decline, 10)

decline %>%
  top_n(10, decline_from_2019) %>%
  arrange(desc(decline_from_2019)) %>%
  ggplot(aes(reorder(company, decline_from_2019, decreasing = TRUE), decline_from_2019)) +
  geom_col() +
  xlab("Company") +
  ylab("Decline from 2019") +
  ggtitle("Top 10 Companies by Decline from 2019") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Is there an industry-wide trend in growth or decline?
# growth
growth_sector <- growth %>% 
  group_by(sector) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  rename(no_of_companies = n)

growth_sector %>% 
  head(10) %>% 
  ggplot() +
  geom_col(aes(reorder(sector, no_of_companies, decreasing = TRUE), no_of_companies)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# financial services
finance <- df %>% # 21 grew and 9 declined
  filter(sector == "Financial Services")

finance %>% # 12 finance companies ranked in Top 100
  filter(rank <= 100) %>% 
  count()

finance_iv <- industry_average %>% 
  filter(sector == "Financial Services") %>% 
  gather(key = rating_scale, value = avg_rating, employee_engagement_and_development_mean:overall_score_sd) %>% 
  select(!sector) %>% 
  filter(grepl("mean$", rating_scale)) %>% 
  filter(rating_scale != "overall_score_mean")

finance_iv %>% ggplot() +
  geom_col(aes(rating_scale, avg_rating)) +
  ggtitle("Finance Industry") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# decline
declining_sector <- decline %>% 
  group_by(sector) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  rename(no_of_companies = n)

declining_sector %>% 
  head(10) %>% 
  ggplot() +
  geom_col(aes(reorder(sector, no_of_companies, decreasing = TRUE), no_of_companies)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Technology

