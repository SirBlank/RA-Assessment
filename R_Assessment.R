library(tidyverse)
library(car)

# reading data
test_data <- read.csv("test_data.csv")
test_data <- na.omit(test_data)

# reverse coding
reverse_cols <- c("team.cohesion_3_r", "task_interdepdence_2_r")
test_data[, reverse_cols] <- 6 - test_data[, reverse_cols]

# renaming column names
names(test_data) <- gsub(x = names(test_data), pattern = "\\.", replacement = "_")
names(test_data) <- gsub(x = names(test_data), pattern = "interdepdence", replacement = "interdependence")
names(test_data) <- gsub(x = names(test_data), pattern = "\\_r", replacement = "")

# composite scale for cohesion and interdependence
test_data <- test_data %>%
  rowwise() %>%
  mutate(cohesion_composite = mean(c_across(contains("cohesion"))),
         interdependence_composite = mean(c_across(contains("interdependence"))))

# descriptive table
cohesion_mean <- mean(test_data$cohesion_composite)
interdependence_mean <- mean(test_data$interdependence_composite)
performance_mean <- mean(test_data$team_performance)

cohesion_sd <- sd(test_data$cohesion_composite)
interdependence_sd <- sd(test_data$interdependence_composite)
performance_sd <- sd(test_data$team_performance)

cohesion_interdependence_cor <- round(cor(test_data$cohesion_composite, test_data$interdependence_composite), digits = 3)
cohesion_performance_cor <- round(cor(test_data$cohesion_composite, test_data$team_performance), digits = 3)
interdependence_performance_cor <- round(cor(test_data$interdependence_composite, test_data$team_performance), digits = 3)

Variable <- c("Team Cohesion", "Task Interdependence", "Team Performance")
Team_Cohesion <- c("", cohesion_interdependence_cor, cohesion_performance_cor)
Task_Interdependence <- c(cohesion_interdependence_cor, "", interdependence_performance_cor)
Team_Performance <- c(cohesion_interdependence_cor, interdependence_performance_cor, "")
Mean <- c(cohesion_mean, interdependence_mean, performance_mean)
Standard_Deviation <- c(cohesion_sd, interdependence_sd, performance_sd)

table <- data.frame(Variable, Team_Cohesion, Task_Interdependence, Team_Performance, Mean, Standard_Deviation)
table <- table %>%
  mutate(across(is.numeric, round, digits = 3))

# visualizing linear regression between team performance (response variable) and 
# team cohesion and task interdependence (explanatory variables)
lm_performance <- lm(team_performance ~ (interdependence_composite + cohesion_composite)^2, data = test_data)
plot(test_data$cohesion_composite, test_data$team_performance)
abline(lm_performance)

avPlots(lm_performance)

summary(lm_performance)

# visualizing linear regression between the response variable and explanatory variables with only task interdependence higher than 3.
high_interdependence <- test_data %>%
  filter(interdependence_composite >= 3)

lm_high_inter <- lm(team_performance ~ (interdependence_composite + cohesion_composite)^2, data = high_interdependence)
plot(high_interdependence$cohesion_composite, high_interdependence$team_performance)
abline(lm_high_inter)

avPlots(lm_high_inter)

summary(lm_high_inter)