## Multiple Regression - External Data

library(ggplot2)

# getwd() = gets current working directory
# setwd() = sets current working directory


# read the data
salary_data = read.csv("salaries.csv", header = TRUE, sep = " ")

dim(salary_data)
names(salary_data)
str(salary_data)

salary_data$sex = as.factor(salary_data$sex)
salary_data$discipline = as.factor(salary_data$discipline)
salary_data$rank = as.factor(salary_data$rank)

###

sex_v_rank_table = table(salary_data$sex, salary_data$rank)
addmargins(prop.table(sex_v_rank_table))

sex_v_rank_table2 = with(salary_data, table(sex, rank))
sex_v_rank_table2

# Chi- squared test
#H0 = rank & gender are independent
#Ha = rank & gender dependent

chisq.test(sex_v_rank_table2)
# Because p-value is <.05 - reject H0 & have evidence for Ha


ggplot(salary_data, aes(yrs.service, salary, color = sex)) +
  geom_point()

linmod_sal_v_service = lm(salary~yrs.service, data = salary_data)
summary(linmod_sal_v_service)

linmod_sal_v_service_and_sex = lm(salary~yrs.service+sex, data = salary_data)
summary(linmod_sal_v_service_and_sex)
# salary_hat = 92356.9 + 747.6*yrs.service + 9071.8(if male, 0 if female)

