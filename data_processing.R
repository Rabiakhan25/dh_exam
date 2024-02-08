pacman::p_load(
  shiny, 
  shinythemes,
  data.table,
  randomForest,
  ggtext,
  ggplot2,
  janitor,
  tidyverse,
  dplyr,
  ggtext
)

raw_data <- read.csv('diabetes_health_indicators.csv') %>%
  clean_names() %>%
  mutate(diabetic_status=case_when(diabetes_012 == 0 ~ "no_diabetes",
                                   diabetes_012 == 1 ~ "pre_diabetes",
                                    diabetes_012 == 2 ~ "diabetes"))

age_gender_data <- raw_data %>%
  select(age, sex, diabetic_status) %>%
  mutate(age_cat=case_when(age == 1 ~ "18-24",
                           age == 2 ~ "25-29",
                           age == 3 ~ "30-34",
                           age == 4 ~ "35-39",
                           age == 5 ~ "40-44",
                           age == 6 ~ "45-49",
                           age == 7 ~ "50-54",
                           age == 8 ~ "55-59",
                           age == 9 ~ "60-64",
                           age == 10 ~ "65-69",
                           age == 11 ~ "70-74",
                           age == 12 ~ "75-79",
                           age == 13 ~ "80+"),
         age_cat=factor(age_cat), 
         sex=case_when(sex == 0 ~ "female",
                        sex == 1 ~ "male"))

data <- raw_data %>%
  select(diabetes_012, high_bp, high_chol, chol_check, smoker, stroke, 
         heart_diseaseor_attack, phys_activity, fruits, veggies, 
         hvy_alcohol_consump, diabetic_status)

colnames(data)[which(names(data) == "heart_diseaseor_attack")] <- "heart_disease_or_attack"

# Aggregate the data
min_samples <- min(table(data$diabetes_012))

equal_data <- data %>%
  group_by(diabetes_012) %>%
  sample_n(min_samples)


agg_data <- equal_data %>%
  group_by(diabetic_status) %>%
  summarise(high_bp = sum(high_bp), 
            high_chol = sum(high_chol),
            chol_check = sum(chol_check),
            smoker = sum(smoker),
            stroke = sum(stroke), 
            heart_disease_or_attack = sum(heart_disease_or_attack),
            phys_activity = sum(phys_activity),
            fruits = sum(fruits),
            veggies = sum(veggies), 
            hvy_alcohol_consump = sum(hvy_alcohol_consump))

# Reshape the data for ggplot
plot_data <- agg_data %>%
  gather(key = "factor", value = "value", -diabetic_status)

clustering_equal_data <- raw_data %>%
  select(diabetes_012, bmi, phys_activity, heart_diseaseor_attack, high_bp, smoker, stroke, age, sex, diabetic_status) %>%
  group_by(diabetes_012) %>%
  sample_n(min_samples)


clustering_equal_data <- subset(clustering_equal_data, select = -c(diabetes_012))
clustering_equal_data$diabetic_status <- as.factor(clustering_equal_data$diabetic_status)

model <- randomForest(diabetic_status ~ ., data = clustering_equal_data, ntree = 500, mtry = 8, importance = TRUE)
