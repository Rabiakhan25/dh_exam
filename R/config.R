# Project-wide constants: file paths, category labels, model settings and
# colour palettes. Everything that might need tweaking lives here.

DATA_PATH  <- file.path("data", "diabetes_health_indicators.csv")
MODEL_PATH <- file.path("models", "rf_model.rds")

SEED <- 2024

# Diabetes_012 coding used by the BRFSS dataset.
STATUS_LEVELS <- c("No diabetes", "Prediabetes", "Diabetes")

# BRFSS 5-year age buckets (Age = 1..13).
AGE_LEVELS <- c(
  "18-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54",
  "55-59", "60-64", "65-69", "70-74", "75-79", "80+"
)

SEX_LEVELS <- c("Female", "Male")

# Binary (0/1) indicators shown in the risk-factor views, with display labels.
RISK_FACTORS <- c(
  high_bp                 = "High blood pressure",
  high_chol               = "High cholesterol",
  chol_check              = "Cholesterol check (5 yrs)",
  smoker                  = "Smoker",
  stroke                  = "History of stroke",
  heart_disease_or_attack = "Heart disease / attack",
  phys_activity           = "Physically active",
  fruits                  = "Eats fruit daily",
  veggies                 = "Eats vegetables daily",
  hvy_alcohol_consump     = "Heavy alcohol use"
)

# Predictors used by the random forest classifier.
MODEL_FEATURES <- c(
  "bmi", "phys_activity", "heart_disease_or_attack", "high_bp",
  "smoker", "stroke", "age", "sex"
)

MODEL_PARAMS <- list(ntree = 500, mtry = 3)

# Brand colours.
COLOR_PRIMARY <- "#0F766E"
STATUS_COLORS <- setNames(c("#5B8DB8", "#E8A33D", "#C4473A"), STATUS_LEVELS)
SEX_COLORS    <- setNames(c("#B5679E", "#3E7CB1"), SEX_LEVELS)
