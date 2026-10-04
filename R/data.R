# Data loading and preparation.

#' Read the BRFSS diabetes indicators CSV and return a tidy data frame with
#' snake_case names and labelled factors for status, age group and sex.
load_health_data <- function(path = DATA_PATH) {
  if (!file.exists(path)) {
    stop("Dataset not found at '", path, "'. See README for setup.", call. = FALSE)
  }

  read.csv(path) |>
    janitor::clean_names() |>
    dplyr::rename(heart_disease_or_attack = heart_diseaseor_attack) |>
    dplyr::mutate(
      diabetic_status = factor(diabetes_012, levels = 0:2, labels = STATUS_LEVELS),
      age_group       = factor(age, levels = seq_along(AGE_LEVELS), labels = AGE_LEVELS),
      sex_label       = factor(sex, levels = 0:1, labels = SEX_LEVELS)
    )
}

#' Down-sample every diabetes class to the size of the smallest one so that
#' comparisons (and the classifier) are not dominated by the majority class.
balance_classes <- function(data, seed = SEED) {
  n_min <- min(table(data$diabetic_status))
  set.seed(seed)
  data |>
    dplyr::group_by(diabetic_status) |>
    dplyr::slice_sample(n = n_min) |>
    dplyr::ungroup()
}

#' Long-format summary of each binary risk factor per diabetes status:
#' number of respondents with the factor and the share within that status.
summarise_risk_factors <- function(data) {
  data |>
    dplyr::select(diabetic_status, dplyr::all_of(names(RISK_FACTORS))) |>
    tidyr::pivot_longer(-diabetic_status, names_to = "factor", values_to = "present") |>
    dplyr::group_by(diabetic_status, factor) |>
    dplyr::summarise(count = sum(present), share = mean(present), .groups = "drop") |>
    dplyr::mutate(factor_label = factor(RISK_FACTORS[factor], levels = rev(RISK_FACTORS)))
}

#' Respondent counts by age group and sex for one diabetes status.
age_sex_counts <- function(data, status) {
  data |>
    dplyr::filter(diabetic_status == status) |>
    dplyr::count(age_group, sex_label, .drop = FALSE)
}

