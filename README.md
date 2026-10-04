# Diabetes Risk Monitor

An interactive R Shiny dashboard for exploring diabetes risk factors in the
CDC **Behavioral Risk Factor Surveillance System (BRFSS) 2015** survey, plus a
random-forest model that estimates a respondent's diabetes status from a small
set of health indicators.

**Live app:** <https://rabiakhan.shinyapps.io/exam/>

## Features

- **Overview**: headline figures (respondents, diabetes and prediabetes
  prevalence, model accuracy), a heat map and bar chart of risk-factor
  prevalence by diabetes status, and a breakdown of how people with a chosen
  risk factor are spread across the three status groups.
- **Demographics**: age/sex population pyramids for each diabetes status.
- **Risk Predictor**: enter BMI, age group, sex and medical history to get
  predicted probabilities for *no diabetes*, *prediabetes* and *diabetes*.
- **About**: notes on the data and the modelling approach.

## Dataset

`data/diabetes_health_indicators.csv` holds 253,680 BRFSS 2015 survey
responses with 21 health indicators. The target variable `Diabetes_012` is
coded `0` = no diabetes, `1` = prediabetes, `2` = diabetes.

Prediabetes makes up under 2% of responses, so the risk-factor comparisons and
the classifier both use a **class-balanced sample**: each group is
down-sampled to the size of the smallest one.

## Model

| Setting  | Value |
|----------|-------|
| Algorithm | Random forest (`randomForest`) |
| Features | BMI, age group, sex, physical activity, high blood pressure, heart disease/attack, stroke, smoking |
| Trees / `mtry` | 500 / 3 |
| Training data | Class-balanced sample (~13.9k rows) |
| OOB accuracy | ~49% on three balanced classes (33% would be chance) |

Prediabetes is the hardest class to tell apart, since its profile sits between
the other two. On first launch the app trains the model and caches it to
`models/rf_model.rds`. After that it loads from the cache.

## Project structure

```
.
├── app.R                    # Entry point: loads data and model, starts the app
├── R/                       # Sourced automatically by Shiny
│   ├── config.R             # Paths, labels, model settings, colours
│   ├── data.R               # Loading, cleaning, balancing, summaries
│   ├── model.R              # Training, caching and prediction
│   ├── plots.R              # ggplot2 chart builders
│   ├── ui.R                 # bslib user interface
│   └── server.R             # Server logic
├── scripts/
│   ├── install_packages.R   # Installs required packages
│   └── train_model.R        # Rebuilds the cached model
├── data/                    # BRFSS 2015 dataset
├── docs/report.pdf          # Project report
└── models/                  # Cached model (generated, git-ignored)
```

## Getting started

Requirements: **R ≥ 4.1**.

```bash
git clone https://github.com/Rabiakhan25/dh_exam.git
cd dh_exam
Rscript scripts/install_packages.R
Rscript -e "shiny::runApp()"
```

You can also open `dh_exam.Rproj` in RStudio and click **Run App**.

To retrain the model after changing `MODEL_FEATURES` or `MODEL_PARAMS` in
`R/config.R`:

```bash
Rscript scripts/train_model.R
```

## Disclaimer

This project is for education only. The data is self-reported survey data, and
the predictor is **not** a medical or diagnostic tool. Please see a healthcare
professional about your health.
