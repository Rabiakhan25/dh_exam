# User interface.

app_theme <- function() {
  bslib::bs_theme(
    version   = 5,
    primary   = COLOR_PRIMARY,
    base_font = bslib::font_google("Inter", local = FALSE),
    "navbar-bg" = COLOR_PRIMARY
  )
}

overview_panel <- function() {
  bslib::nav_panel(
    "Overview",
    icon = shiny::icon("chart-pie"),
    bslib::layout_column_wrap(
      width = 1 / 4, fill = FALSE,
      bslib::value_box("Survey respondents", shiny::textOutput("n_respondents"),
                       showcase = shiny::icon("users"), theme = "primary"),
      bslib::value_box("Diabetes", shiny::textOutput("pct_diabetes"),
                       showcase = shiny::icon("droplet"), theme = "danger"),
      bslib::value_box("Prediabetes", shiny::textOutput("pct_prediabetes"),
                       showcase = shiny::icon("triangle-exclamation"), theme = "warning"),
      bslib::value_box("Model accuracy (OOB)", shiny::textOutput("model_accuracy"),
                       showcase = shiny::icon("bullseye"), theme = "secondary")
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::navset_card_pill(
        title = "Risk factor prevalence by diabetes status",
        full_screen = TRUE,
        bslib::nav_panel("Heat map", shiny::plotOutput("risk_heatmap", height = "460px")),
        bslib::nav_panel("Bar chart", shiny::plotOutput("risk_bars", height = "460px")),
        footer = shiny::p(
          class = "text-muted small mb-0",
          "Computed on a class-balanced sample so that each status group has the same size."
        )
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Who has this risk factor?"),
        shiny::selectInput(
          "factor", NULL,
          choices = setNames(names(RISK_FACTORS), RISK_FACTORS),
          selected = "high_bp", width = "100%"
        ),
        shiny::plotOutput("factor_split", height = "380px")
      )
    )
  )
}

demographics_panel <- function() {
  bslib::nav_panel(
    "Demographics",
    icon = shiny::icon("people-group"),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        shiny::radioButtons(
          "pyramid_status", "Diabetes status",
          choices = STATUS_LEVELS, selected = "Diabetes"
        ),
        shiny::p(class = "text-muted small",
                 "Age and sex distribution of respondents in the selected group.")
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(shiny::textOutput("pyramid_title", inline = TRUE)),
        shiny::plotOutput("age_pyramid", height = "520px")
      )
    )
  )
}

predictor_panel <- function() {
  bslib::nav_panel(
    "Risk Predictor",
    icon = shiny::icon("stethoscope"),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 320,
        title = "Your profile",
        shiny::radioButtons("sex", "Sex", choices = setNames(0:1, SEX_LEVELS),
                            selected = 0, inline = TRUE),
        shiny::selectInput("age", "Age group", choices = setNames(seq_along(AGE_LEVELS), AGE_LEVELS),
                           selected = 5),
        shiny::numericInput("bmi", "Body mass index (BMI)", value = 25, min = 12, max = 98, step = 0.5),
        shiny::checkboxInput("phys_activity", "Physically active in the past 30 days", FALSE),
        shiny::checkboxInput("high_bp", "Diagnosed with high blood pressure", FALSE),
        shiny::checkboxInput("heart_disease_or_attack", "Coronary heart disease or heart attack", FALSE),
        shiny::checkboxInput("stroke", "Ever had a stroke", FALSE),
        shiny::checkboxInput("smoker", "Smoked at least 100 cigarettes in lifetime", FALSE),
        shiny::actionButton("predict", "Estimate risk", class = "btn-primary w-100",
                            icon = shiny::icon("calculator"))
      ),
      shiny::uiOutput("prediction_result"),
      bslib::card(
        class = "border-warning",
        bslib::card_body(
          class = "small text-muted",
          shiny::strong("Disclaimer: "),
          "This tool is an educational demonstration built on self-reported survey data. ",
          "It is not a medical device and must not be used for diagnosis. ",
          "Please consult a healthcare professional about your health."
        )
      )
    )
  )
}

about_panel <- function() {
  bslib::nav_panel(
    "About",
    icon = shiny::icon("circle-info"),
    bslib::card(
      bslib::card_body(
        shiny::h4("About this project"),
        shiny::p(
          "Diabetes Risk Monitor explores the CDC Behavioral Risk Factor Surveillance ",
          "System (BRFSS) 2015 diabetes health indicators dataset and provides an ",
          "interactive random-forest estimate of diabetes status."
        ),
        shiny::h5("Data"),
        shiny::tags$ul(
          shiny::tags$li("253,680 survey responses, 21 health indicators."),
          shiny::tags$li("Target: no diabetes, prediabetes, or diabetes."),
          shiny::tags$li("Classes are down-sampled to equal size before analysis and modelling.")
        ),
        shiny::h5("Model"),
        shiny::p(
          "A random forest trained on BMI, age, sex, physical activity, blood pressure, ",
          "heart disease, stroke and smoking history. Accuracy is reported from out-of-bag ",
          "samples on the balanced three-class problem."
        ),
        shiny::p(
          "A full write-up is available in ",
          shiny::tags$code("docs/report.pdf"), " in the project repository."
        )
      )
    )
  )
}

app_ui <- function() {
  bslib::page_navbar(
    title = shiny::span(shiny::icon("heart-pulse"), "Diabetes Risk Monitor"),
    window_title = "Diabetes Risk Monitor",
    theme = app_theme(),
    fillable = FALSE,
    overview_panel(),
    demographics_panel(),
    predictor_panel(),
    about_panel()
  )
}
