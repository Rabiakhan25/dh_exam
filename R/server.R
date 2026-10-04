# Server logic.

app_server <- function(health_data, model) {
  balanced      <- balance_classes(health_data)
  risk_summary  <- summarise_risk_factors(balanced)
  status_shares <- prop.table(table(health_data$diabetic_status))

  function(input, output, session) {
    # Overview -----------------------------------------------------------------
    output$n_respondents   <- shiny::renderText(scales::comma(nrow(health_data)))
    output$pct_diabetes    <- shiny::renderText(scales::percent(status_shares[["Diabetes"]], 0.1))
    output$pct_prediabetes <- shiny::renderText(scales::percent(status_shares[["Prediabetes"]], 0.1))
    output$model_accuracy  <- shiny::renderText(scales::percent(model_accuracy(model), 0.1))

    output$risk_heatmap <- shiny::renderPlot(plot_risk_heatmap(risk_summary), res = 96)
    output$risk_bars    <- shiny::renderPlot(plot_risk_bars(risk_summary), res = 96)
    output$factor_split <- shiny::renderPlot({
      shiny::req(input$factor)
      plot_factor_split(risk_summary, input$factor)
    }, res = 96)

    # Demographics -------------------------------------------------------------
    output$pyramid_title <- shiny::renderText(
      paste("Age and sex distribution:", input$pyramid_status)
    )
    output$age_pyramid <- shiny::renderPlot({
      shiny::req(input$pyramid_status)
      plot_age_pyramid(age_sex_counts(health_data, input$pyramid_status))
    }, res = 96)

    # Risk predictor -----------------------------------------------------------
    prediction <- shiny::eventReactive(input$predict, {
      shiny::validate(
        shiny::need(is.numeric(input$bmi) && input$bmi >= 12 && input$bmi <= 98,
                    "Please enter a BMI between 12 and 98.")
      )
      profile <- list(
        bmi                     = input$bmi,
        phys_activity           = input$phys_activity,
        heart_disease_or_attack = input$heart_disease_or_attack,
        high_bp                 = input$high_bp,
        smoker                  = input$smoker,
        stroke                  = input$stroke,
        age                     = input$age,
        sex                     = input$sex
      )
      predict_status(model, profile)
    })

    output$prediction_result <- shiny::renderUI({
      if (input$predict == 0) {
        return(bslib::card(
          bslib::card_body(
            class = "text-center text-muted py-5",
            shiny::icon("clipboard-list", class = "fa-2x mb-3"),
            shiny::p("Fill in your profile and select ", shiny::strong("Estimate risk"), ".")
          )
        ))
      }
      result <- prediction()
      bslib::card(
        bslib::card_header("Estimated diabetes status"),
        bslib::card_body(
          shiny::h3(
            style = paste0("color:", STATUS_COLORS[[result$class]], ";"),
            result$class
          ),
          shiny::renderPlot(plot_prediction(result$probabilities), height = 220, res = 96)
        )
      )
    })
  }
}
