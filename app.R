pacman::p_load(
  shiny, 
  shinythemes,
  data.table,
  randomForest,
  ggtext,
  ggplot2,
  bslib,
  ggpubr,
  RColorBrewer,
  tidyr
)

source("data_processing.R")

ui <- fluidPage(theme = shinytheme('cerulean'),
                navbarPage(
                  "Diabetes Risk Monitor",
                  tabPanel("Data Analysis",
                           fluidRow(
                             column(width = 6, 
                                    
                                    h3("Risk Factors Analysis", 
                                        style='background-color:#008080;
                                              padding-left: 15px;
                                              color: white'),
                                    
                                    mainPanel(
                                      navset_card_pill(
                                        nav_panel("Heat Plot", 
                                                  plotOutput("riskFactorHeatPlot"),
                                                  p("Association of diabetes status
                                                    with risk factors.")),
                                        
                                        nav_panel("Bar Plot", 
                                                  plotOutput("riskFactor"),
                                                  p("Distribution of risk 
                                                    factors among population.")),
                                        
                                        nav_panel("Gender/Age", 
                                                  radioButtons("plotType", 
                                                               "Select Plot Type:",
                                                               choices = c("pre_diabetes", 
                                                                           "diabetes"),
                                                               selected = "diabetes",
                                                               inline = TRUE),
                                                  plotOutput("pyramidPlot"),
                                                  p("Distribution of diabetic
                                                  and prediabetic population with 
                                                    respect to age groups"))
                                      )
                                    )
                             ),
                             column(width = 6,
                                    
                                    style = "background-color: #eff5f5;",
                                    
                                    h3("Factor Contribution In Developing Diabetes", 
                                        style='background-color:#008080;
                                            padding-left: 15px;
                                            color: white'),
                                    
                                    sidebarLayout(
                                      sidebarPanel(
                                        selectInput("option", "Select Factor",
                                                    choices = c("fruits", 
                                                                "heart_disease_or_attack", 
                                                                "high_bp",
                                                                "phys_activity",
                                                                "smoker", 
                                                                "stroke", "veggies"),
                                                    selected = "fruits")
                                      ),
                                      mainPanel(
                                        plotOutput("piePlot")
                                      )
                                    )
                             )
                           )),
                  tabPanel("Predictor",
                           fluidRow(
                             style = "background-color: #eff5f5;",
                             
                             h3("Check Your Diabetic Status!", 
                                style='background-color:#008080;
                                    padding-left: 15px;
                                    color: white'),
                             
                             sidebarLayout(
                               sidebarPanel(
                                 selectInput("Gender", label = "Gender:", 
                                             choices = list("Female" = "0", "Male" = "1"), 
                                             selected = "Female"),
                                 
                                 sliderInput("bmi", "BMI:",
                                             min = 10, max = 100,
                                             value = 70),
                                 
                                 selectInput("age", label = "Age Group:", 
                                             choices = list("18-24" = "1", "25-29" = "2",
                                                            "30-34" = "3", "35-39" = "4",
                                                            "40-44" = "5", "45-49" = "6",
                                                            "50-54" = "7", "55-59" = "8",
                                                            "60-64" = "9", "65-69" = "10",
                                                            "70-74" = "11","75-79" = "12",
                                                            "80+" = "13"), 
                                             selected = "25-29"),
                                 
                                 checkboxInput(inputId = "PhysicalActivity", 
                                               label = "Physical Activity", 
                                               value = FALSE),
                                 
                                 checkboxInput(inputId = "HeartDisease", 
                                               label = "Heart Disease/Attack", 
                                               value = FALSE),
                                 
                                 checkboxInput(inputId = "Highbp", 
                                               label = "High BP", 
                                               value = FALSE),
                                 
                                 checkboxInput(inputId = "smoker", 
                                               label = "Smoker", 
                                               value = FALSE),
                                 
                                 checkboxInput(inputId = "stroke", 
                                               label = "Stroke", 
                                               value = FALSE),
                                 
                                 actionButton("submitbutton", "Submit", 
                                              class = "btn btn-primary")
                               ),
                               
                               # Show a plot of the generated distribution
                               mainPanel(
                                 tags$label(h3('Disease Status Prediction')), # Status/Output Text Box
                                 verbatimTextOutput('contents'),
                                 tableOutput('tabledata') # Prediction results table
                               )
                             )
                           )
                           
                  )# tabPanel
                  
                ) # navbarPage
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$riskFactor <- renderPlot({
    
    # Create the plot
    ggplot(plot_data, aes(x = factor, y = value, fill = diabetic_status)) +
      geom_bar(stat = "identity", position=position_dodge()) +
      labs(x = "Factors", y = "Number of people") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 14, 
                                       face = "bold"),
            text = element_text(size = 14, face = "bold"))
  })
  
  output$riskFactorHeatPlot <- renderPlot({
    
    plot_data$norm <- round(plot_data$value / 
                              nrow(equal_data), 1)
    # Create the plot
    ggplot(plot_data, aes(x = diabetic_status, y = factor, fill = norm))+
      geom_tile(colour="white")+
      geom_text(aes(label = value)) +
      scale_fill_gradient(low = "orange", high = "darkgreen", na.value = "grey80")+
      theme_minimal()+ 
      theme(
        legend.title = element_text(size=12, face="bold"),
        legend.text  = element_text(size=10, face="bold"),
        legend.key.height = grid::unit(1,"cm"),
        legend.key.width  = grid::unit(0.6,"cm"),
        axis.text.x = element_text(angle = 90, vjust=0.05, size=12, face='bold'),
        axis.text.y = element_text(size=12, vjust=0.2, face='bold'),
        axis.ticks = element_line(size=0.4),
        axis.title = element_text(size=12, face="bold")
      )+
      labs(x = "Diabetic Status",
           y = "Risk Factors",
           fill = "Occurences(norm)")
  })
  
  output$piePlot <- renderPlot({
    # Get selected option
    selected_option <- input$option
    
    # Based on the selected option, choose the corresponding data
    filtered_data <- plot_data %>% filter(factor == selected_option)
    
    # Create pie plot
    filtered_data$percent <- round(filtered_data$value / 
                                     sum(filtered_data$value)*100, 1)
    filtered_data$percent_label <- paste0(filtered_data$percent, "%") 
    pie(x = filtered_data$value, labels = filtered_data$percent_label,
        angle = 180,
        col = brewer.pal(n = nrow(filtered_data), name = "Pastel1"),
        radius = 1.05)
    legend("center", legend = c("Diabetic", "Non Diabetic", "Pre Diabetic"),
           fill = brewer.pal(n = nrow(filtered_data), name = "Pastel1"))
  })
  
  output$pyramidPlot <- renderPlot({
    plot_choice <- input$plotType
    filtered_data <- age_gender_data %>%
      filter(diabetic_status == plot_choice) #%>%
    
    apyramid::age_pyramid(data = filtered_data,
                          age_group = "age_cat",
                          split_by = "sex")
  })
  
  # Input Data
  datasetInput <- reactive({  
    
    # outlook,temperature,humidity,windy,play
    df <- data.frame(
      Name = c("bmi",
               "phys_activity",
               "heart_diseaseor_attack",
               "high_bp",
               "smoker",
               "stroke",
               "age",
               "sex"),
      Value = as.character(c(input$bmi,
                             input$PhysicalActivity,
                             input$HeartDisease,
                             input$Highbp,
                             input$smoker,
                             input$stroke,
                             input$age,
                             input$Gender)),
      stringsAsFactors = FALSE)
    
    diabetic_status <- "diabetic_status"
    df <- rbind(df, diabetic_status)
    print(df)
    input <- as.data.frame(t(as.matrix(df)))
    # Set column names to the first row of the transposed dataframe
    colnames(input) <- input[1, ]
    # Remove the first row (column names row)
    transposed_df <- input[-1, ]
    print(input)
    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
    
    test$bmi <- as.numeric(test$bmi)
    test$phys_activity <- as.numeric(as.logical( test$phys_activity))
    test$heart_diseaseor_attack <- as.numeric(as.logical( test$heart_diseaseor_attack))
    test$high_bp <- as.numeric(as.logical( test$high_bp))
    test$smoker <- as.numeric(as.logical( test$smoker))
    test$stroke <- as.numeric(as.logical( test$stroke))
    test$age <- as.numeric(test$age)
    test$sex <- as.numeric(test$sex)
    
    Output <- data.frame(Prediction=predict(model,test), round(predict(model,test,type="prob"), 3))
    #print(Output)
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Prediction completed.") 
    } else {
      return("Ready to predict.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
}

shinyApp(ui = ui, server = server)