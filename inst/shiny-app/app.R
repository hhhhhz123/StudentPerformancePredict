# Student Performance Prediction Shiny App
# Simple version

library(shiny)
library(caret)

# Load the trained model
model <- readRDS("../../models/final_model.rds")

# Load training data for factor levels
load("../../data/processed/train_data.rda")

# Get factor levels from training data
factor_levels <- list()
for (col in names(train_data)) {
  if (is.factor(train_data[[col]]) && col != "Exam_Score") {
    factor_levels[[col]] <- levels(train_data[[col]])
  }
}

# UI
ui <- fluidPage(
  titlePanel("Student Exam Score Predictor"),

  sidebarLayout(
    sidebarPanel(
      h4("Enter Student Information:"),

      # Numeric inputs
      sliderInput("Hours_Studied", "Hours Studied per Week:",
                  min = 1, max = 44, value = 20),

      sliderInput("Attendance", "Attendance (%):",
                  min = 60, max = 100, value = 80),

      sliderInput("Sleep_Hours", "Sleep Hours per Day:",
                  min = 4, max = 10, value = 7, step = 0.5),

      sliderInput("Previous_Scores", "Previous Exam Score:",
                  min = 50, max = 100, value = 75),

      sliderInput("Tutoring_Sessions", "Tutoring Sessions per Month:",
                  min = 0, max = 8, value = 2),

      sliderInput("Physical_Activity", "Physical Activity Hours per Week:",
                  min = 0, max = 6, value = 3),

      # Categorical inputs
      selectInput("Parental_Involvement", "Parental Involvement:",
                  choices = factor_levels$Parental_Involvement),

      selectInput("Access_to_Resources", "Access to Resources:",
                  choices = factor_levels$Access_to_Resources),

      selectInput("Extracurricular_Activities", "Extracurricular Activities:",
                  choices = factor_levels$Extracurricular_Activities),

      selectInput("Motivation_Level", "Motivation Level:",
                  choices = factor_levels$Motivation_Level),

      selectInput("Internet_Access", "Internet Access:",
                  choices = factor_levels$Internet_Access),

      selectInput("Family_Income", "Family Income:",
                  choices = factor_levels$Family_Income),

      selectInput("Teacher_Quality", "Teacher Quality:",
                  choices = factor_levels$Teacher_Quality),

      selectInput("School_Type", "School Type:",
                  choices = factor_levels$School_Type),

      selectInput("Peer_Influence", "Peer Influence:",
                  choices = factor_levels$Peer_Influence),

      selectInput("Learning_Disabilities", "Learning Disabilities:",
                  choices = factor_levels$Learning_Disabilities),

      selectInput("Parental_Education_Level", "Parental Education Level:",
                  choices = factor_levels$Parental_Education_Level),

      selectInput("Distance_from_Home", "Distance from Home:",
                  choices = factor_levels$Distance_from_Home),

      selectInput("Gender", "Gender:",
                  choices = factor_levels$Gender),

      br(),
      actionButton("predict", "Predict Score", class = "btn-primary btn-lg")
    ),

    mainPanel(
      h3("Prediction Result:"),

      div(style = "padding: 20px; margin-top: 20px;",
          uiOutput("prediction_box")
      ),

      hr(),

      h4("Model Information:"),
      verbatimTextOutput("model_info"),

      hr(),

      h4("About This Predictor:"),
      p("This app predicts student exam scores based on various factors including:"),
      tags$ul(
        tags$li("Study habits (hours studied, attendance)"),
        tags$li("Personal factors (sleep, motivation, previous scores)"),
        tags$li("Family factors (parental involvement, income, education)"),
        tags$li("School factors (resources, teacher quality, peer influence)"),
        tags$li("Other factors (tutoring, physical activity)")
      ),
      p("Enter the student's information and click 'Predict Score' to see the expected exam score.")
    )
  )
)

# Server
server <- function(input, output, session) {

  # Make prediction when button is clicked
  prediction <- eventReactive(input$predict, {

    # Create input data frame
    input_data <- data.frame(
      Hours_Studied = input$Hours_Studied,
      Attendance = input$Attendance,
      Parental_Involvement = factor(input$Parental_Involvement,
                                    levels = factor_levels$Parental_Involvement),
      Access_to_Resources = factor(input$Access_to_Resources,
                                   levels = factor_levels$Access_to_Resources),
      Extracurricular_Activities = factor(input$Extracurricular_Activities,
                                          levels = factor_levels$Extracurricular_Activities),
      Sleep_Hours = input$Sleep_Hours,
      Previous_Scores = input$Previous_Scores,
      Motivation_Level = factor(input$Motivation_Level,
                                levels = factor_levels$Motivation_Level),
      Internet_Access = factor(input$Internet_Access,
                               levels = factor_levels$Internet_Access),
      Tutoring_Sessions = input$Tutoring_Sessions,
      Family_Income = factor(input$Family_Income,
                             levels = factor_levels$Family_Income),
      Teacher_Quality = factor(input$Teacher_Quality,
                               levels = factor_levels$Teacher_Quality),
      School_Type = factor(input$School_Type,
                           levels = factor_levels$School_Type),
      Peer_Influence = factor(input$Peer_Influence,
                              levels = factor_levels$Peer_Influence),
      Physical_Activity = input$Physical_Activity,
      Learning_Disabilities = factor(input$Learning_Disabilities,
                                     levels = factor_levels$Learning_Disabilities),
      Parental_Education_Level = factor(input$Parental_Education_Level,
                                        levels = factor_levels$Parental_Education_Level),
      Distance_from_Home = factor(input$Distance_from_Home,
                                  levels = factor_levels$Distance_from_Home),
      Gender = factor(input$Gender,
                      levels = factor_levels$Gender),
      stringsAsFactors = FALSE
    )

    # Make prediction
    pred <- predict(model, newdata = input_data)

    return(round(pred, 1))
  })

  # Display prediction
  output$prediction_box <- renderUI({
    pred <- prediction()

    # Color based on score
    if (pred >= 80) {
      color <- "success"
      emoji <- "🎉"
      message <- "Excellent performance expected!"
    } else if (pred >= 70) {
      color <- "info"
      emoji <- "👍"
      message <- "Good performance expected!"
    } else if (pred >= 60) {
      color <- "warning"
      emoji <- "⚠️"
      message <- "Average performance expected."
    } else {
      color <- "danger"
      emoji <- "📚"
      message <- "More effort needed."
    }

    div(class = paste0("alert alert-", color),
        style = "font-size: 20px; text-align: center;",
        h2(emoji, " Predicted Exam Score: ", strong(pred), "/100 ", emoji),
        p(message)
    )
  })

  # Model info
  output$model_info <- renderPrint({
    cat("Model Type:", class(model)[1], "\n")
    cat("Number of predictors:", length(names(train_data)) - 1, "\n")
    cat("Training data size:", nrow(train_data), "observations\n")
  })
}

# Run the app
shinyApp(ui = ui, server = server)
