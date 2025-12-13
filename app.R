library(shiny)
library(DBI)
library(RPostgres)
library(caret)
library(dplyr)

## ---- 1. Connect to Amazon RDS PostgreSQL ----
con <- dbConnect(
  Postgres(),
  host     = "database-1.cjguyecq29kw.us-east-2.rds.amazonaws.com",
  port     = 5432,
  user     = "postgres",
  password = "12345678",
  dbname   = "studentdb"
)

## ---- 2. Read data from RDS table ----
dats <- dbReadTable(con, "student_performance")

## ---- 3. numeric / categorical vars ----
num_vars <- c(
  "Hours_Studied",
  "Attendance",
  "Sleep_Hours",
  "Previous_Scores",
  "Tutoring_Sessions",
  "Physical_Activity",
  "Exam_Score"
)

cat_vars <- c(
  "Parental_Involvement",
  "Access_to_Resources",
  "Extracurricular_Activities",
  "Motivation_Level",
  "Internet_Access",
  "Family_Income",
  "Teacher_Quality",
  "School_Type",
  "Peer_Influence",
  "Learning_Disabilities",
  "Parental_Education_Level",
  "Distance_from_Home",
  "Gender"
)


dats[num_vars] <- lapply(dats[num_vars], as.numeric)


dats[cat_vars] <- lapply(dats[cat_vars], factor)
train_levels <- lapply(dats[cat_vars], levels)


train_stepwise <- function(train_data) {
  cv_control <- trainControl(method = "cv", number = 10)
  
  model <- train(
    Exam_Score ~ .,
    data      = train_data,
    method    = "lmStepAIC",
    direction = "backward",
    trControl = cv_control,
    trace     = FALSE
  )
  
  cv_rmse <- model$results$RMSE
  cat("Stepwise CV-RMSE:", round(cv_rmse, 4), "\n")
  
  list(model = model, cv_rmse = cv_rmse)
}

set.seed(123)
fit     <- train_stepwise(dats)
step_lm <- fit$model$finalModel

model_cols <- names(step_lm$coefficients)[-1]


make_dummy_from_model <- function(value, all_levels, base_name) {
  clean_levels <- trimws(all_levels)
  clean_value  <- trimws(value)
  
  
  ref <- clean_levels[1]
  
  
  other_levels <- clean_levels[-1]
  
  out <- list()
  
  for (lev in other_levels) {
    col_name <- paste0(base_name, lev)
    
    if (clean_value == lev) {
      out[[col_name]] <- 1
    } else {
      out[[col_name]] <- 0
    }
  }
  
  return(out)
}
## ================================ UI ================================
ui <- fluidPage(
  titlePanel("Student Exam Score Prediction (Stepwise + RDS)"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Enter all variables except Exam_Score"),
      
      
      sliderInput(
        "Hours_Studied",
        "Hours_Studied (per week)",
        min = min(dats$Hours_Studied, na.rm = TRUE),
        max = max(dats$Hours_Studied, na.rm = TRUE),
        value = median(dats$Hours_Studied, na.rm = TRUE)
      ),
      
      sliderInput(
        "Attendance",
        "Attendance (%)",
        min = min(dats$Attendance, na.rm = TRUE),
        max = max(dats$Attendance, na.rm = TRUE),
        value = median(dats$Attendance, na.rm = TRUE)
      ),
      
      selectInput(
        "Parental_Involvement",
        "Parental_Involvement",
        choices = train_levels$Parental_Involvement
      ),
      
      selectInput(
        "Access_to_Resources",
        "Access_to_Resources",
        choices = train_levels$Access_to_Resources
      ),
      
      selectInput(
        "Extracurricular_Activities",
        "Extracurricular_Activities",
        choices = train_levels$Extracurricular_Activities
      ),
      
      sliderInput(
        "Sleep_Hours",
        "Sleep_Hours (per day)",
        min = min(dats$Sleep_Hours, na.rm = TRUE),
        max = max(dats$Sleep_Hours, na.rm = TRUE),
        value = median(dats$Sleep_Hours, na.rm = TRUE)
      ),
      
      sliderInput(
        "Previous_Scores",
        "Previous_Scores",
        min = min(dats$Previous_Scores, na.rm = TRUE),
        max = max(dats$Previous_Scores, na.rm = TRUE),
        value = median(dats$Previous_Scores, na.rm = TRUE)
      ),
      
      selectInput(
        "Motivation_Level",
        "Motivation_Level",
        choices = train_levels$Motivation_Level
      ),
      
      selectInput(
        "Internet_Access",
        "Internet_Access",
        choices = train_levels$Internet_Access
      ),
      
      sliderInput(
        "Tutoring_Sessions",
        "Tutoring_Sessions (per week)",
        min = min(dats$Tutoring_Sessions, na.rm = TRUE),
        max = max(dats$Tutoring_Sessions, na.rm = TRUE),
        value = median(dats$Tutoring_Sessions, na.rm = TRUE)
      ),
      
      selectInput("Family_Income", "Family_Income", choices = train_levels$Family_Income),
      
      selectInput(
        "Teacher_Quality",
        "Teacher_Quality",
        choices = train_levels$Teacher_Quality
      ),
      
      selectInput("School_Type", "School_Type", choices = train_levels$School_Type),
      
      selectInput("Peer_Influence", "Peer_Influence", choices = train_levels$Peer_Influence),
      
      sliderInput(
        "Physical_Activity",
        "Physical_Activity (hours/week)",
        min = min(dats$Physical_Activity, na.rm = TRUE),
        max = max(dats$Physical_Activity, na.rm = TRUE),
        value = median(dats$Physical_Activity, na.rm = TRUE)
      ),
      
      selectInput(
        "Learning_Disabilities",
        "Learning_Disabilities",
        choices = train_levels$Learning_Disabilities
      ),
      
      selectInput(
        "Parental_Education_Level",
        "Parental_Education_Level",
        choices = train_levels$Parental_Education_Level
      ),
      
      selectInput(
        "Distance_from_Home",
        "Distance_from_Home",
        choices = train_levels$Distance_from_Home
      ),
      
      selectInput("Gender", "Gender", choices = train_levels$Gender),
      
      actionButton("predict_btn", "Predict"),
      width = 4
    ),
    
    mainPanel(
      h3("Prediction Result"),
      verbatimTextOutput("pred_result"),
      hr(),
      h4("Stepwise Model Summary"),
      verbatimTextOutput("model_summary")
    )
  )
)


## ================================ SERVER ================================
server <- function(input, output, session) {
  pred_ci <- eventReactive(input$predict_btn, {
    numeric_values <- list(
      Hours_Studied     = input$Hours_Studied,
      Attendance        = input$Attendance,
      Sleep_Hours       = input$Sleep_Hours,
      Previous_Scores   = input$Previous_Scores,
      Tutoring_Sessions = input$Tutoring_Sessions,
      Physical_Activity = input$Physical_Activity
    )
    
    
    dummy_values <- c(
      make_dummy_from_model(
        input$Parental_Involvement,
        train_levels$Parental_Involvement,
        "Parental_Involvement"
      ),
      
      make_dummy_from_model(
        input$Access_to_Resources,
        train_levels$Access_to_Resources,
        "Access_to_Resources"
      ),
      
      make_dummy_from_model(
        input$Extracurricular_Activities,
        train_levels$Extracurricular_Activities,
        "Extracurricular_Activities"
      ),
      
      make_dummy_from_model(
        input$Motivation_Level,
        train_levels$Motivation_Level,
        "Motivation_Level"
      ),
      
      make_dummy_from_model(
        input$Internet_Access,
        train_levels$Internet_Access,
        "Internet_Access"
      ),
      
      make_dummy_from_model(
        input$Family_Income,
        train_levels$Family_Income,
        "Family_Income"
      ),
      
      make_dummy_from_model(
        input$Teacher_Quality,
        train_levels$Teacher_Quality,
        "Teacher_Quality"
      ),
      
      make_dummy_from_model(
        input$School_Type,
        train_levels$School_Type,
        "School_Type"
      ),
      
      make_dummy_from_model(
        input$Peer_Influence,
        train_levels$Peer_Influence,
        "Peer_Influence"
      ),
      
      make_dummy_from_model(
        input$Learning_Disabilities,
        train_levels$Learning_Disabilities,
        "Learning_Disabilities"
      ),
      
      make_dummy_from_model(
        input$Parental_Education_Level,
        train_levels$Parental_Education_Level,
        "Parental_Education_Level"
      ),
      
      make_dummy_from_model(
        input$Distance_from_Home,
        train_levels$Distance_from_Home,
        "Distance_from_Home"
      ),
      
      make_dummy_from_model(input$Gender, train_levels$Gender, "Gender")
    )
    
    
    all_values <- c(numeric_values, dummy_values)
    
    
    new_x <- as.data.frame(all_values, check.names = FALSE)
    
    predict(step_lm,
            newdata  = new_x,
            interval = "confidence",
            level    = 0.95)
  })
  
  
  output$pred_result <- renderPrint({
    p <- pred_ci()
    if (is.null(p)) {
      cat("Enter variables on the left and click Predict.\n")
    } else {
      cat(sprintf("Predicted Exam_Score: %.2f\n", p[1, "fit"]))
      cat(sprintf("95%% Confidence Interval: [%.2f , %.2f]\n", p[1, "lwr"], p[1, "upr"]))
    }
  })
  
  
  output$model_summary <- renderPrint({
    summary(step_lm)
  })
  
  
  session$onSessionEnded(function() {
    dbDisconnect(con)
  })
}

shinyApp(ui, server)
