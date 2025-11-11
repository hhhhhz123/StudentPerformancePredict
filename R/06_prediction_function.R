#' Predict Exam Score
#' @export
predict_score <- function(model, hours_studied, attendance,
                          motivation_level = "Medium",
                          previous_scores = 75) {

  # Create input
  new_data <- data.frame(
    Hours_Studied = hours_studied,
    Attendance = attendance,
    Parental_Involvement = "Medium",
    Access_to_Resources = "Medium",
    Extracurricular_Activities = "Yes",
    Sleep_Hours = 7,
    Previous_Scores = previous_scores,
    Motivation_Level = motivation_level,
    Internet_Access = "Yes",
    Tutoring_Sessions = 1,
    Family_Income = "Medium",
    Teacher_Quality = "Medium",
    School_Type = "Public",
    Peer_Influence = "Positive",
    Physical_Activity = 3,
    Learning_Disabilities = "No",
    Parental_Education_Level = "College",
    Distance_from_Home = "Near",
    Gender = "Male"
  )

  # Convert to matrix
  X <- model.matrix(~ . - 1, data = new_data)

  # Predict
  pred <- predict(model, newx = X, s = "lambda.min")

  return(as.numeric(pred))
}
