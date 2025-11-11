# Student Performance Prediction

Predict student exam scores based on study habits, family background, and school factors.

## Project Structure
StudentPerformancePredict/
├── data/                  # Dataset
├── R/                     # R functions
├── analysis/              # EDA and model training
├── models/                # Saved models
└── inst/shiny-app/        # Web app

## How to Run

1. Install Packages
install.packages(c("tidyverse", "caret", "glmnet", "shiny"))

2. Run Analysis
rmarkdown::render("analysis/eda.Rmd")
rmarkdown::render("analysis/model_training.Rmd")
rmarkdown::render("analysis/final_evaluation.Rmd")

3. Launch App
shiny::runApp("inst/shiny-app")

## Model Performance
- RMSE: ~4.5 points
- R²: ~0.78

