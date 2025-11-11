# Student Performance Prediction

Predict student exam scores based on study habits, family background, and school factors.

## Project Structure
1. Load data
2. Do EDA
3. Split data (train/test)
4. Model selection (stepwise, lasso) WITH CV on training data
5. Select best model based on CV performance
6. Refit best model on full training data
7. Evaluate on test data (diagnosis)
8. Create prediction function
9. Build Shiny app using prediction function

## How to Run
0. Clone

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

