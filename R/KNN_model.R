
# function to clean the fluxes data
clean_flux_data <- function(file_path) {
  daily_fluxes <- readr::read_csv(file_path) |>  
    # Select only the relevant variables
    dplyr::select(
      TIMESTAMP, 
      GPP_NT_VUT_REF,         # target variable
      NEE_VUT_REF_QC,
      TA_F, TA_F_QC,
      SW_IN_F, SW_IN_F_QC,
      VPD_F, VPD_F_QC
    ) |>
    
    # Convert TIMESTAMP to Date format
    dplyr::mutate(TIMESTAMP = lubridate::ymd(TIMESTAMP)) |>
    
    # Replace -9999 with NA
    dplyr::mutate(across(where(is.numeric), ~na_if(., -9999))) |> 
    
    # Set values to NA based on QC threshold (< 0.8)
    dplyr::mutate(
      GPP_NT_VUT_REF = ifelse(NEE_VUT_REF_QC < 0.8, NA, GPP_NT_VUT_REF),
      TA_F           = ifelse(TA_F_QC        < 0.8, NA, TA_F),
      SW_IN_F        = ifelse(SW_IN_F_QC     < 0.8, NA, SW_IN_F),
      VPD_F          = ifelse(VPD_F_QC       < 0.8, NA, VPD_F),
    ) |> 
   
    # Drop QC variables
    dplyr::select(-ends_with("_QC"))
  
  
  return(daily_fluxes)
}


# Train a k-NN regression model with data splitting, preprocessing, and cross-validation
train_knn_model <- function(train_data, response = "GPP_NT_VUT_REF", predictors = c("TA_F", "SW_IN_F", "VPD_F"), 
                            k_values = c(2, 5, 10, 15, 20, 25, 30, 35, 40, 60, 100),
                            cv_folds = 10,
                            seed_train = 7197) {

  # define preprocessing recipe
  pp <- recipe(as.formula(paste(response, "~", paste(predictors, collapse = " + "))),
               data = train_data) |> 
    step_center(all_numeric(), -all_outcomes()) |> 
    step_scale(all_numeric(), -all_outcomes())
  
  # train knn model
  set.seed(seed_train)
  model <- caret::train(pp,
                        data = drop_na(train_data),
                        method = "knn",
                        trControl = trainControl(method = "cv", number = cv_folds),
                        tuneGrid = data.frame(k = k_values),
                        metric = "RMSE")
  
  return(list(
    model = model,
    train_data = train_data
  ))
}


# Function to evaluate a model on multiple test sets and format the result into a table
evaluate_model_across_sets <- function(model_result, test_data_list, model_label) {
  # Apply the evaluation to each test set and attach the evaluation set label
  eval_results <- purrr::map2(test_data_list, names(test_data_list), function(test_data, label) {
    eval_model(
      model_result$model, 
      df_train = model_result$train_data, 
      df_test = test_data,
      return_metrics = TRUE
    )$test |> 
      dplyr::mutate(evaluation_set = label)
  }) |> dplyr::bind_rows()
  
  # Format the result: keep relevant columns, pivot metrics to wide format, and rename columns
  eval_results |> 
    dplyr::select(evaluation_set, .metric, .estimate) |> 
    tidyr::pivot_wider(names_from = .metric, values_from = .estimate) |> 
    dplyr::rename_with(~c(model_label, "RMSE", "R²", "MAE"))
}
