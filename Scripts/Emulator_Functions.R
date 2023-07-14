# Function: train_emulator
# Description: This function trains an emulator, either a Gaussian Process (GP) or a Neural Network (NN), based on the inputs and outputs from the agent-based model.
# Parameters:
# - model_type: A string indicating the type of emulator to train. Must be either "GP" for Gaussian Process, or "NN" for Neural Network.
# - inputs: A data frame where each row represents an observation, and each column represents an input variable to the model.
# - outputs_Infections: A numeric vector containing the output variable (number of infections) for each observation.
train_emulator <- function(model_type, inputs, outputs_Infections) {
  if (model_type == "GP") {
    gausspr(outputs_Infections ~ ., kernel = "rbfdot", data = inputs)
  } else if (model_type == "NN") {
    # Preprocess inputs and outputs
    input_matrix <- as.matrix(inputs)
    output_matrix <- as.matrix(outputs_Infections)
    
    # Build the NN model
    model <- keras_model_sequential() %>%
      layer_dense(units = 64, activation = 'relu', input_shape = ncol(input_matrix)) %>%
      layer_dense(units = 64, activation = 'relu') %>%
      layer_dense(units = 1)
    
    # Compile the model
    model %>% compile(
      optimizer = optimizer_adam(),
      loss = 'mean_squared_error'
    )
    
    # Train the model
    history <- model %>% fit(
      x = input_matrix,
      y = output_matrix,
      epochs = 2000,
      validation_split = 0.3
    )
    
    model
  } else {
    stop("Invalid model type. Please choose 'GP' or 'NN'.")
  }
}

# Function: predict_emulator
# Description: This function makes predictions using an already trained emulator.
# Parameters:
# - emulator: An object representing a trained emulator.
# - model_type: A string indicating the type of emulator used. Must be either "GP" for Gaussian Process, or "NN" for Neural Network.
# - start_time: A number representing the starting time of the predictions.
# - end_time: A number representing the ending time of the predictions.
# - infection_rate: A number between 0 and 1 indicating the infection rate used in the agent-based model.
# - recovery_rate: A number between 0 and 1 indicating the recovery rate used in the agent-based model.
predict_emulator <- function(emulator, model_type, start_time, end_time, infection_rate, recovery_rate) {
  prediction_data <- data.frame(
    Time = start_time:end_time,
    InfectionRate = rep(infection_rate, length(start_time:end_time)),
    RecoveryRate = rep(recovery_rate, length(start_time:end_time))
  )
  
  if (model_type == "GP") {
    predict(emulator, newdata = prediction_data)
  } else if (model_type == "NN") {
    predict(emulator, as.matrix(prediction_data))
  } else {
    stop("Unsupported emulator type.")
  }
}