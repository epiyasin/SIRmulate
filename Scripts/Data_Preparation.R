# Function: prepare_data
# Description: This function prepares data for plotting.
# Parameters:
# - incidence: A numeric vector containing the incidence (number of new infections) at each recorded timestep.
# - model: A string indicating the type of model from which the data comes. Must be either "ABM" for the agent-based model, "GP" for the Gaussian Process emulator, or "NN" for the Neural Network emulator.
prepare_data <- function(incidence, model) {
  data.frame(
    Time = seq(1, steps, record_frequency),
    Incidence = incidence,
    Model = model
  )
}