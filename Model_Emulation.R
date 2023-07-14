# Function: emulate_model
# Description: This function emulates the agent-based model for a given set of parameters, using a trained emulator.
# Parameters:
# - emulator: An object representing a trained emulator.
# - emulator_type: A string indicating the type of emulator used. Must be either "GP" for Gaussian Process, or "NN" for Neural Network.
# - start_time: A number representing the starting time of the emulation.
# - end_time: A number representing the ending time of the emulation.
# - infection_rate: A number between 0 and 1 indicating the infection rate used in the agent-based model.
# - recovery_rate: A number between 0 and 1 indicating the recovery rate used in the agent-based model.
emulate_model <- function(emulator, emulator_type, start_time, end_time, infection_rate, recovery_rate) {
  
  predictions <- predict_emulator(emulator, emulator_type, start_time, end_time, infection_rate, recovery_rate)
  
  list(predictions = predictions)
}