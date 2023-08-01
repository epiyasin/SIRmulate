setwd(dir = "Scripts/")

source("Global.R")
source("ABM_Functions.R")
source("Emulator_Functions.R")
source("Model_Training.R")
source("Model_Emulation.R")
source("Data_Preparation.R")

# Using the functions
# Generate multiple realisations of the ABM model
record_frequency <- 20
steps <- 500
N <- 100000
infection_rate_range <- c(0.1, 0.2)
recovery_rate_range <- c(0.05, 0.15)
num_realisations <- 10#100

# Generate data by simulating ABM multiple times
abm_data <- generate_ABM_data(record_frequency = record_frequency,
                              N = N,
                              steps = steps,
                              infection_rate_range = infection_rate_range,
                              recovery_rate_range = recovery_rate_range,
                              num_realisations = num_realisations)

# Set the size of the training set
training_set <- 2/3

# Train Gaussian Process (GP) and Neural Network (NN) emulators using the generated data
gp_emulator <- train_model("GP", abm_data, training_set = training_set)
nn_emulator <- train_model("NN", abm_data, training_set = training_set)

# Set parameters for emulation
start_time <- 1
end_time <- 500
infection_rate <- 0.15
recovery_rate <- 0.1

# Emulate the ABM using the trained GP and NN emulators
gp_results <- emulate_model(gp_emulator, "GP", start_time = start_time, end_time = end_time, infection_rate = infection_rate, recovery_rate = recovery_rate)
nn_results <- emulate_model(nn_emulator, "NN", start_time = start_time, end_time = end_time, infection_rate = infection_rate, recovery_rate = recovery_rate)

# Extract outputs from ABM data
abm_output_subsets <- lapply(abm_data, function(x) x$outputs)

# Calculate the average of ABM outputs
abm_average_values <- sapply(1:length(abm_output_subsets[[1]]), function(i) mean(sapply(abm_output_subsets, `[[`, i), na.rm = TRUE))
abm_average_values

# Prepare data for summary and plotting
tibble::tibble(
  Time = start_time:end_time,
  GP_output = gp_results$predictions,
  NN_output = nn_results$predictions
)

## Store output for plotting