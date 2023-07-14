# Combine data from ABM and emulations for plotting
combined_data <- bind_rows(
  prepare_data(abm_average_values, "ABM"),
  prepare_data(gp_results$predictions[seq(1, steps, record_frequency),], "GP"),
  prepare_data(nn_results$predictions[seq(1, steps, record_frequency),], "NN")
)

# Convert data to long format for plotting
combined_data_long <- combined_data %>%
  gather(key = "Status", value = "Count", -c(Time, Model))

# Plot the results
ggplot(combined_data_long, aes(x = Time, y = Count, colour = Model, linetype = Model)) +
  geom_line() +
  geom_point() +
  labs(title = "SIR model: ABM vs GP vs NN", x = "Time", y = "Count", colour = "Model") +
  scale_linetype_discrete(name = "Model", labels = c("ABM", "GP", "NN")) +
  scale_x_continuous(limits = c(0, NA)) +
  scale_y_continuous(limits = c(0, NA)) +
  theme_minimal()