# Function: update_agents
# Description: This function simulates one timestep of the agent-based model of infection. 
# Each agent can be susceptible (S), infectious (I), or recovered (R). Infectious agents can infect susceptible agents, and can recover over time.
# Parameters:
# - agents: A vector representing the current status (S, I, or R) of each agent.
# - infection_rate: A number between 0 and 1 indicating the probability that an infectious agent will infect a susceptible one in one timestep.
# - recovery_rate: A number between 0 and 1 indicating the probability that an infectious agent will recover in one timestep.
# - N: The total number of agents.
update_agents <- function(agents, infection_rate, recovery_rate, N) {
	new_infections = 0
	new_recoveries = 0
	for (i in which(agents == "I")) {
	# Infection happens
		if (runif(1) < infection_rate) { 
			contact <- sample(N, 1)
		# If contact is susceptible
			if (agents[contact] == "S") { 
		# Then they become infected
				agents[contact] <- "I" 
				new_infections = new_infections + 1
			}
		}
	# Recovery happens
		if (runif(1) < recovery_rate) { 
			agents[i] <- "R"
			new_recoveries = new_recoveries + 1
		}
	}
	return(list(agents = agents, new_infections = new_infections, new_recoveries = new_recoveries))
}

# Function: generate_ABM_data
# Description: This function generates data by simulating an agent-based model multiple times.
# Parameters:
# - record_frequency: A number indicating how often (in number of timesteps) to record the state of the model.
# - N: The total number of agents.
# - steps: The total number of timesteps to simulate.
# - infection_rate_range: A numeric vector of length 2 indicating the range of possible infection rates.
# - recovery_rate_range: A numeric vector of length 2 indicating the range of possible recovery rates.
# - num_realisations: The number of times to simulate the model.
generate_ABM_data <- function(record_frequency, N, steps, infection_rate_range, recovery_rate_range, num_realisations) {
	all_realisations <- vector("list", num_realisations)
	
	for (r in 1:num_realisations) {
		infection_rate <- runif(1, infection_rate_range[1], infection_rate_range[2])
		recovery_rate <- runif(1, recovery_rate_range[1], recovery_rate_range[2])
		
		agents <- rep("S", N)
		agents[sample(N, 1)] <- "I"
		
		counts <- data.frame(S = rep(0, steps), I = rep(0, steps), R = rep(0, steps))
		incidences <- numeric()
		inputs <- data.frame()
		outputs_Infections <- numeric()
		
	# Run the simulation
		for (t in 1:steps) {
			update_results <- update_agents(agents, infection_rate, recovery_rate, N)
			agents <- update_results$agents
			agent_counts <- table(agents)
			counts[t, ] <- c(agent_counts["S"], agent_counts["I"], agent_counts["R"])
			
			incidences <- c(incidences, update_results$new_infections)
			
			## Currently SIR states will not be recorded with record_frequency of 1 < x 
			if (t %% record_frequency == 0) {
				average_incidence <- mean(tail(incidences, record_frequency))
				inputs <- rbind(inputs, data.frame(Time = t, InfectionRate = infection_rate, RecoveryRate = recovery_rate))
				outputs_Infections <- c(outputs_Infections, average_incidence)
			}
		}
		
		# Fill NA values with 0
		counts[is.na(counts)] <- 0

		all_realisations[[r]] <- list(inputs = inputs, outputs = outputs_Infections, counts = counts)
	}
	
	all_realisations
}