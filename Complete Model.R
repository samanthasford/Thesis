rich_population <- 5000
poor_population <- 10000
savings_rate <- 0.5
prob_rich_to_poor <- 0.1
prob_poor_to_rich <- 0.05
time_periods <- 40
rich_wage <- 2
poor_wage <- 1
bequest_rate <- 0.5 ##the portion of wealth in retirement which gets bequested
r <- 0.1 #the interest rate

# Cobb-Douglas parameters
A <- 1  # Total factor productivity (could be adjusted)
alpha <- 0.3  # Output elasticity of capital

# Initialize the working list and retired list
wages <-c(rep(rich_wage,rich_population), rep(poor_wage, poor_population))
working <- wages * savings_rate  
retired <- c(rep(0, rich_population+poor_population))  # Initially, the retired list is empty



#class movement
update_class <- function(wages) {
  new_wages <- lapply(wages, function(income) {
    if (income == poor_wage) {
      if (runif(1) < (1-prob_poor_to_rich)) {
        return(poor_wage)
      } 
      else {
        return(rich_wage)
      }
    } 
    else {
      if (runif(1) < (1-prob_rich_to_poor)) {
        return(rich_wage)
      } else {
        return(poor_wage)
      }
    }
  })
  
  return(unlist(new_wages))
}

update_savings <- function(rich_wage,poor_wage, new_rich_wage, new_poor_wage, wages){
  new_working <-lapply(wages, function(income) {
    if (income == poor_wage) {
      return(new_poor_wage)
    } 
    else {
      return(new_rich_wage)
    }
  })
  
  return(unlist(new_working))
}


#Random Death/Bequest Rate #right now a random probability function, in the future something that takes retirement as a function?

#stochastic_bequest <- function(retirement){
  
#}

# Function to compute the total capital (wealth) of the working population
compute_capital <- function(working, retired) {
  # Assuming working agents' income is their savings
  working_capital <- sum(working)
  
  # Retired agents wealth is their savings
  retired_capital <- sum(retired)
  
  # Total capital in the economy is total savings b/c no foreign investment
  total_capital <- working_capital + retired_capital
  return(total_capital)
}

# Function to compute the number of workers
total_labor <- function(working) {
  return(length(working))  # The number of working agents
}


# Function to calculate the marginal product of labor (wage) using the Cobb-Douglas production function
update_wages <- function(wages) {
  #separating working agents into ricch and poor groups
  rich_agents <- wages[wages == rich_wage]  # Agents earning the rich wage
  poor_agents <- wages[wages == poor_wage]  # Agents earning the poor wage
  
  
  # Compute total capital (wealth)
  total_capital <- compute_capital(working, retired)
  rich_capital <- sum(rich_agents)  # Capital for rich agents
  poor_capital <- sum(poor_agents)  # Capital for poor agents
  
  
  # Total labor (number of working agents)
  labor <- total_labor(wages)
  rich_labor <- length(rich_agents)
  poor_labor <- length(poor_agents)
  
  # Cobb-Douglas production function for the marginal product of labor (MPL)
  total_output <- A * total_capital^alpha * labor^(1 - alpha)  # Total output in the economy
  wage <- (1 - alpha) * total_output / labor  # MPL = wage per worker
  
  new_rich_wage <- (1 - alpha) * total_output * (rich_capital / total_capital)^alpha / rich_labor
  new_poor_wage <- (1 - alpha) * total_output * (poor_capital / total_capital)^alpha / poor_labor
  return(c(new_rich_wage,new_poor_wage))
}

av_savings <- function(wages, retired) {
  rich_agents <- retired[wages == rich_wage]  # Agents earning the rich wage
  poor_agents <- retired[wages == poor_wage]  # Agents earning the poor wage
  rich_savings <- mean(rich_agents)
  poor_savings <- mean(poor_agents)
  return(c(rich_savings, poor_savings))
}

# Initialize vectors to store average savings over time
rich_avg_savings <- c()
poor_avg_savings <- c()

tracking_wages_rich <- c()
tracking_wages_poor <- c()

############ Main simulation loop##########################
for (t in 1:(time_periods - 1)) {
  #print(paste("Time Period:", t))
  #print("Working List:")
  #print(working)
  #print("Retired List:")
  #print(retired)
  
  
  # Compute capital and labor
  #total_capital <- compute_capital(working, retired)
  #labor <- total_labor(working)
  
  #Updated Bequest Rate
  #bequest_rate <- 
    
  # Update the retired list with new retirees and their inheritance
  retired <- bequest_rate*retired + working
  
  
  #for graph
  avg_savings <- av_savings(wages,retired)
  rich_avg_savings <- c(rich_avg_savings, avg_savings[1])
  poor_avg_savings <- c(poor_avg_savings, avg_savings[2])
  tracking_wages_rich <- c(tracking_wages_rich, rich_wage)
  tracking_wages_poor <- c(tracking_wages_poor, poor_wage)
  
  #get new wage rates
  new_rich_wage <- update_wages(wages)[1]
  new_poor_wage <- update_wages(wages)[2]
  
  # Update the working list with class movement of new generation
  wages <- update_class(wages)
  #update working incomes
  wages <-update_savings(rich_wage, poor_wage, new_rich_wage,new_poor_wage, wages)
  working <- wages *savings_rate
  rich_wage <- new_rich_wage
  poor_wage <- new_poor_wage
  
  
}


############ Graph ##################

# Plotting the average savings over time
time_periods_plot <- 1:(time_periods - 1)

plot(time_periods_plot, rich_avg_savings, type = "l", col = "blue", ylim = range(c(rich_avg_savings, poor_avg_savings)),
     xlab = "Time Period", ylab = "Average Savings at Retirement",
     main = "Average Savings at Retirement Over Time")
lines(time_periods_plot, poor_avg_savings, col = "red")
legend("topright", legend = c("Rich", "Poor"), col = c("blue", "red"), lty = 1)
