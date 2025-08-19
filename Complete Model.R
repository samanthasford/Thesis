library(tidyverse)
set.seed(123)
rich_population <- 50
poor_population <- 100

rich_wage <- 0.4
poor_wage <- .2
#savings = c(rep(1.5, rich_population), rep(0.5, poor_population))

#initial interest rate
r <- 0.1
depreciation <- 0.07
R <- 1 + r - depreciation

delta <- 0.3
zeta <- 0
eta <- 1
sigma <- 0.9

#constants for optimization later
k <- (delta / zeta) ^ (1 / sigma)
A <- (R ^ (1 - sigma) * (delta + zeta * k ^ (1 - sigma))) / (1 + k) ^ (1 - sigma)





#### Random Class Determination
#prob_poor_to_rich <- 0.05
#prob_rich_to_poor <- 0.1
prob_poor_to_rich <- 0.05
prob_rich_to_poor <- 0.1

update_class <- function(type) {
  new_type <- ifelse(
    type == "poor",
    ifelse(runif(length(type)) < prob_poor_to_rich, "rich", "poor"),
    ifelse(runif(length(type)) < prob_rich_to_poor, "poor", "rich")
  )
  return(new_type)
}

### Compute Capital

#doesn't take into consideration inheritance to dead agents, capital to low???
compute_capital <- function(old, young) {
  #total_capital <- sum(old$wealth)
  
  surviving_old_wealth <- sum(old$wealth[old$death_age >= 60])
  orphan_wealth <- sum(young$available_bequest_young[young$orphan == 1])
  
  total_capital <- surviving_old_wealth + orphan_wealth
  
  return(total_capital)
}

### Wage Rates

alpha <- 0.3
beta <- 0.64
gamma <- 0.06


update_wages <- function(young, old) {
  total_capital <- compute_capital(old, young)
  
  rich_labor <- sum(young$type == "rich")
  poor_labor <- sum(young$type == "poor")
  
  new_rich_wage <- beta * total_capital ^ alpha * rich_labor ^ (beta - 1) * poor_labor ^
    gamma
  new_poor_wage <- gamma * total_capital ^ alpha * rich_labor ^ beta * poor_labor ^
    (gamma - 1)
  
  young$wages[young$type == "rich"] <- new_rich_wage
  young$wages[young$type == "poor"] <- new_poor_wage
  
  return(young)
}

## Interest Rate #########

update_interest <- function(young, old) {
  total_capital <- compute_capital(old, young)
  rich_labor <- sum(young$type == "rich")
  poor_labor <- sum(young$type == "poor")
  
  new_mpk <- alpha * total_capital ^ (alpha - 1) * rich_labor ^ (beta) * poor_labor ^
    gamma
  
  return(new_mpk)
}


### Random Death Function

Social_Security_Deaths_2021 <- read_csv("~/Thesis/Social Security Deaths 2021.csv")


cdf_data <- Social_Security_Deaths_2021 |>
  mutate(cdf = `total deaths` / 205000)


#inverse transform sampling
inverse_transform_sampling <- function(cdf_data, n_samples) {
  uniform_randoms <- runif(n_samples)
  sampled_values <- sapply(uniform_randoms, function(u) {
    age_sampled <- cdf_data$age[min(which(cdf_data$cdf >= u))]
    return(age_sampled)
  })
  
  return(sampled_values)
}


death_age <- inverse_transform_sampling(cdf_data, rich_population + poor_population)


##### Inner Optimization Problem (bequests in old age)

optimize_bequest <- function(wealth) {
  inner_utility <- function(b_tp1) {
    c_tp1 <- wealth - b_tp1
    return(delta * u(c_tp1, eta) + zeta * v(b_tp1, sigma))
  }
  
  result <- optimize(inner_utility,
                     interval = c(0.001, wealth - 0.001),
                     maximum = TRUE)
  
  return(list(b_tp1 = result$maximum, c_tp1 = wealth - result$maximum))
}


##################Optimization#######################

#utility functions
u <- function(c, eta) {
  if (eta == 1)
    return(log(c))
  return(c ^ (1 - eta) / (1 - eta))
}

v <- function(b, sigma) {
  if (sigma == 1)
    return(log(b))
  return(b ^ (1 - sigma) / (1 - sigma))
}


cdf_data <- Social_Security_Deaths_2021 |>
  mutate(cdf = `total deaths` / 205000)

true_prob_deaths <- Social_Security_Deaths_2021 |>
  mutate(deaths_at_age = `total deaths` - lag(`total deaths`, default = first(`total deaths`))) |>
  mutate(prob_of_death = deaths_at_age / 205000)

p_after_60 <- true_prob_deaths |>
  filter(age >= 60, age <= 100) |>
  mutate(p_i = prob_of_death / sum(prob_of_death))
p_i = p_after_60$p_i


generate_realized_bequests <- function(death_age, planned_c_t, planned_b_t) {
  b_realized <- planned_b_t + planned_c_t * (100 - death_age) / 40
  return(b_realized)
}


#optimize c_t

optimize_c_t <- function(w, expected_bequest_path) {
  lifetime_utility <- function(c_t) {
    future_resources <- w - c_t + expected_bequest_path
    
    utility_today <- u(c_t, sigma)
    utility_future <- sum(p_i * A * future_resources ^ (1 - sigma), na.rm = TRUE) / (1 - sigma)
    
    
    return(utility_today + utility_future)
  }
  
  opt <- optimize(lifetime_utility,
                  interval = c(0.001, w - 0.001),
                  maximum = TRUE)
  
  return(opt$maximum)
}




optimize_orphan_consumption <- function(w, b_t) {
  # Define lifetime utility function in terms of c_t only
  lifetime_utility <- function(c_t) {
    if (c_t <= 0 || c_t >= w + R * b_t)
      return(-Inf)
    
    u_prime <- c_t ^ (-eta)
    
    if (zeta == 0) {
      b_tp1 <- 0
    } else {
      b_tp1 <- (u_prime / (R * zeta))^(-1 / sigma)
    }
    
    c_tp1 <- R * (w + R * b_t - c_t) - b_tp1
    
    if (c_tp1 <= 0) return(-Inf)  # Optional sanity check
    
    U <- u(c_t, eta) + delta * u(c_tp1, eta) + zeta * v(b_tp1, sigma)
    return(U)
  }
  
  result <- optimize(lifetime_utility, interval = c(0.001, w + R * b_t - 0.001), maximum = TRUE)
  return(result$maximum)
}



############Update Savings#######################
update_savings <- function(young, previous_old) {
  non_orphans <- young |>
    filter(orphan == 0) |>
    rowwise() |>
    mutate(
      parent_b_tp1 = previous_old$bequest_amount[id],
      parent_c_tp1 = previous_old$planned_c_tp1[id],
      
      
      b_t_realized_path = list(
        generate_realized_bequests(60:100, parent_c_tp1, parent_b_tp1)
      ),
      
      
      optimal_ct = optimize_c_t(wages, expected_bequest_path = b_t_realized_path[[1]]),
      
      
      savings = wages - optimal_ct
    ) |>
    ungroup() |>
    select(-parent_b_tp1, -parent_c_tp1, -b_t_realized_path)
  
  
  
  orphans <- young |>
    filter(orphan != 0) |>
    rowwise() |>
    mutate(
      optimal_ct = optimize_orphan_consumption(wages, available_bequest_young),
      
      
      savings = wages + R * available_bequest_young - optimal_ct
    ) |>
    ungroup()
  
  
  young <- bind_rows(non_orphans, orphans) |> arrange(id)
  return(young)
}
#young_consumption <- young$optimal_ct


### Gini Coefficient

gini_coefficient <- function(x) {
  x <- x[!is.na(x)]  # Remove NAs
  n <- length(x)
  if (n == 0)
    return(NA)
  x_sorted <- sort(x)
  index <- seq_along(x_sorted)
  G <- (2 * sum(index * x_sorted) - (n + 1) * sum(x_sorted)) / (n * sum(x_sorted))
  return(G)
}


# Initialize the "young" dataframe
young <- data.frame(
  id = 1:(rich_population + poor_population),
  type = c(rep("rich", rich_population), rep("poor", poor_population)),
  wages = c(
    rep(rich_wage, rich_population),
    rep(poor_wage, poor_population)
  ),
  savings_rate = c(rep(0.8, rich_population), rep(0.1, poor_population)),
  savings = c(
    rep(rich_wage * 0.5, rich_population),
    rep(poor_wage * 0.1, poor_population)
  ),
  death_age = inverse_transform_sampling(cdf_data, rich_population + poor_population),
  bequest_recieved = c(rep(0, rich_population + poor_population)),
  orphan = rep(0, rich_population + poor_population),
  available_bequest_young = 0,
  optimal_ct = 0
  
)


# Initialize the "old" dataframe
old <- data.frame(
  id = integer(),
  type = character(),
  wages = numeric(),
  bequest_recieved = numeric(),
  savings = numeric(),
  #bequest_rate = numeric(),
  bequest_amount = numeric(),
  death_age = numeric(),
  wealth = numeric(),
  optimal_ct = numeric()
)

# Dataframe to track wealth over time
wealth_history <- data.frame(
  time = integer(),
  type = character(),
  total_wealth = numeric(),
  r = numeric()
)

inequality_metrics <- data.frame(time = numeric(), gini = numeric(), gini_consumption = numeric())

####################### Main Simulation Loop ########################################
time_periods <- 200
for (t in 1:(time_periods - 1)) {
  if (t == 1) {
    previous_old <- young
    #  previous_old$wealth <- c(rep(1.0, rich_population), rep(0.5, poor_population))
    #  previous_old$bequest_amount <- c(rep(0.3, rich_population), rep(0.07, poor_population))
    #old$actual_bequest_from_parent <- c(rep(1, rich_population), rep(0.1, poor_population))
  }
  
  old <- young
  
  
  old$actual_bequest_from_parent <- 0
  
  non_orphan_indices <- which(old$orphan == 0)
  
  
  if (!"realized_bequest" %in% names(previous_old)) {
    previous_old$realized_bequest <- rep(0, nrow(previous_old))
  }
  
  
  old$actual_bequest_from_parent[non_orphan_indices] <- previous_old$realized_bequest[old$id[non_orphan_indices]]
  
  
  
  
  old$wealth <- (old$savings + old$actual_bequest_from_parent) * (1 + r - depreciation)
  
  if (t == 1) {
    old$wealth <- c(rep(0.2, rich_population), rep(0.05, poor_population))
  }
  
  
  old <- old |>
    rowwise() |>
    mutate(
      opt = if (death_age < 60) {
        list(list(b_tp1 = 0, c_tp1 = wages - savings))  # died young
      } else {
        list(optimize_bequest(wealth = wealth))
      },
      bequest_amount = opt$b_tp1,
      planned_c_tp1 = opt$c_tp1
    ) |>
    ungroup() |>
    select(-opt)
  
  
  old <- old |>
    mutate(realized_bequest = ifelse(
      death_age >= 60,
      bequest_amount + planned_c_tp1 * (100 - death_age) / 40,
      0
    ))
  
  old$total_cons <- (old$optimal_ct+old$wealth-old$realized_bequest)
  
  gini <- gini_coefficient(old$wealth)
  gini_consumption <- gini_coefficient(old$total_cons)
  
  inequality_metrics <- rbind(inequality_metrics, data.frame(time = t, gini = gini, gini_consumption=gini_consumption))
  
  rich_wealth <- sum(old$wealth[old$type == "rich"], na.rm = TRUE) / rich_population
  poor_wealth <- sum(old$wealth[old$type == "poor"], na.rm = TRUE) / poor_population
  rich_savings <- mean(old$savings[old$type == "rich"], na.rm = TRUE)
  poor_savings <- mean(old$savings[old$type == "poor"], na.rm = TRUE)
  rich_bequest <- mean(old$bequest_amount[old$type == "rich"], na.rm = TRUE)
  poor_bequest <- mean(old$bequest_amount[old$type == "poor"], na.rm = TRUE)
  
  wealth_history <- rbind(
    wealth_history,
    data.frame(
      time = c(t, t),
      type = c("rich", "poor"),
      total_wealth = c(rich_wealth, poor_wealth),
      avg_savings = c(rich_savings, poor_savings),
      avg_bequest = c(rich_bequest, poor_bequest),
      r = R - depreciation
    )
  )
  
  young <- data.frame(
    id = 1:(rich_population + poor_population),
    type = update_class(old$type),
    wages = NA,
    savings = 0,
    death_age = inverse_transform_sampling(cdf_data, rich_population + poor_population),
    orphan = case_when(
      old$death_age >= 60 ~ 0,
      old$death_age >= 20 & old$death_age < 60 ~ 1,
      old$death_age < 20 ~ 2
    )
  )
  
  #young$available_bequest_young <- case_when(
  #  young$orphan == 1 ~ pmax(0, old$wealth - (old$wages - old$savings) * (old$death_age - 20) / 40),
  #  TRUE ~ 0
  #)
  
  parent_wealth_residual <- pmax(0,
                                 old$wealth - (old$wages - old$savings) * (old$death_age - 20) / 40)
  
  grandparent_bequest <- ifelse(old$death_age < 60, previous_old$realized_bequest[old$id], 0)
  
  young$available_bequest_young <- case_when(young$orphan == 1 ~ parent_wealth_residual + grandparent_bequest,
                                             TRUE ~ 0)
  
  
  young <- update_wages(young, old)
  
  young <- update_savings(young = young, previous_old = old)
  
  #print(R)
  #print(compute_capital(old,young))
  R <- 1 + update_interest(young, old) - depreciation
  #print(R)
  
  total_savings <- sum(young$savings)
  total_bequests <- sum(old$bequest_amount)
  #print(sprintf("t=%d | Savings: %.2f | Bequests: %.2f | Capital: %.2f | R: %.3f",
  #              t, total_savings, total_bequests, compute_capital(old, young), R))
  previous_old <- old
  
  
  avg_rich_wage <- mean(young$wages[young$type == "rich"], na.rm = TRUE)
  avg_poor_wage <- mean(young$wages[young$type == "poor"], na.rm = TRUE)
  
  wage_history <- rbind(
    if (!exists("wage_history")) data.frame(time = integer(), rich_wage = numeric(), poor_wage = numeric()),
    data.frame(time = t, rich_wage = avg_rich_wage, poor_wage = avg_poor_wage)
  )
  
  
}


wealth_inequality <- wealth_history %>%
  pivot_wider(
    names_from = type,
    values_from = c(total_wealth, avg_savings, avg_bequest),
    names_sep = "_"
  ) %>%
  mutate(
    wealth_ratio = total_wealth_rich / total_wealth_poor,
    wealth_gap = total_wealth_rich - total_wealth_poor,
    share_rich = total_wealth_rich * rich_population /
      (
        total_wealth_rich * rich_population + total_wealth_poor * poor_population
      )
  )


ggplot(wealth_inequality, aes(x = time)) +
  geom_line(aes(y = share_rich), linewidth = 1.2) +
  labs(title = "Share of Wealth owned by Rich", y = "Wealth Ratio", x = "Time")

ggplot(inequality_metrics, aes(x = time, y = gini)) +
  geom_line(linewidth = 1.2) +
  labs(title = "Gini Coefficient Over Time", x = "Time Period", y = "Gini Coefficient")

# Wealth Density Histogram
ggplot(old, aes(x = wealth)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_density(color = "blue", linewidth = 1) +
  labs(
    title = "Density Histogram of Wealth",
    x = "Wealth",
    y = "Density"
  ) +
  theme_minimal()

# Total Consumption Density Histogram
ggplot(old, aes(x = total_cons)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightgreen", color = "black", alpha = 0.7) +
  geom_density(color = "darkgreen", linewidth = 1) +
  labs(
    title = "Density Histogram of Total Consumption",
    x = "Total Consumption",
    y = "Density"
  ) +
  theme_minimal()


#### Graph
library(ggplot2)

ggplot(wealth_history, aes(x = time, y = total_wealth, color = type)) +
  geom_line(linewidth = 1.2) +
  labs(
    title = "Wealth of Rich and Poor Over Time",
    x = "Time Period",
    y = "Total Wealth",
    color = "Class"
  )





# ---- Metrics from the last 100 time steps ----

last_100_inequality <- inequality_metrics %>% tail(100)
last_100_wealth <- wealth_history %>% tail(200)  # 2 rows per time step (rich + poor)

# Average Gini coefficient
avg_gini <- mean(last_100_inequality$gini, na.rm = TRUE)
avg_gini_consumption <- mean(last_100_inequality$gini_consumption, na.rm = TRUE)
# Average R (interest rate net of depreciation)
avg_r <- mean(last_100_wealth$r, na.rm = TRUE)

# Combine all old wealth from final 100 periods
final_wealth <- old$wealth
final_b_amt <- old$bequest_amount
mean_wealth <- mean(final_wealth, na.rm = TRUE)
mean_consumption <- mean(old$total_cons)
median_wealth <- median(final_wealth, na.rm = TRUE)
poverty_threshold <- 0.5 * median_wealth
poverty_rate <- mean(final_wealth < poverty_threshold, na.rm = TRUE)
avg_rich_wage <- mean(wage_history$rich_wage)
avg_poor_wage <- mean(wage_history$poor_wage)

# Savings rate: savings / wages
final_savings <- old$savings
final_wages <- old$wages
savings_rate <- mean(final_savings / final_wages, na.rm = TRUE)


summary_stats <- tibble(
  average_gini = avg_gini,
  average_gini_consumption = avg_gini_consumption,
  average_R = avg_r,
  avg_rich_wage = avg_rich_wage,
  avg_poor_wage = avg_poor_wage,
  mean_wealth = mean_wealth,
  mean_consumption = mean_consumption,
  median_wealth = median_wealth,
  poverty_rate = poverty_rate,
  avg_savings_rate = savings_rate
)

print(summary_stats)
