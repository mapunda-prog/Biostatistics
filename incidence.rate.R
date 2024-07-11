Sobj <- Surv(cleaned_data$time, cleaned_data$rebound_occurred)
cox_model <- coxph(Sobj ~ 1, data = cleaned_data)
summary(cox_model)


fit <- survfit((Surv(cleaned_data$time, cleaned_data$rebound_occurred) ~ 1))
plot(fit, fun = "cumhaz", xlab = "Time", ylab = "Cumulative Hazard")
summary(fit)
cumulative_hazard <- fit$cumhaz
times <- fit$time
summary(fit, times = c(365, 730, 1095, 1460))
plot(fit, fun = "cumhaz", xlab = "Time", ylab = "cumulative Hazard", main = "Cumulative Hazard Over Time")
final_cumulative_hazard <- max(cumulative_hazard)
print(paste("Avarage incidence rate:", final_cumulative_hazard))



cumhaz_data <- data.frame(
  time = fit$time,
  cumhaz = fit$cumhaz
)

ggplot(cumhaz_data, aes(x = time, y = cumhaz)) +
  geom_step() +
  labs(x = "Time (days)", y = "Cumulative Hazard",
       title = "Cumulative Hazard Over Time") +
  theme_minimal() +
  scale_x_continuous(breaks = c(0, 365, 730, 1095, 1460),
                     labels = c("0", "2019", "2020", "2021", "2022")) +
  geom_vline(xintercept = c(365, 730, 1095, 1460), linetype = "dashed", color = "gray")





year_ends <- c(365, 730, 1095, 1460)
year_cumhaz <- summary(fit, times = year_ends)$cumhaz

year_points <- data.frame(
  time = year_ends,
  cumhaz = year_cumhaz
)

ggplot(cumhaz_data, aes(x = time, y = cumhaz)) +
  geom_step() +
  geom_point(data = year_points, color = "red", size = 3) +
  geom_text(data = year_points, aes(label = round(cumhaz, 2)), 
            vjust = -1, hjust = 1) +
  labs(x = "Time (days)", y = "Cumulative Hazard",
       title = "Cumulative Hazard Over Time") +
  theme_minimal() +
  scale_x_continuous(breaks = c(0, 365, 730, 1095, 1460),
                     labels = c("0", "2019", "2020", "2021", "2022")) +
  geom_vline(xintercept = c(365, 730, 1095, 1460), linetype = "dashed", color = "gray")


# Define year endpoints
year_ends <- c(365, 730, 1095, 1460)

# Get summary at year endpoints
summary_years <- summary(fit, times = year_ends)

# Calculate incidence rates
incidence_data <- data.frame(
  Year = c("2019", "2020", "2021", "2022"),
  Start = c(0, 365, 730, 1095),
  End = year_ends,
  Events = diff(c(0, summary_years$n.event)),
  AtRisk = summary_years$n.risk
)

# Calculate person-years and incidence rates
incidence_data$PersonYears <- with(incidence_data, (End - Start) * AtRisk / 365)
incidence_data$IncidenceRate <- with(incidence_data, (Events / PersonYears) * 100)

print(incidence_data)


ggplot(incidence_data, aes(x = Year, y = IncidenceRate, group = 1)) +
  geom_line(color = "steelblue") +
  geom_point(color = "steelblue", size = 3) +
  geom_text(aes(label = sprintf("%.2f", IncidenceRate)), vjust = -1) +
  labs(x = "Year", y = "Incidence Rate (per 100 person-years)",
       title = "Incidence Rate of Viral Load Rebound by Year") +
  theme_minimal() +
  ylim(0, max(incidence_data$IncidenceRate) * 1.2)  # Adjust y-axis to fit labels



ggplot(incidence_data, aes(x = Year, y = IncidenceRate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = sprintf("%.2f", IncidenceRate)), vjust = -0.5) +
  labs(x = "Year", y = "Incidence Rate (per 100 person-years)",
       title = "Incidence Rate of Viral Load Rebound by Year") +
  theme_minimal() +
  ylim(0, max(incidence_data$IncidenceRate) * 1.2)  

# create incident rate functions ----

# Create a function to calculate incidence rate for a specific time interval
calc_incidence_rate <- function(cleaned_data, start_time, end_time) {
  # Subset data for the specific time interval
  interval_data <- cleaned_data %>%
    filter(time > start_time, time <= end_time | (time > start_time & event == 0))
  
  # Calculate total person-time (in years)
  total_time <- sum(pmin(interval_data$time, end_time) - start_time) / 365.25
  
  # Count events in this interval
  events <- sum(interval_data$event & interval_data$time <= end_time)
  
  # Calculate incidence rate
  rate <- (events / total_time) * 100
  
  return(c(events = events, person_years = total_time, rate = rate))
}

# Calculate incidence rates for each year
year_intervals <- data.frame(
  Year = c("2019", "2020", "2021", "2022"),
  Start = c(0, 365, 730, 1095),
  End = c(365, 730, 1095, 1460)
)

incidence_data <- year_intervals %>%
  rowwise() %>%
  mutate(
    results = list(calc_incidence_rate(data, Start, End)),
    Events = results[1],
    PersonYears = results[2],
    IncidenceRate = results[3]
  ) %>%
  select(-results)

print(incidence_data)






#trial 2 ----
calc_incidence_rate <- function(cleaned_data, start_time, end_time) {
  # Subset data for the specific time interval
  interval_data <- cleaned_data %>%
    dplyr::filter(time > start_time & (time <= end_time | event == 0))
  
  # Calculate total person-time (in years)
  total_time <- sum(pmin(interval_data$time, end_time) - start_time) / 365.25
  
  # Count events in this interval
  events <- sum(interval_data$event & interval_data$time <= end_time)
  
  # Calculate incidence rate
  rate <- (events / total_time) * 100
  
  return(data.frame(Events = events, PersonYears = total_time, IncidenceRate = rate))
}

# Example usage:
year_intervals <- data.frame(
  Year = c("2019", "2020", "2021", "2022"),
  Start = c(0, 365, 730, 1095),
  End = c(365, 730, 1095, 1460)
)

incidence_data <- year_intervals %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    results = list(calc_incidence_rate(data, Start, End)),
    Events = results$Events,
    PersonYears = results$PersonYears,
    IncidenceRate = results$IncidenceRate
  ) %>%
  dplyr::select(-results)

print(incidence_data)














