data <-import("cleaned_data.csv")

breaks <- c(0, 10, Inf)  # Define the breaks for your categories
labels <- c("0-10 km", "More than 10 km")  # Define the labels for your categories

data$HF_distance_cat2 <- cut(data$HF_distance, breaks = breaks, 
                                         labels = labels, include.lowest = TRUE, right = FALSE)


breaks2 <- c(0, 1, 2, Inf)  # Define the breaks for your categories
labels2 <- c("less than or equal to 1 test", "2 test", "More than 2 test")  # Define the labels for your categories

data$VL_test_freq <- cut(data$frequency_viral_load_test, breaks = breaks2, 
                             labels = labels2, include.lowest = TRUE, right = FALSE)

data$interuption_cat <- as.factor(recode(data$interruption_art_treatment, 
                                         "Attending this clinic" = "client without interuption",
                                         .default = "Client with interruption", .missing = "0"))

breaks3 <- c(-Inf, 12, 48, Inf)  # Define the breaks for your categories
labels3 <- c("less than or equal to 1 year", "2-4 years", "5 years or more")  # Define the labels for your categories

data$duration_art_cat <- cut(data$duration_on_art_months, breaks = breaks3, 
                         labels = labels3, include.lowest = TRUE, right = FALSE)



data <- data %>% mutate(duration_on_art_months = cut(duration_on_art_months,
                                                                     breaks = c(-Inf, 12, 48, Inf),
                                                                     labels = c("less than  a year", "1-4 years", "5 years or More"),
                                                                     right = FALSE))

data <- data %>%
  mutate(months_to_rebound = time / 30)


export(data, "cleaned_after_comment.csv")


