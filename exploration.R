library(tidyverse)

#Read in the data
logged_data <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vR_Kd10ljJWzdtGTuIJp6_cE9V271d4MVxbJK9-NosxzyArSoGiPJfBea1BaSf2CCBXKSSji8txlPOC/pub?gid=1024395860&single=true&output=csv")

#Renaming variables and create a new data frame

latest_data <- logged_data %>% 
  rename (timestamp = 1, 
          task_avoided = 2,
          procrastination_minutes = 3,
          activity_done = 4,
          stress_level = 5)


# First look at the data

glimpse(latest_data)

head(latest_data)

summary(latest_data)



# Number of observations
nrow(latest_data)

# Minimum procrastination time
min(latest_data$procrastination_minutes, na.rm = TRUE)

# Maximum procrastination time
max(latest_data$procrastination_minutes, na.rm = TRUE)

# Mean procrastination time
mean(latest_data$procrastination_minutes, na.rm = TRUE)

# Median procrastination time
median(latest_data$procrastination_minutes, na.rm = TRUE)

# Mean stress level
mean(latest_data$stress_level, na.rm = TRUE)

# Most common task avoided
latest_data %>%
  count(task_avoided, sort = TRUE)

# Most common activity done instead
latest_data %>%
  count(activity_done, sort = TRUE)





# Bar chart 1: tasks avoided
latest_data %>%
  ggplot(aes(x = task_avoided)) +
  geom_bar() +
  labs(
    title = "Number of procrastination episodes by task avoided",
    x = "Task avoided",
    y = "Count of episodes"
  ) +
  theme_minimal()

# Bar chart 2: activity done instead
latest_data %>%
  ggplot(aes(x = activity_done)) +
  geom_bar() +
  labs(
    title = "Activities done instead of the intended task",
    x = "Activity done instead",
    y = "Count of episodes"
  ) +
  theme_minimal()


# Bar chart 3: stress level
latest_data %>%
  ggplot(aes(x = factor(stress_level))) +
  geom_bar() +
  labs(
    title = "Stress level during procrastination episodes",
    x = "Stress level",
    y = "Count of episodes"
  ) +
  theme_minimal()




# Average procrastination time by task avoided
latest_data %>%
  group_by(task_avoided) %>%
  summarise(
    avg_minutes = mean(procrastination_minutes, na.rm = TRUE),
    count = n()
  )

# Average procrastination time by activity done instead
latest_data %>%
  group_by(activity_done) %>%
  summarise(
    avg_minutes = mean(procrastination_minutes, na.rm = TRUE),
    count = n()
  )


# FINAL CHOICES FOR REPORT
# Code below is the code I have decided to use
# for my dynamic report


# Summary value 1: mean procrastination time
mean(latest_data$procrastination_minutes, na.rm = TRUE)

# Summary value 2: maximum procrastination time
max(latest_data$procrastination_minutes, na.rm = TRUE)

# Bar chart 1: task avoided
latest_data %>%
  ggplot(aes(x = task_avoided)) +
  geom_bar() +
  labs(
    title = "Number of procrastination episodes by task avoided",
    x = "Task avoided",
    y = "Count of episodes"
  ) +
  theme_minimal()

# Bar chart 2: activity done instead
latest_data %>%
  ggplot(aes(x = activity_done)) +
  geom_bar() +
  labs(
    title = "Activities done instead of the intended task",
    x = "Activity done instead",
    y = "Count of episodes"
  ) +
  theme_minimal()


