library(dplyr)
library(tidyr)
library(ggplot2)
library(gghalves)

library(effectsize)
options(scipen=999)


# Demographics ----

subject_data = read.csv('../data/pilot/subject_cleaned.csv')

reportStats <- function(vec, digits=2) {
  return(paste0(round(mean(vec, na.rm = TRUE), digits), '\\pm', 
                round(sd(vec, na.rm = TRUE), digits)))
}

reportStats(subject_data$age)
reportStats(subject_data$sex=='female')
reportStats(subject_data$reflect_duration_ms/60000)
reportStats(subject_data$task_duration_ms/60000)
reportStats(subject_data$composition_duration_ms/60000)




# Points per condition ----
subject_data$total_points_log = log(subject_data$total_points)

cond_levels = c("easy", "medium", "hard")
subject_data$condition <- factor(
  subject_data$condition,
  levels = cond_levels
)

ggplot(subject_data, aes(x = condition, y = total_points_log, fill = condition)) +
  #geom_half_violin(side = "l", width = 0.8, trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.5, alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.5, size = 2) +
  theme_minimal() +
  labs(title = "Total Points per Condition",
       x = "Condition", y = "Total Points (log)") +
  theme(legend.position = "none")


# Points per action ----
action_data = read.csv('../data/pilot/actions_cleaned.csv') %>% select(-X)
action_data$condition <- factor(action_data$condition, levels = cond_levels)

action_data_summary = action_data %>%
  mutate(total_points_log = log(total_points+1)) %>%
  group_by(step, condition) %>%
  summarise(
    mean_total_points_log = mean(total_points_log, na.rm = TRUE),
    se_total_points_log = sd(total_points_log, na.rm = TRUE) / sqrt(n())
  )

action_data_summary %>%
  ggplot(aes(x = step, y = mean_total_points_log, color = condition)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean_total_points_log - se_total_points_log, 
                  ymax = mean_total_points_log + se_total_points_log, 
                  fill = condition), alpha = 0.2, color = NA) + 
  #geom_point(aes(shape = condition), size = 3) + 
  theme_minimal() +   
  labs(x = "Action ID", y = "Mean Log of Total Points") + 
  theme(legend.position = "bottom")


# Compare with prev. generation ----

prev_action_data = read.csv('../data/g0/actions_sampled.csv') %>% select(-c(X, token)) %>%
  rename(step=action_id) %>%
  mutate(gen='gen_0')

combined_action_data = action_data %>%
  mutate(gen='gen_1', id = id+1000) %>%
  select(colnames(prev_action_data)) %>%
  rbind(prev_action_data)
combined_action_data$condition <- factor(combined_action_data$condition, levels = cond_levels)

combined_data_summary = combined_action_data %>%
  mutate(total_points_log = log(total_points+1)) %>%
  group_by(step, condition, gen) %>%
  summarise(
    mean_total_points_log = mean(total_points_log, na.rm = TRUE),
    se_total_points_log = sd(total_points_log, na.rm = TRUE) / sqrt(n())
  )
combined_data_summary %>%
  ggplot(aes(x = step, y = mean_total_points_log, color = condition)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean_total_points_log - se_total_points_log, 
                  ymax = mean_total_points_log + se_total_points_log, 
                  fill = condition), alpha = 0.2, color = NA) + 
  theme_minimal() +   
  labs(x = "Action ID", y = "Mean Log of Total Points") + 
  theme(legend.position = "bottom") +
  facet_grid(~gen)

combined_data_summary %>%
  ggplot(aes(x = step, y = mean_total_points_log, color = gen)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean_total_points_log - se_total_points_log, 
                  ymax = mean_total_points_log + se_total_points_log, 
                  fill = condition), alpha = 0.2, color = NA) + 
  theme_minimal() +   
  labs(x = "Action ID", y = "Mean Log of Total Points") + 
  theme(legend.position = "bottom") +
  facet_grid(~condition)




# Highest levels ----
combined_level_data = combined_action_data %>%
  select(id, step, action, held, target, yield, condition, gen) %>%
  mutate(level = ifelse(
    nchar(yield) > 1 & !is.na(yield),
    as.numeric(sub(".*_(\\d+)$", "\\1", yield)),
    0
  ))
combined_level_data <- combined_level_data %>%
  group_by(id) %>%
  arrange(step) %>%
  mutate(highest_level = cummax(level)) %>%
  ungroup()

level_data_summary = combined_level_data %>%
  group_by(step, condition, gen) %>%
  summarise(
    mean_highest_level = mean(highest_level, na.rm = TRUE),
    se_highest_level = sd(highest_level, na.rm = TRUE) / sqrt(n())
  )
ggplot(level_data_summary, aes(x = step, y = mean_highest_level, color = condition)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean_highest_level - se_highest_level, 
                  ymax = mean_highest_level + se_highest_level, 
                  fill = condition), alpha = 0.2, color = NA) + 
  geom_point(aes(shape = condition), size = 3) + 
  theme_minimal() +   
  labs(x = "Action ID", y = "Mean Highest Levels") + 
  theme(legend.position = "bottom") +
  facet_grid(~gen)


ggplot(level_data_summary, aes(x = step, y = mean_highest_level, color = gen)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean_highest_level - se_highest_level, 
                  ymax = mean_highest_level + se_highest_level, 
                  fill = condition), alpha = 0.2, color = NA) + 
  geom_point(aes(shape = condition), size = 3) + 
  theme_minimal() +   
  labs(x = "Action ID", y = "Mean Highest Levels") + 
  theme(legend.position = "bottom") +
  facet_grid(~condition)






