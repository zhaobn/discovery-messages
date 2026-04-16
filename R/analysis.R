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



# distribution of msg_rank per condition
browse_data %>%
  ggplot(aes(x = condition, y = msg_rank)) +
  geom_violin(trim = FALSE) +
  geom_jitter(width = 0.1, alpha = 0.2)

# total times_read per sub_id per condition
browse_data %>%
  group_by(sub_id, condition) %>%
  summarise(total_times_read = sum(times_read), .groups = "drop") %>%
  ggplot(aes(x = condition, y = total_times_read)) +
  geom_violin(trim = FALSE) +
  geom_jitter(width = 0.1, alpha = 0.2)

# total number of messages per sub_id per condition
browse_data %>%
  group_by(sub_id, condition) %>%
  summarise(total_msgs = n(), .groups = "drop") %>%
  ggplot(aes(x = condition, y = total_msgs)) +
  geom_violin(trim = FALSE) +
  geom_jitter(width = 0.1, alpha = 0.2)


# aggregate to subject level
plot_data <- browse_data %>%
  group_by(sub_id, condition) %>%
  mutate(dwell_m = dwell_ms/60000) %>%
  summarise(
    total_read = sum(times_read),
    total_dwell = sum(dwell_m),
    total_points_log = first(log_total_points),
    .groups = "drop"
  )

plot_data %>%
  ggplot(aes(x = total_read, y = total_points_log)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm") +
  facet_wrap(~condition)
plot_data %>%
  ggplot(aes(x = total_dwell, y = total_points_log)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm") +
  facet_wrap(~condition)

# distribution of msg_rank per condition
notebook_data %>%
  ggplot(aes(x = condition, y = msg_rank)) +
  geom_violin(trim = FALSE) +
  geom_jitter(width = 0.1, alpha = 0.2)

# average msg_rank per sub_id + relationship with log_total_points
notebook_data %>%
  group_by(sub_id, condition) %>%
  summarise(
    avg_msg_rank = mean(msg_rank),
    log_total_points = first(log_total_points),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = avg_msg_rank, y = log_total_points)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") +
  facet_wrap(~condition)




# Check labeled data ----
msg_lab_pilot <- read.csv('../data/pilot/message_labeled_fixed.csv')
msg_lab_prev <- read.csv('../data/g0/message_sample_labeled_fixed.csv')

msg_lab_pilot <- msg_lab_pilot %>% mutate(id = 1000 + as.integer(id), batch = 'pilot')
msg_lab_prev <- msg_lab_prev %>% mutate(batch = 'prev')

msg_merged <- rbind(msg_lab_pilot, msg_lab_prev)


msg_merged$s_len <- nchar(msg_merged$sentence)
msg_merged$condition <- factor(
  msg_merged$condition,
  levels = cond_levels
)

msg_merged %>%
  filter(label == "rule") %>%
  mutate(batch = factor(batch, levels = c("prev", "pilot"))) %>%
  ggplot(aes(x = condition, y = s_len, fill = batch)) +
  stat_summary(fun = mean, geom = "bar", position = position_dodge()) +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               position = position_dodge(width = 0.9), width = 0.2)
  # geom_jitter(aes(color = batch),
  #             position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.9),
  #             alpha = 0.2)


msg_merged %>%
  mutate(batch = factor(batch, levels = c("prev", "pilot"))) %>%
  mutate(label = factor(label, levels = c("rule", "strategy", "tip", "other"))) %>%
  group_by(condition, label, batch) %>%
  summarise(total_s_len = sum(s_len), .groups = "drop") %>%
  group_by(condition) %>%
  mutate(prop = total_s_len / sum(total_s_len)) %>%
  ggplot(aes(x = condition, y = prop, fill = label)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(~batch)























