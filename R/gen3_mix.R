library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(gghalves)

library(effectsize)
options(scipen=999)


# Prep mix data ----
OUTPUT_DIR <- "../data/gen3_mix/" 

subject_1_data = read.csv('../data/pilot_3/subject.csv') %>%
  mutate(version = 'original')
subject_2_data = read.csv('../data/pilot_4/subject.csv') %>%
  filter(condition == 'medium') %>%
  mutate(version = 'corrected')
subject_data = rbind(subject_1_data, subject_2_data)
write_csv(subject_data, file.path(OUTPUT_DIR, 'subject.csv'))


action_1_data = read.csv('../data/pilot_3/actions.csv') %>%
  select(-X) %>%
  mutate(version = 'original')
action_2_data = read.csv('../data/pilot_4/actions.csv') %>%
  filter(condition == 'medium') %>%
  mutate(version = 'corrected') %>%
  select(colnames(action_1_data))
action_data = rbind(action_1_data, action_2_data)
write_csv(action_data, file.path(OUTPUT_DIR, 'actions.csv'))


# Analysis ----
cond_levels = c("easy", "medium", "hard")
base_colors <- c("#009E73","#56B4E9","#E69F00")

subject_data = read.csv('../data/gen3_mix/subject.csv')
subject_data$condition <- factor(subject_data$condition, levels = cond_levels)

action_data = read.csv('../data/gen3_mix/actions.csv')
action_data$condition <- factor(action_data$condition, levels = cond_levels)

# Original run
action_data_summary_original = action_data %>%
  mutate(total_points_log = log(total_points + 1)) %>%
  filter(version=='original') %>%
  group_by(sub_id, condition) %>%
  complete(step = 1:40) %>%
  fill(total_points_log, .direction = "down") %>%
  group_by(step, condition) %>%
  summarise(
    mean_total_points_log = mean(total_points_log, na.rm = TRUE),
    se_total_points_log = sd(total_points_log, na.rm = TRUE) / sqrt(n())
  )

action_data_summary_original %>%
  ggplot(aes(x = step, y = mean_total_points_log, color = condition)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean_total_points_log - se_total_points_log, 
                  ymax = mean_total_points_log + se_total_points_log, 
                  fill = condition), alpha = 0.2, color = NA) + 
  geom_point(aes(shape = condition), size = 2) + 
  scale_color_manual(values = base_colors) +
  scale_fill_manual(values = base_colors) +
  theme_minimal() +   
  labs(x = "Action ID", y = "Mean Log of Total Points", title = "Cumulative Rewards over Actions (Original)") + 
  theme(legend.position = "bottom")

# Corrected run
action_data_summary_oringal = action_data %>%
  mutate(total_points_log = log(total_points + 1)) %>%
  filter(version=='corrected' | condition %in% c('easy', 'hard')) %>%
  group_by(sub_id, condition) %>%
  complete(step = 1:40) %>%
  fill(total_points_log, .direction = "down") %>%
  group_by(step, condition) %>%
  summarise(
    mean_total_points_log = mean(total_points_log, na.rm = TRUE),
    se_total_points_log = sd(total_points_log, na.rm = TRUE) / sqrt(n())
  )

action_data_summary_oringal %>%
  ggplot(aes(x = step, y = mean_total_points_log, color = condition)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean_total_points_log - se_total_points_log, 
                  ymax = mean_total_points_log + se_total_points_log, 
                  fill = condition), alpha = 0.2, color = NA) + 
  geom_point(aes(shape = condition), size = 2) + 
  scale_color_manual(values = base_colors) +
  scale_fill_manual(values = base_colors) +
  theme_minimal() +   
  labs(x = "Action ID", y = "Mean Log of Total Points", title = "Cumulative Rewards over Actions (Corrected)") + 
  theme(legend.position = "bottom")



# Compare with prev. generation ----
shade_colors <- c(
  "easy_gen_0"     = "#89D4BC",  # light green
  "easy_gen_1"     = "#009E73",  # base green
  "easy_gen_2"     = "#005C43",  # dark green
  
  "medium_gen_0"   = "#A8D8F0",  # light blue
  "medium_gen_1"   = "#56B4E9",  # base blue
  "medium_gen_2"   = "#1A6A99",  # dark blue
  
  "hard_gen_0"     = "#F5CF8E",  # light orange
  "hard_gen_1"     = "#E69F00",  # base orange
  "hard_gen_2"     = "#8A5F00"   # dark orange
)

action_gen_0 = read.csv('../data/g0/actions_sampled.csv') %>% select(-c(X, token)) %>%
  rename(step=action_id) %>%
  mutate(gen='gen_0')

action_gen_1 = read.csv('../data/pilot_2/actions.csv') %>% select(-X) %>%
  rename(id=sub_id) %>%
  mutate(gen='gen_1', id = id+1000) %>%
  select(colnames(action_gen_0))

combined_action_data_corrected = action_data %>%
  filter(version=='corrected' | condition %in% c('easy', 'hard')) %>%
  rename(id=sub_id) %>%
  mutate(gen='gen_2', id = id+2000) %>%
  select(colnames(action_gen_0)) %>%
  rbind(action_gen_1) %>%
  rbind(action_gen_0)
combined_action_data_corrected$condition <- factor(combined_action_data_corrected$condition, levels = cond_levels)

combined_data_summary_corrected = combined_action_data_corrected %>%
  mutate(total_points_log = log(total_points+1)) %>%
  group_by(id, condition, gen) %>%
  complete(step = 1:40) %>%
  fill(total_points_log, .direction = "down") %>%
  group_by(step, condition, gen) %>%
  summarise(
    mean_total_points_log = mean(total_points_log, na.rm = TRUE),
    se_total_points_log = sd(total_points_log, na.rm = TRUE) / sqrt(n())
  ) %>%
  ungroup()

combined_data_summary_corrected %>%
  ggplot(aes(x = step, y = mean_total_points_log, color = condition)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean_total_points_log - se_total_points_log, 
                  ymax = mean_total_points_log + se_total_points_log, 
                  fill = condition), alpha = 0.2, color = NA) + 
  scale_color_manual(values = base_colors) +
  scale_fill_manual(values = base_colors) +
  theme_minimal() +   
  labs(x = "Action ID", y = "Mean Log of Total Points") + 
  theme(legend.position = "bottom") +
  facet_grid(~gen)

combined_data_summary_corrected %>%
  mutate(cond_gen = paste(condition, gen, sep = "_")) %>%
  mutate(cond_gen = factor(paste(condition, gen, sep = "_"), 
                           levels = c("easy_gen_0","easy_gen_1","easy_gen_2",
                                      "medium_gen_0","medium_gen_1","medium_gen_2",
                                      "hard_gen_0","hard_gen_1","hard_gen_2"))) %>%
  ggplot(aes(x = step, y = mean_total_points_log, color = cond_gen, group = cond_gen)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean_total_points_log - se_total_points_log,
                  ymax = mean_total_points_log + se_total_points_log,
                  fill = cond_gen), alpha = 0.2, color = NA) +
  scale_color_manual(values = shade_colors) +
  scale_fill_manual(values = shade_colors) +
  theme_minimal() +
  labs(x = "Action ID", y = "Mean Log of Total Points") +
  facet_grid(condition ~ .)


# Highest levels ----
combined_level_data = combined_action_data_corrected %>%
  select(id, step, action, held, target, yield, condition, gen) %>%
  mutate(level = ifelse(
    nchar(yield) > 1 & !is.na(yield),
    as.numeric(sub(".*_(\\d+)$", "\\1", yield)),
    0
  ))
combined_level_data = combined_level_data %>%
  group_by(id, condition, gen) %>%
  arrange(step) %>%
  mutate(highest_level = cummax(level)) %>%
  complete(step = 1:40) %>%
  fill(highest_level, .direction = "down") %>%
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
  scale_color_manual(values = base_colors) +
  scale_fill_manual(values = base_colors) +
  theme_minimal() +   
  labs(x = "Action ID", y = "Mean Highest Levels") + 
  theme(legend.position = "bottom") +
  facet_grid(~gen)


level_data_summary %>% 
  mutate(cond_gen = paste(condition, gen, sep = "_")) %>%
  mutate(cond_gen = factor(paste(condition, gen, sep = "_"), 
                           levels = c("easy_gen_0","easy_gen_1","easy_gen_2",
                                      "medium_gen_0","medium_gen_1","medium_gen_2",
                                      "hard_gen_0","hard_gen_1","hard_gen_2"))) %>%
  ggplot(aes(x = step, y = mean_highest_level, color = cond_gen, group = cond_gen)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean_highest_level - se_highest_level, 
                  ymax = mean_highest_level + se_highest_level, 
                  fill = cond_gen), alpha = 0.2, color = NA) + 
  scale_color_manual(values = shade_colors) +
  scale_fill_manual(values = shade_colors) + 
  theme_minimal() +   
  labs(x = "Action ID", y = "Mean Highest Levels") + 
  facet_grid(condition~.)



# Check labeled data ----
msg_lab_g0 <- read.csv('../data/g0/message_sample_labeled_fixed.csv') %>%
  mutate(gen = 0, version = 'default')
msg_lab_g1 <- read.csv('../data/pilot_2/messages_labeled.csv') %>%
  mutate(id = 1000 + as.integer(id), gen = 1, version = 'default') %>%
  select(colnames(msg_lab_g0))

msg_lab <- read.csv('../data/gen3_mix/messages_labeled.csv') %>%
  mutate(id = 2000 + as.integer(id), gen = 2) %>%
  filter(version == 'corrected' | condition %in% c('easy', 'hard')) %>%
  select(colnames(msg_lab_g0)) 

msg_merged <- rbind(msg_lab_g0, msg_lab_g1, msg_lab) %>%
  mutate(
    source_col = recode(
      source_col,
      "messageHow" = "message_how",
      "messageRules" = "message_rules"
    )
  )

msg_merged$s_len <- nchar(msg_merged$sentence)
msg_merged$condition <- factor(
  msg_merged$condition,
  levels = cond_levels
)

# message lengths
msg_merged %>%
  filter(source_col != 'summary') %>% 
  filter(label == "rule") %>%
  mutate(gen = factor(gen, levels = c(0, 1, 2))) %>%
  group_by(id, condition, gen) %>%
  summarise(mean_s_len = mean(s_len), .groups = "drop") %>%
  ggplot(aes(x = condition, y = mean_s_len, group = gen, fill=gen)) +
  stat_summary(fun = mean, geom = "bar", position = position_dodge()) +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               position = position_dodge(width = 0.9), width = 0.2) +
  labs(x = NULL, y = "average rule length", title = 'Message length (raw)') +
  scale_fill_manual(values = c("0" = "#9ECAE1", "1" = "#3182BD", "2" = "#08519C")) +
  theme(legend.position = 'bottom') 

# message lengths - normalized by easy condition
msg_merged %>%
  filter(source_col != 'summary') %>%
  filter(label == "rule") %>%
  mutate(gen = factor(gen, levels = c(0, 1, 2))) %>%
  group_by(id, condition, gen) %>%
  summarise(mean_s_len = mean(s_len), .groups = "drop") %>%
  group_by(gen) %>%
  mutate(easy_mean = mean(mean_s_len[condition == "easy"]),
         relative_len = mean_s_len / easy_mean) %>%
  ggplot(aes(x = condition, y = relative_len, group = gen, fill = gen)) +
  stat_summary(fun = mean, geom = "bar", position = position_dodge()) +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               position = position_dodge(width = 0.9), width = 0.2) +
  labs(x = NULL, y = "relative rule length (easy = 1)", title = 'Message length (normalized)') +
  scale_fill_manual(values = c("0" = "#9ECAE1", "1" = "#3182BD", "2" = "#08519C")) +
  theme(legend.position = 'bottom')

# message types per generation
msg_merged %>%
  filter(source_col != 'summary') %>% 
  mutate(gen = factor(gen, levels = c(0, 1, 2))) %>%
  mutate(label = factor(label, levels = c("rule", "strategy", "tip", "other"))) %>%
  group_by(condition, label, gen) %>%
  summarise(total_s_len = sum(s_len), .groups = "drop") %>%
  group_by(condition) %>%
  mutate(prop = total_s_len / sum(total_s_len)) %>%
  ggplot(aes(x = condition, y = prop, fill = label)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(~gen) +
  theme(legend.position = 'bottom') +
  scale_fill_manual(values = c(
    "rule"     = "#264653",
    "strategy" = "#2A9D8F",
    "tip"      = "#E9C46A",
    "other"    = "#E76F51"
  ))

# message types per condition
msg_merged %>%
  filter(source_col != 'summary') %>% 
  mutate(gen = factor(gen, levels = c(0, 1, 2))) %>%
  mutate(label = factor(label, levels = c("rule", "strategy", "tip", "other"))) %>%
  group_by(condition, label, gen) %>%
  summarise(total_s_len = sum(s_len), .groups = "drop") %>%
  group_by(condition) %>%
  mutate(prop = total_s_len / sum(total_s_len)) %>%
  ggplot(aes(x = gen, y = prop, fill = label)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(~condition) +
  theme(legend.position = 'bottom') +
  scale_fill_manual(values = c(
    "rule"     = "#264653",
    "strategy" = "#2A9D8F",
    "tip"      = "#E9C46A",
    "other"    = "#E76F51"
  ))

# summary note types
msg_merged %>%
  filter(source_col == "summary") %>%
  ggplot(aes(x = condition, fill = label)) +
  geom_bar(position = "fill") +
  facet_wrap(~gen) +
  scale_fill_manual(values = c(
    "rule"     = "#264653",
    "strategy" = "#2A9D8F",
    "tip"      = "#E9C46A",
    "other"    = "#E76F51"
  )) +
  labs(title = 'Summary notes') +
  theme(legend.position = 'none')


# Check browse data ----





























