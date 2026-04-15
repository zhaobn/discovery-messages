
library(dplyr)
library(tidyr)

sampled_msgs = read.csv('../data/g0/message_sample.csv') 


test_subject_data = read.csv('../data/prep_mix/subject_data_l3.csv') %>% select(-X)
main_subject_data = read.csv('../data/prep_mix/subject_data.csv') %>%  select(colnames(test_subject_data))
combined_subject_data = rbind(main_subject_data, test_subject_data)

sampled_subject_data = combined_subject_data %>%
  filter( id %in% sampled_msgs$id) %>%
  mutate(cond_name = ifelse(condition == "high", "easy",
                            ifelse(condition == "low-3", "hard",
                                   condition))) %>%
  mutate(condition = cond_name) %>%
  select(colnames(text_subject_data))
write.csv(sampled_subject_data, '../data/g0/subject_sampled.csv')


test_action_data = read.csv('../data/prep_mix/action_data_l3.csv') %>% select(-X)
main_action_data = read.csv('../data/prep_mix/action_data.csv') %>%  select(colnames(test_action_data))
combined_action_data = rbind(main_action_data, test_action_data)

sampled_action_data = combined_action_data %>%
  filter( id %in% sampled_msgs$id) %>%
  mutate(cond_name = ifelse(condition == "high", "easy",
                            ifelse(condition == "low-3", "hard",
                                   condition))) %>%
  mutate(condition = cond_name) %>%
  select(colnames(test_action_data))

sampled_action_data_appended = sampled_action_data %>%
  arrange(id, action_id) %>%
  group_by(id) %>%
  mutate(total_points = cumsum(points)) %>%
  ungroup()
write.csv(sampled_action_data_appended, '../data/g0/actions_sampled.csv')
