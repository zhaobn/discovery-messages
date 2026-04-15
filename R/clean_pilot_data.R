
# Load libraries
library(jsonlite)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(stringr)

# Global setting
options(scipen = 999)
INPUT_FILE <- "../data/pilot/raw_1.tsv" 
OUTPUT_DIR <- "../data/pilot/" 


# Read raw data
lines <- read_lines(INPUT_FILE)
raw <- read_delim(I(paste(lines, collapse = "\n")), delim = "\t", quote = "", 
                  col_types = cols(.default = col_character()))
raw <- raw %>%
  rename_with(~ gsub('^"|"$', '', .x)) |>
  mutate(across(everything(), ~ gsub('^"|"$', '', .x)))

# Filter rows
clean <- raw %>% 
  mutate(id = as.numeric(as.character(id))) %>%
  filter(id > 4)

# Ad-hoc fixes
x <- clean$subject[13]
x_fixed <- gsub('matter\\.\\\\\\\\\\\\\"\\\\\"', 'matter.\\\\\"', x, fixed = FALSE)
clean$subject[13] <- x_fixed

# Helper functions to parse json
fix_json <- function(s) {
  #gsub("\\", '"', s, fixed = TRUE)
  gsub('^"|"$', '', s, fixed = TRUE)
  gsub('\\\\\"', '"', s)
}
parse_json_col <- function(s) {
  tryCatch(fromJSON(fix_json(s)), error = function(e) NULL)
}



# Parses raw data and writes five clean CSVs:
#   subject.csv    – one row per participant, demographics + timing
#   events.csv     – one row per raw game event (all movement + actions)
#   actions.csv    – one row per combine/consume action, with trial_n
#   msgs_read.csv  – one row per message the participant opened
#   notebook.csv   – one row per note the participant saved


# Subject data - one row per participant ----
subject <- clean %>%
  mutate(parsed = map(subject, parse_json_col)) %>%
  mutate(
    prolific_id               = map_chr(parsed, \(x) x[["prolific_id"]]               %||% NA_character_),
    token                     = map_chr(parsed, \(x) x[["token"]]                     %||% NA_character_),
    condition                 = map_chr(parsed, \(x) x[["condition"]]                 %||% NA_character_),
    age                       = map_chr(parsed, \(x) x[["age"]]                       %||% NA_character_),
    sex                       = map_chr(parsed, \(x) x[["sex"]]                       %||% NA_character_),
    engagement                = map_chr(parsed, \(x) as.character(x[["engagement"]]   %||% NA)),
    difficulty                = map_chr(parsed, \(x) as.character(x[["difficulty"]]   %||% NA)),
    feedback                  = map_chr(parsed, \(x) x[["feedback"]]                  %||% NA_character_),
    summary                   = map_chr(parsed, \(x) x[["summary"]]                   %||% NA_character_),
    message_how               = map_chr(parsed, \(x) x[["messageHow"]]                %||% NA_character_),
    start_time                = map_chr(parsed, \(x) x[["start_time"]]                %||% NA_character_),
    allow_regeneration        = map_lgl(parsed, \(x) isTRUE(x[["allow_regeneration"]])),
    total_points              = map_int(parsed, \(x) as.integer(x[["total_points"]]   %||% NA)),
    # instruction_duration_ms   = map_int(parsed, \(x) as.integer(x[["instruction_duration"]]      %||% NA)),
    # browse_duration_ms        = map_int(parsed, \(x) as.integer(x[["messages_browse_duration"]]  %||% NA)),
    reflect_duration_ms       = map_int(parsed, \(x) as.integer(x[["messages_reflect_duration"]] %||% NA)),
    task_duration_ms          = map_int(parsed, \(x) as.integer(x[["task_duration"]]             %||% NA)),
    composition_duration_ms   = map_int(parsed, \(x) as.integer(x[["composition_duration"]]      %||% NA))
  ) %>%
  select(
    sub_id = id, worker, assignment, created_at, token, condition,
    age, sex, engagement, difficulty, feedback,
    start_time, allow_regeneration, total_points,
    summary, message_how,
    # instruction_duration_ms, browse_duration_ms, 
    reflect_duration_ms,
    task_duration_ms, composition_duration_ms
  )
# ----


# Action data - one row per combine/consume actions ----
actions <- clean %>%
  mutate(sub_id = id, parsed_actions = map(actions, parse_json_col)) %>%
  select(sub_id, parsed_actions) %>%
  unnest_longer(parsed_actions, indices_to = "act_key") %>%
  unnest_wider(parsed_actions) %>%
  group_by(sub_id) %>%
  mutate(
    total_points = cumsum(points)
  ) %>%
  ungroup() %>%
  mutate(
    act_id = as.integer(str_extract(act_key, "\\d+")),
    
    # held item
    held_shape   = if_else(is.na(held), "", str_extract(held, "^[^_]+")),
    held_texture = if_else(is.na(held), "", str_extract(held, "(?<=_)[^_]+(?=_\\d+$)")),
    held_level   = if_else(is.na(held),  0L, as.integer(str_extract(held, "\\d+$"))),
    
    # target item
    target_shape   = if_else(is.na(target) | target == "", "", str_extract(target, "^[^_]+")),
    target_texture = if_else(is.na(target) | target == "", "", str_extract(target, "(?<=_)[^_]+(?=_\\d+$)")),
    target_level   = if_else(is.na(target) | target == "", 0L, as.integer(str_extract(target, "\\d+$"))),
    
    # yield item
    yield_shape   = if_else(is.na(yield) | yield == "", "", str_extract(yield, "^[^_]+")),
    yield_texture = if_else(is.na(yield) | yield == "", "", str_extract(yield, "(?<=_)[^_]+(?=_\\d+$)")),
    yield_level   = if_else(is.na(yield) | yield == "", 0L, as.integer(str_extract(yield, "\\d+$"))),
    
    # log points (total_points = 0 at the start, so guard against log(0))
    log_total_points = if_else(total_points > 0, log(total_points), 0)
  ) %>%
  group_by(sub_id) %>%
  mutate(
    # highest yield level seen so far within this participant's session
    max_level = cummax(yield_level)
  ) %>%
  ungroup() %>%
  select(
    sub_id, act_id,
    action, held, target, yield, max_level,
    points, total_points, log_total_points,
    held_shape,   held_texture,   held_level,
    target_shape, target_texture, target_level,
    yield_shape,  yield_texture,  yield_level
  ) %>%
  arrange(sub_id, act_id)
# ---


# Notebook data - one row per note the participant saved (from notes > notebook) ----
notebook <- clean %>%
  mutate(sub_id = id, parsed_notes  = map(notes, parse_json_col)) %>%
  mutate(nb = map(parsed_notes, \(x) x[["notebook"]])) %>%
  select(sub_id, nb) %>%
  unnest(nb) %>%
  rename(
    msg_from  = from,
    note_text = text,
    msg_id    = sampleId,
    saved_at  = savedAt
  ) %>%
  mutate(saved_at = as.POSIXct(saved_at, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")) %>%
  select(sub_id, msg_id, msg_from, note_text, saved_at) |>
  arrange(sub_id, saved_at)
# ----


# Event data - one row per raw game event (moves, pickUp, combine, consume) ----
events <- clean %>%
  mutate(sub_id = id, parsed_events = map(events, parse_json_col)) %>%
  select(sub_id, parsed_events) %>%
  unnest_longer(parsed_events, indices_to = "event_key") %>%
  unnest_wider(parsed_events) %>%
  mutate(
    event_id  = as.integer(str_extract(event_key, "\\d+")),
    timestamp = as.POSIXct(timestamp, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
  ) %>%
  rename(
    event_action       = action,
    pos_x              = x,
    pos_y              = y,
    actions_left       = actionsLeft,
    current_points     = currentPoints,
    currently_carrying = currentlyCarrying
  ) %>%
  select(
    sub_id, event_id, timestamp,
    event_action, pos_x, pos_y,
    actions_left, current_points, currently_carrying, token
  ) %>%
  arrange(sub_id, event_id)
# ----


# msgs_read - one row per message the participant opened (from notes > msgsRead) ----
msgs_read <- clean %>%
  mutate(sub_id = id, parsed_notes  = map(notes, parse_json_col)) %>%
  mutate(msgs = map(parsed_notes, \(x) x[["msgsRead"]])) %>%
  select(sub_id, msgs) %>%
  unnest(msgs) %>%
  rename(
    msg_event_id = msgEventId,
    sample_id    = sampleId,
    dwell_ms     = dwellMs,
    opened_at    = openedAt,
    closed_at    = closedAt
  ) %>%
  mutate(
    mevt_id   = as.integer(str_extract(msg_event_id, "\\d+$")), # message event counter
    msg_id    = as.integer(sample_id),
    msg_rank  = as.integer(rank),
    opened_at = as.POSIXct(opened_at, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"),
    closed_at = as.POSIXct(closed_at, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
  ) %>%
  arrange(sub_id, opened_at) %>%
  group_by(sub_id, msg_id) %>%
  mutate(
    read_n        = row_number(),       # which time this participant opened this message
    times_read    = n()                 # total times this participant opened this message
  ) %>%
  ungroup() %>%
  select(
    sub_id, mevt_id, msg_id, msg_rank,
    read_n, times_read, dwell_ms, opened_at, closed_at, raw_msg_event_id = msg_event_id
  ) %>%
  arrange(sub_id, opened_at)
# ----


# Write CSVs 
write_csv(subject,   file.path(OUTPUT_DIR, "subject.csv"))
write_csv(events,    file.path(OUTPUT_DIR, "events.csv"))
write_csv(actions,   file.path(OUTPUT_DIR, "actions.csv"))
write_csv(msgs_read, file.path(OUTPUT_DIR, "msgs_read.csv"))
write_csv(notebook,  file.path(OUTPUT_DIR, "notebook.csv"))


# Prep for data sharing - remove prolific IDs
subject_data = read.csv('../data/pilot/subject.csv')
bonus_data = subject_data %>%
  mutate(bonus = log(total_points+1)/10) %>%
  select(worker, bonus)
write.csv(bonus_data, file = '../data/pilot/bonus.csv')

subject_cleaned = subject_data %>%
  select(id = sub_id, 
         condition, 
         messageHow = message_how, messageRules = summary, total_points,
         age, sex, engagement, difficulty, feedback, 
         task_duration_ms, reflect_duration_ms, composition_duration_ms)
write.csv(subject_cleaned, file = '../data/pilot/subject_cleaned.csv')

# append condition to action data
id_cond = subject_cleaned %>% select(id, condition)
action_data = read.csv('../data/pilot/actions.csv') 
action_data_cleaned = action_data %>%
  rename(id=sub_id, step=act_id) %>%
  filter(step <= 40) %>% 
  left_join(id_cond, by='id')
write.csv(action_data_cleaned, file = '../data/pilot/actions_cleaned.csv')


# get cleaned message data
msg_g0 = read.csv('../data/g0/message_data.csv')
message_data = subject_data %>%
  select(id=sub_id, condition, messageHow = message_how, messageRules = summary, total_points)
write.csv(message_data, file = '../data/pilot/message_data.csv')






