# %%
import pandas as pd
import numpy as np
import json
import os


# %%
# Paths 
DATA_PATH   = "../data/g0/message_data.csv"
PREVIEW_PATH = "../data/g0/message_sample.csv"
OUTPUT_PATH  = "../data/g0/messages.js"

# Config 
SAMPLE_N = 10
RANDOM_SEED = 42

# Clean up conditions, { original_name: new_name }
CONDITION_MAP = {
    "easy":     "easy",
    "medium-1": "medium",
    "hard-3":   "hard",
}

# %%
df_raw = pd.read_csv(DATA_PATH)
df = df_raw[df_raw["condition"].isin(CONDITION_MAP.keys())].copy()
df["condition"] = df["condition"].map(CONDITION_MAP)

df["nchar_how"]   = df["messageHow"].astype(str).str.len()
df["nchar_rules"] = df["messageRules"].astype(str).str.len()
df["log_points"] = np.log1p(df["total_points"])

sampled = (
    df.groupby("condition", group_keys=False)
      .apply(lambda g: g.sample(
          n=min(SAMPLE_N, len(g)),
          weights=g["log_points"].clip(lower=0) + 1e-6 if len(g) > SAMPLE_N else None,
          random_state=RANDOM_SEED
      ))
      .reset_index(drop=True)
)
sampled = sampled.merge(df[["id", "condition"]], on="id", how="left")

# %%
# Preview as CSV
os.makedirs(os.path.dirname(PREVIEW_PATH), exist_ok=True)
sampled.to_csv(PREVIEW_PATH, index=False)

# %%
# Build JSON object and save as messages.js

# Sort by log_points
sampled = sampled.sort_values(["condition", "log_points"], ascending=[True, False])
sampled["sample_id"] = range(1, len(sampled) + 1)

# Structure: { condition: [ { sample_id, id, messageHow, messageRules, nchar_how, nchar_rules, total_points }, ... ] }
output = {}
for condition, group in sampled.groupby("condition"):
    output[condition] = group[[
        "sample_id", "id", "messageHow", "messageRules", "nchar_how", "nchar_rules", "total_points"
    ]].to_dict(orient="records")

js_content = f"const messages = {json.dumps(output, indent=2)};\n"
os.makedirs(os.path.dirname(OUTPUT_PATH), exist_ok=True)
with open(OUTPUT_PATH, "w") as f:
    f.write(js_content)

# %%
# Prep second pilot
# Paths 
PILOT_PATH   = "../data/pilot_2/subject.csv"
PREVIEW_PATH = "../data/g0/message_sample.csv"
OUTPUT_PATH  = "../data/pilot_2/messages_p1.js"

SAMPLE_N = 10
RANDOM_SEED = 42

# Load data
df_pilot = pd.read_csv(PILOT_PATH)
df_prev  = pd.read_csv(PREVIEW_PATH)

df_pilot["id"] = df_pilot["sub_id"] + 1000
df_pilot["total_points_log"] = np.log1p(df_pilot["total_points"]+1)
df_prev["total_points_log"]  = np.log1p(df_prev["total_points"])

cols = ["id", "condition", "messageHow", "messageRules", "total_points"]
df_pilot.rename(columns={"message_how": "messageHow", "message_rules": "messageRules"}, inplace=True)

result = []

for cond in df_pilot["condition"].unique():
    pilot_subset = df_pilot[df_pilot["condition"] == cond]
    prev_subset  = df_prev[df_prev["condition"] == cond]

    n_pilot = len(pilot_subset)

    # 1. Take from pilot
    if n_pilot <= SAMPLE_N:
        pilot_sample = pilot_subset
    else:
        # use top k because sample size is too small for weighted sampling to be effective
        pilot_sample = pilot_subset.nlargest(n=SAMPLE_N, columns="total_points")
    
    # 2. Fill remainder from prev
    n_needed = SAMPLE_N - len(pilot_sample)

    if n_needed > 0:
        # use top k because sample size is too small for weighted sampling to be effective
        prev_sample = prev_subset.nlargest(n=n_needed, columns="total_points_log")
        # prev_sample = prev_subset.sample(
        #     n=n_needed,
        #     weights=prev_subset["total_points_log"],
        #     random_state=RANDOM_SEED
        # )
        combined = pd.concat([pilot_sample, prev_sample], ignore_index=True)
    else:
        combined = pilot_sample

    result.append(combined)

# Final dataframe
df_final = pd.concat(result, ignore_index=True)[cols]

df_final["rank"] = df_final.groupby("condition")["total_points"].rank(method="first", ascending=False)
df_final = df_final.sort_values(["condition", "rank"], ascending=[True, True])
df_final["sample_id"] = range(1, len(df_final) + 1)

df_final[["sample_id", "id", "messageHow", "messageRules", "total_points"]].to_csv('../data/pilot_2/message.csv', index=False)


# %%
# Save (JS format)
messages = {}
for cond, subdf in df_final.groupby("condition"):
    messages[cond] = subdf[
        ["sample_id", "id", "messageHow", "messageRules", "total_points"]
        ].to_dict(orient="records")

 # Save (JS format)
with open(OUTPUT_PATH, "w") as f:
    import json
    f.write("const messages = ")
    json.dump(messages, f, indent=2)
    f.write(";")
# %%
