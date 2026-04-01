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
