# %%
import pandas as pd
import re

from dotenv import load_dotenv
from openai import OpenAI

# %%
# Paths 
# DATA_PATH   = "../data/pilot/message_data.csv"
# OUTPUT_PATH  = "../data/pilot/message_labeled.csv"

DATA_PATH   = "../data/g0/message_sample.csv"
OUTPUT_PATH  = "../data/g0/message_sample_labeled.csv"

df_raw = pd.read_csv(DATA_PATH)
df_raw.head(2)

# %%
# Split messages into sentences and explode into long format
def split_sentences(text: str) -> list[str]:
    text = re.sub(r'\\n|\n', ' ', text)
    # Split on . ! ? followed by whitespace or end of string
    sentences = re.split(r'(?<=[.!?])\s+', text)
    # Clean and filter empties / very short fragments
    sentences = [s.strip() for s in sentences if len(s.strip()) > 5]
    return sentences

def load_and_explode(path: str) -> pd.DataFrame:
    df = pd.read_csv(path)
    rows = []
    for _, row in df.iterrows():
        for col in ["messageHow", "messageRules"]:
            text = row.get(col, "")
            if pd.isna(text) or not str(text).strip():
                continue
            for sentence in split_sentences(str(text)):
                rows.append({
                    "id": row["id"],
                    "condition": row["condition"],
                    "source_col": col,
                    "sentence": sentence,
                })
    return pd.DataFrame(rows)


df = load_and_explode(DATA_PATH)
print(f"Total sentences extracted: {len(df)}")

# Save the exploded sentences for review before labeling
# df.to_csv('../data/g0/messages_split.csv', index=False) 



# %%
load_dotenv()
client = OpenAI()

LABEL_PROMPT = """You are labeling sentences from game tips written by participants.

Classify the sentence into exactly one of these categories:
- rule: describes what kinds of items should be combined or not (e.g. "Triangles combine with diamonds", "Merge same shapes", "Colour and pattern doesn't seem to matter.")
- strategy: a high-level approach to the task (e.g. "try different variations")
- tip: practical tip on how to use the game interface (e.g. "Using the arrow keys")
- other: anything that doesn't fit the above

Respond with ONLY the category label, nothing else.

Sentence: {sentence}"""


def label_sentence(sentence: str) -> str:
    response = client.chat.completions.create(
        model="gpt-4o-mini",
        messages=[
            {"role": "user", "content": LABEL_PROMPT.format(sentence=sentence)}
        ],
        temperature=0,
    )
    return response.choices[0].message.content.strip().lower()


def label_dataframe(df: pd.DataFrame) -> pd.DataFrame:
    df = df.copy()
    df["label"] = df["sentence"].apply(label_sentence)
    return df


# df = pd.read_csv('../data/pilot/messages_split.csv')
df_labeled = label_dataframe(df)
df_labeled.to_csv(OUTPUT_PATH, index=False)
print(df_labeled["label"].value_counts())
# %%
