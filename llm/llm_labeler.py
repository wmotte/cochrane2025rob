#!/usr/bin/env python
#
# W.M. Otte (w.m.otte@umcutrecht.nl)
# 
# Use LLM to label primary/secondary outcomes
#
# Run first (to install local LLM): ollama run gemma3n
################################################################################

import json
import subprocess
import argparse
import os

MODEL_NAME = 'gemma3n'

def call_local_llm(outcome, primary_text, secondary_text):
    prompt = f"""
You are helping classify standardized medical outcomes based on provided context.

Primary outcome examples:
{primary_text}

Secondary outcome examples:
{secondary_text}

Now classify the following outcome as either 'primary' or 'secondary'.
Outcome: "{outcome}"

Reply ONLY with 'primary' or 'secondary'.
"""

    try:
        result = subprocess.run(
            ['ollama', 'run', MODEL_NAME],
            input=prompt.encode('utf-8'),
            capture_output=True,
            timeout=60
        )
        raw = result.stdout.decode('utf-8').strip().lower()
        if 'primary' in raw:
            return 'primary'
        elif 'secondary' in raw:
            return 'secondary'
        else:
            return 'unknown'
    except Exception as e:
        print(f"Error classifying outcome '{outcome}': {e}")
        return 'unknown'

def label_outcomes(input_path, output_path):
    with open(input_path, 'r', encoding='utf-8') as f:
        data = json.load(f)

    primary_text = data.get("primary", "")
    secondary_text = data.get("secondary", "")
    outcomes = data.get("outcomes", [])

    labeled = []
    for outcome in outcomes:
        label = call_local_llm(outcome, primary_text, secondary_text)
        labeled.append({"outcome": outcome, "label": label})
        #print(f"Labeled '{outcome}' as {label}")

    data['labeled_outcomes'] = labeled

    with open(output_path, 'w', encoding='utf-8') as f:
        json.dump(data, f, indent=2, ensure_ascii=False)

    #print(f"\n✅ Output written to: {output_path}")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Label outcomes using local Gemma3n LLM via Ollama.")
    parser.add_argument('--input', '-i', required=True, help='Path to input JSON file')
    parser.add_argument('--output', '-o', required=True, help='Path to output JSON file')
    args = parser.parse_args()

    if not os.path.isfile(args.input):
        print(f"❌ Input file does not exist: {args.input}")
        exit(1)

    label_outcomes(args.input, args.output)

