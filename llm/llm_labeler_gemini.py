#!/usr/bin/env python
#
# W.M. Otte (w.m.otte@umcutrecht.nl)
# 
# Use LLM to label primary/secondary outcomes
#
# Install requirements: pip install google-generativeai
# Set API key: export GEMINI_API_KEY="your_api_key_here"
################################################################################

import json
import argparse
import os
import google.generativeai as genai

MODEL_NAME = "gemini-2.5-flash-lite-preview-06-17"

def call_gemini_llm(outcome, primary_text, secondary_text):
    """Call Google Gemini API to classify outcomes"""
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
        # Configure the API key
        api_key = os.getenv('GEMINI_API_KEY')
        if not api_key:
            raise ValueError("GEMINI_API_KEY environment variable not set")
        
        genai.configure(api_key=api_key)
        
        # Initialize the model
        model = genai.GenerativeModel(MODEL_NAME)
        
        # Generate response
        response = model.generate_content(prompt)
        raw = response.text.strip().lower()
        
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
        label = call_gemini_llm(outcome, primary_text, secondary_text)
        labeled.append({"outcome": outcome, "label": label})
        #print(f"Labeled '{outcome}' as {label}")

    data['labeled_outcomes'] = labeled

    with open(output_path, 'w', encoding='utf-8') as f:
        json.dump(data, f, indent=2, ensure_ascii=False)

    #print(f"\n✅ Output written to: {output_path}")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Label outcomes using Google Gemini LLM.")
    parser.add_argument('--input', '-i', required=True, help='Path to input JSON file')
    parser.add_argument('--output', '-o', required=True, help='Path to output JSON file')
    args = parser.parse_args()

    if not os.path.isfile(args.input):
        print(f"❌ Input file does not exist: {args.input}")
        exit(1)

    label_outcomes(args.input, args.output)
