
# * Datathon 2024 - Hey Banco - Outliers
# * Pronósticos e integración con LLM
# * Licencia: GPL-3.0

from vaderSentiment.vaderSentiment import SentimentIntensityAnalyzer
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import requests
import os 
from dotenv import load_dotenv

def read_data(path):
    df = pd.read_csv(path, index_col=0)
    df.drop(["year_month", "year", "time"], axis=1, inplace=True)
    df['tweet'] = df['tweet'].astype(str)
    return df

def get_sentiment_score(comment):
    global sia 
    sia = SentimentIntensityAnalyzer()
    return sia.polarity_scores(comment)['compound']

def classify_sentiment(score):
    if score >= 0.05:
        return 'Positive'
    elif score <= -0.05:
        return 'Negative'
    else:
        return 'Neutral'

# Integración a OpenAI
def get_openai_response(prompt, model="gpt-3.5-turbo"):

    # load the .env file
    load_dotenv()

    OPENAI_API_KEY = os.environ.get('OPENAI_API_KEY')

    headers = {
        "Content-Type": "application/json",
        "Authorization": f"Bearer {OPENAI_API_KEY}"
    }

    response_json = requests.post("https://api.openai.com/v1/chat/completions", headers=headers, json={
        "model": model,
        "messages": [{"role": "user", "content": prompt}],
        "temperature": 0
    }).json()

    return response_json["choices"][0]["message"]["content"]

def get_summary(comments):
    prompt = f""" I am going to give you a list of comments made posts on twitter. The posts the people are commenting are from a digital bank company in latam. The comments are in spanish, give me a summary of the comments and what are the main topics that the people are talking about. The list is {comments}.

    Just give me strictly the summary of the comments and the main topics that the people are talking about and don't include any other information.
    Do not use special characters like \n-, 'summary:' or any other. 
    Give me the results in SPANISH.
    """
    return get_openai_response(prompt, "gpt-3.5-turbo")

def get_summaries(tweets):
    summaries = {}
    for key, tweet_list in tweets.items():
        summaries[key] = get_summary(tweet_list)
    return summaries

def get_monthly_summaries_of_tweets(data):

    df = data.copy()

    df.drop(["sentiment_score", "sentiment"], axis=1, inplace=True)
    df.index = pd.to_datetime(df.index)

    tweets = {}
    for date, tweet in zip(df.index, df["tweet"]):
        key = str(date.year) + "-" + str(date.month) + "-" + str(date.days_in_month)
        if key not in tweets:
            tweets[key] = []
        tweets[key].append(tweet)

    summaries = get_summaries(tweets)

    summaries_df = pd.DataFrame(summaries.items(), columns=["date", "summary"])
    summaries_df.index = pd.to_datetime(summaries_df["date"])
    summaries_df.drop("date", axis=1, inplace=True)

    return summaries_df

# metrics
def get_pnratio(df):
    # make the data int type
    df["Positive"] = df["Positive"].astype(int)
    df["Negative"] = df["Negative"].astype(int)
    for i, row in df.iterrows():
        if row["Negative"] == 0:
            df.loc[i, "P/N Ratio"] = row["Positive"]

        if row["Positive"] == 0:
            df.loc[i, "P/N Ratio"] = -row["Negative"]

        if row["Positive"] != 0 and row["Negative"] != 0:
            df.loc[i, "P/N Ratio"] = row["Positive"] / row["Negative"]

    return df

def get_ratios(df):
    df["Positive Ratio"] = df["Positive"] / df["total"]
    df["Negative Ratio"] = df["Negative"] / df["total"]
    df["Neutral Ratio"] = df["Neutral"] / df["total"]
    return df

def get_metrics(df):
    sentiment_counts = df.resample("M").sentiment.value_counts().unstack().fillna(0)
    sentiment_counts["total"] = sentiment_counts.sum(axis=1)
    sentiment_counts["Positive"] = sentiment_counts["Positive"] 
    sentiment_counts["Negative"] = sentiment_counts["Negative"] 
    sentiment_counts["Neutral"] = sentiment_counts["Neutral"]
    new_df = get_pnratio(sentiment_counts)
    new_df = get_ratios(new_df)
    new_df["Adjusted Social Sentiment"] = ((new_df["Positive"] * new_df["Positive Ratio"]) + (new_df["Negative"] * new_df["Negative Ratio"]) + (new_df["Neutral"] * new_df["Neutral Ratio"])) / new_df["total"]
    return new_df

def social_sentiment_model(df):
    df['sentiment_score'] = df['tweet'].apply(get_sentiment_score) 
    df['sentiment'] = df['sentiment_score'].apply(classify_sentiment)
    return df

def run_pipeline():

    df = read_data("dataset.csv")
    df = social_sentiment_model(df)
    summaries_df = get_monthly_summaries_of_tweets(df)

    df.index = pd.to_datetime(df.index)
    df_with_metrics = get_metrics(df)
    df = pd.concat([df_with_metrics, summaries_df], axis=1)

    df.to_csv("dataset_final_quantitative.csv")

run_pipeline()
