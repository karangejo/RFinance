from twitterscraper import query_tweets
from vaderSentiment.vaderSentiment import SentimentIntensityAnalyzer
import datetime
import pandas as pd

# start the ananlyzer
analyzer = SentimentIntensityAnalyzer()
# query for twitter
search_term = 'bitcoin'
# limit to amount of query results (in multiples of 20)
limit = 10

# helper method to get the day before a date
def get_day_before(date):
    two_dates = pd.date_range(end = date,periods = 2).to_pydatetime().tolist()
    print(two_dates)
    return two_dates[0]

# helper method to calculate average compound sentiment score from list of tweets
def get_average_score_from_tweets(tweet_list):
    tweet_scores = [None] * len(tweet_list)
    for index,tweet in enumerate(tweet_list):
        # analyze the tweets and store the compound score in the list
        tweet_scores[index] = analyzer.polarity_scores(tweet.text)['compound']
    # calculate the average compound score
    average_score = (sum(tweet_scores)/len(tweet_scores))
    return  average_score

# get a historical sentiment analysis day by day for the query for the period specified
def get_hist_sentiment_dataframe(query,num_tweets,period):
    # get some dates and specify the period
    dates = pd.date_range(end = pd.datetime.today(), periods = period).to_pydatetime().tolist()
    # lists to store tweets and sentiment scores
    scores = [None] * len(dates)
    scrapped_list = [None] * len(dates)

    # go through each day and get tweets and analyze them
    for index,date in enumerate(dates):
        # get the tweets
        scrapped_list[index]= query_tweets(query,limit=num_tweets,begindate=get_day_before(date.date()).date(),enddate=date.date(),lang='en')
        # analyze and return the average compound score
        scores[index] = get_average_score_from_tweets(scrapped_list[index])

    #organize the dataframe
    df = pd.DataFrame(list(zip(dates, scores)),
                   columns =['dates', 'scores'])
    # transform to date time with no minutes or hours data
    df['dates'] = pd.to_datetime(df['dates'],format='%Y/%m/%d').dt.date
    df.set_index('dates')

    return df

def save_dataframe(dataframe,name,):
    filename = '/home/karang/Documents/data/'+name+'.csv'
    dataframe.to_csv(filename)

bitcoindf = get_hist_sentiment_dataframe(search_term,limit,5)
print(bitcoindf)
save_dataframe(bitcoindf,search_term+'Sentiment5d')
