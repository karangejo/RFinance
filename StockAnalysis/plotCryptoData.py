import pandas as pd
import numpy as np
import talib.abstract as ta
import datetime as dt
import plotly.graph_objects as go
from plotly.subplots import make_subplots
from hurst import compute_Hc
from numpy import cumsum, log, polyfit, sqrt, std, subtract
from numpy.random import randn

# read the data
df = pd.read_csv('/home/karang/Documents/freqtrade/user_data/data/onehour/ETHUSDT1h.csv')
'''
# calculation for daily data
number_of_weeks = 120
data_length = number_of_weeks * 7
'''
# calculation for hourly data
number_of_days = 700
# 24 hours in a day for hourly data
data_length = number_of_days * 24

df = df.tail(data_length)

# calculate the rolling vwap
# 7 days a week
win = 48
nrows = len(df.index)
splits = nrows/win
splitdf = np.array_split(df, splits)
vwaplist = [None] * len(splitdf)

# rolling vwap helper function
def split_vwap(df):
    df['split_vwap'] = (df['volume']*(df['high']+df['low'])/2).cumsum()/df['volume'].cumsum()
    return df

# hurst exponent helper function
def hurst(ts):
    """Returns the Hurst Exponent of the time series vector ts"""
    # Create the range of lag values
    lags = range(2, 100)

    # Calculate the array of the variances of the lagged differences
    tau = [sqrt(std(subtract(ts[lag:], ts[:-lag]))) for lag in lags]

    # Use a linear fit to estimate the Hurst Exponent
    poly = polyfit(log(lags), log(tau), 1)

    # Return the Hurst exponent from the polyfit output
    return poly[0]*2.0

# calculate rolling vwap
for index, dataframe in enumerate(splitdf):
    vwaplist[index] = split_vwap(dataframe)

df = pd.concat(vwaplist)

# get the dates in the right format and set it as index
df['datetime'] = pd.to_datetime(df['date'])
df.set_index('datetime')

# calculate the total period vwap
df['vwap'] = (df['volume']*(df['high']+df['low'])/2).cumsum()/df['volume'].cumsum()

# calculate the SAR
df['sar'] = ta.SAR(df, acceleration=0.01, maximum=0.02)

# calculate the Hurst Exponent and print it
H, c, data = compute_Hc(df['close'],kind='price')
alt_Hurst = hurst(df['close'].values)
print("Hurst Exponent:")
print(H)
print(alt_Hurst)

# calculate the rolling hurst Exponent for each week
def get_rolling_hurst(dataframe):
    period = 24*30
    df['H'] = 0
    for i in range(period,len(dataframe)):
        dataframe.H.iloc[i] =  compute_Hc(dataframe.close.iloc[i-period:i-1],kind='price')[0]
    return dataframe['H']

df['rolling_hurst']=get_rolling_hurst(df)

# calculate the split hurst exponent
# 90 days is one quarter
win = 24*30
nrows = len(df.index)
splits = nrows/win
splitdf = np.array_split(df, splits)
splithurstlist = [None] * len(splitdf)

# split_hurst helper function
def split_hurst(df):
    df['split_hurst'] = compute_Hc(df['close'],kind='price')[0]
    df['alt_split_hurst'] = hurst(df['close'].values)
    return df


# calculate split hurst
for index, dataframe in enumerate(splitdf):
    splithurstlist[index] = split_hurst(dataframe)

df = pd.concat(splithurstlist)

# plot everything with plotly

fig = make_subplots(rows=7, cols=1,shared_xaxes=True,specs=[[{"rowspan":4}],[None],[None],[None],[{}],[{}],[{}]])

fig.add_trace(go.Candlestick(x=df['datetime'],
                open=df['open'],
                high=df['high'],
                low=df['low'],
                close=df['close']),
                row=1, col=1)

fig.add_trace(go.Scatter(x=df['datetime'], y=df['ema10'],
                    mode='lines',
                    name='ema10'),
                    row=1, col=1)

fig.add_trace(go.Scatter(x=df['datetime'], y=df['split_vwap'],
                    mode='lines',
                    name='split_vwap'),
                    row=1, col=1)

fig.add_trace(go.Scatter(x=df['datetime'], y=df['sar'],
                    mode='markers',
                    name='sar'),
                    row=1, col=1)

'''
fig.add_trace(go.Scatter(x=df['datetime'], y=df['vwap'],
                    mode='lines',
                    name='vwap'),
                    row=1, col=1)
'''
fig.add_trace(go.Scatter(x=df['datetime'], y=df['bb_upperband'],
                    mode='lines',
                    line=dict(color='rgba(0, 255, 0, 0.1)'),
                    name='bbupper'),
                    row=1, col=1)

fig.add_trace(go.Scatter(x=df['datetime'], y=df['bb_lowerband'],
                    mode='lines',
                    line=dict(color='rgba(0, 255, 0, 0.1)'),
                    name='bblower',
                    fill='tonexty',
                    fillcolor='rgba(0, 0, 255, 0.1)'),
                    row=1, col=1)
'''
fig.add_trace(go.Scatter(x=df['datetime'], y=df['close'],
                    mode='lines',
                    name='close'),
                    row=1, col=1)
'''

fig.add_trace(go.Scatter(x=df['datetime'], y=df['split_hurst'],
                    mode='lines',
                    name='split_hurst'),
                    row=5, col=1)

fig.add_trace(go.Scatter(x=df['datetime'], y=df['alt_split_hurst'],
                    mode='lines',
                    name='alt_split_hurst'),
                    row=5, col=1)

fig.add_trace(go.Scatter(x=df['datetime'], y=df['rolling_hurst'],
                    mode='lines',
                    name='rolling_hurst'),
                    row=5, col=1)

fig.add_trace(go.Scatter(x=df['datetime'], y=df['rsi'],
                    mode='lines',
                    name='rsi'),
                    row=6, col=1)

fig.add_trace(go.Bar(x=df['datetime'], y=df['volume'], name='volume'),row=7, col=1)

fig.update_layout(xaxis_rangeslider_visible=False)
fig.show()
