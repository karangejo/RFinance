import datetime as dt
import matplotlib.pyplot as plt
from matplotlib import style
import pandas as pd
import pandas_datareader.data as web

style.use('ggplot')

start = dt.datetime(2000,1,1)
end = dt.datetime(2019,12,1)

df = web.DataReader('TSLA','yahoo',start,end)

#df.to_csv('filename.csv')
#df = pd.read_csv('filename.csv',parse_dates=True,index_col=0)
#print(df.head())
#df['Adj Close'].plot()
#plt.show()

# calculate 100 day moving average
df['100ma'] = df['Adj Close'].rolling(window=100,min_periods=0).mean()
df['50ma'] = df['Adj Close'].rolling(window=50,min_periods=0).mean()


ax1 = plt.subplot2grid((6,1),(0,0), rowspan=5,colspan=1)
ax2 = plt.subplot2grid((6,1),(5,0), rowspan=1,colspan=1,sharex=ax1)

ax1.plot(df.index,df['Adj Close'])
ax1.plot(df.index,df['100ma'])
ax1.plot(df.index,df['50ma'])
ax2.bar(df.index,df['Volume'])
plt.show()
