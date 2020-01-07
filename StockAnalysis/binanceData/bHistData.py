# IMPORTS
import pandas as pd
import math
import os.path
import time
#from bitmex import bitmex
from binance.client import Client
from datetime import timedelta, datetime
from dateutil import parser
from tqdm import tqdm_notebook #(Optional, used for progress-bars)
from hurst import compute_Hc
from numpy import cumsum, log, polyfit, sqrt, std, subtract
from numpy.random import randn


### API
bitmex_api_key = '[REDACTED]'    #Enter your own API-key here
bitmex_api_secret = '[REDACTED]' #Enter your own API-secret here
binance_api_key = 'X07GyKQ9VaDKKeXX3A6hI4bwTHbEVzrQAaUHTCAjFkfwMAEdsjPThE1dB4hWQK2r'    #Enter your own API-key here
binance_api_secret = 'eaUAK7pkejndSiYZcTQdvq59ZUeY1kY2RWT6Bp0lZgfLdESeE7z8p0lXLc5DEXsO' #Enter your own API-secret here

### CONSTANTS
binsizes = {"1m": 1, "5m": 5, "1h": 60, "1d": 1440}
batch_size = 750
#bitmex_client = bitmex(test=False, api_key=bitmex_api_key, api_secret=bitmex_api_secret)
binance_client = Client(api_key=binance_api_key, api_secret=binance_api_secret)


### FUNCTIONS
def minutes_of_new_data(symbol, kline_size, data, source):
    if len(data) > 0:  old = parser.parse(data["timestamp"].iloc[-1])
    elif source == "binance": old = datetime.strptime('1 Jan 2017', '%d %b %Y')
    elif source == "bitmex": old = bitmex_client.Trade.Trade_getBucketed(symbol=symbol, binSize=kline_size, count=1, reverse=False).result()[0][0]['timestamp']
    if source == "binance": new = pd.to_datetime(binance_client.get_klines(symbol=symbol, interval=kline_size)[-1][0], unit='ms')
    if source == "bitmex": new = bitmex_client.Trade.Trade_getBucketed(symbol=symbol, binSize=kline_size, count=1, reverse=True).result()[0][0]['timestamp']
    return old, new

def get_all_binance(symbol, kline_size, save = False):
    filename = '%s-%s-data.csv' % (symbol, kline_size)
    if os.path.isfile(filename): data_df = pd.read_csv(filename)
    else: data_df = pd.DataFrame()
    oldest_point, newest_point = minutes_of_new_data(symbol, kline_size, data_df, source = "binance")
    delta_min = (newest_point - oldest_point).total_seconds()/60
    available_data = math.ceil(delta_min/binsizes[kline_size])
    if oldest_point == datetime.strptime('1 Jan 2017', '%d %b %Y'): print('Downloading all available %s data for %s. Be patient..!' % (kline_size, symbol))
    else: print('Downloading %d minutes of new data available for %s, i.e. %d instances of %s data.' % (delta_min, symbol, available_data, kline_size))
    klines = binance_client.get_historical_klines(symbol, kline_size, oldest_point.strftime("%d %b %Y %H:%M:%S"), newest_point.strftime("%d %b %Y %H:%M:%S"))
    data = pd.DataFrame(klines, columns = ['timestamp', 'open', 'high', 'low', 'close', 'volume', 'close_time', 'quote_av', 'trades', 'tb_base_av', 'tb_quote_av', 'ignore' ])
    data['timestamp'] = pd.to_datetime(data['timestamp'], unit='ms')
    if len(data_df) > 0:
        temp_df = pd.DataFrame(data)
        data_df = data_df.append(temp_df)
    else: data_df = data
    data_df.set_index('timestamp', inplace=True)
    if save: data_df.to_csv(filename)
    print('All caught up..!')
    return data_df
'''
def get_all_bitmex(symbol, kline_size, save = False):
    filename = '%s-%s-data.csv' % (symbol, kline_size)
    if os.path.isfile(filename): data_df = pd.read_csv(filename)
    else: data_df = pd.DataFrame()
    oldest_point, newest_point = minutes_of_new_data(symbol, kline_size, data_df, source = "bitmex")
    delta_min = (newest_point - oldest_point).total_seconds()/60
    available_data = math.ceil(delta_min/binsizes[kline_size])
    rounds = math.ceil(available_data / batch_size)
    if rounds > 0:
        print('Downloading %d minutes of new data available for %s, i.e. %d instances of %s data in %d rounds.' % (delta_min, symbol, available_data, kline_size, rounds))
        for round_num in tqdm_notebook(range(rounds)):
            time.sleep(1)
            new_time = (oldest_point + timedelta(minutes = round_num * batch_size * binsizes[kline_size]))
            data = bitmex_client.Trade.Trade_getBucketed(symbol=symbol, binSize=kline_size, count=batch_size, startTime = new_time).result()[0]
            temp_df = pd.DataFrame(data)
            data_df = data_df.append(temp_df)
    data_df.set_index('timestamp', inplace=True)
    if save and rounds > 0: data_df.to_csv(filename)
    print('All caught up..!')
    return data_df
'''

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

btcs_pairlist = ["BTCUSDT",
                "TOMOBTC",
                "ATOMBTC",
                "MATICBTC",
                "BNBBTC"]

# list of tickers to check up on
    btc_pairlist = ["BTCUSDT",
                "TOMOBTC",
                "ATOMBTC",
                "MATICBTC",
                "BNBBTC",
                "ETHBTC",
                "BTCUSDC",
                "LINKBTC",
                "STRATBTC",
                "XRPBTC",
                "XTZBTC",
                "ENJBTC",
                "WAVESBTC",
                "XMRBTC",
                "VETBTC",
                "THETABTC",
                "ZECBTC",
                "BTCBUSD",
                "TROYBTC",
                "EOSBTC",
                "KAVABTC",
                "BCHBTC",
                "LTCBTC",
                "DATABTC",
                "INSBTC",
                "BATBTC",
                "BTCTUSD",
                "BTCPAX",
                "RVNBTC",
                "ADABTC",
                "NEOBTC",
                "WABIBTC",
                "TRXBTC",
                "FETBTC",
                "XVGBTC",
                "ALGOBTC",
                "VITEBTC",
                "STXBTC",
                "MITHBTC",
                "ONEBTC",
                "ARPABTC",
                "ERDBTC",
                "FTMBTC",
                "DASHBTC",
                "BEAMBTC",
                "AIONBTC",
                "TNTBTC",
                "VIBBTC",
                "LSKBTC",
                "CHZBTC",
                "WPRBTC",
                "XLMBTC",
                "RENBTC",
                "KNCBTC",
                "ARNBTC",
                "DOCKBTC",
                "QTUMBTC",
                "NANOBTC",
                "IOTABTC",
                "GOBTC",
                "LOOMBTC",
                "IOSTBTC",
                "SNGLSBTC",
                "ONTBTC",
                "RLCBTC",
                "DCRBTC",
                "STEEMBTC",
                "ICXBTC",
                "ZRXBTC",
                "QSPBTC",
                "ZENBTC",
                "ENGBTC",
                "ETCBTC",
                "NKNBTC",
                "ZILBTC",
                "REPBTC",
                "FUELBTC",
                "WTCBTC",
                "BQXBTC",
                "HBARBTC",
                "POWRBTC",
                "MANABTC",
                "CELRBTC",
                "FUNBTC",
                "MTLBTC",
                "IOTXBTC",
                "BANDBTC",
                "POLYBTC",
                "DOGEBTC",
                "CNDBTC",
                "LENDBTC"]

# some empty lists to store data in
close_list = [None] * len(btc_pairlist)
hurst_list = [None] * len(btc_pairlist)
alt_hurst_list = [None] * len(btc_pairlist)
length_list = [None] * len(btc_pairlist)



# for each symbol get the data and store it in our list
for index, symbol in enumerate(btc_pairlist):
    close_list[index] = get_all_binance(symbol, '1h', save = False)['close']
    length_list[index] = len(close_list[index])

# create a dataframe with all the closes so we can get a correlation matrix
close_df = pd.concat(close_list, axis=1, sort=False)
close_df.columns = btc_pairlist
close_df = close_df.apply( pd.to_numeric, errors='coerce' )

#calculate the correlation matrix
corr_matrix = close_df.corr()
print(corr_matrix)

corr_filename = 'correlation_matrix.csv'
corr_matrix.to_csv(corr_filename)

# calculate the hurst exponent for each symbol and store it in our list
for index, elem in enumerate(close_list):
    closes = pd.to_numeric(close_list[index]).values
    hurst_list[index]= compute_Hc(closes,kind='price')[0]
    alt_hurst_list[index] = hurst(closes)

# create the hurst exponent dataframe
df = pd.DataFrame(list(zip(btc_pairlist,hurst_list,alt_hurst_list,length_list)),
              columns=['ticker','hurst_exponent','althurstlst','number_of_hours'])

print(df)
filename = 'hurst_data.csv'

df.to_csv(filename)
