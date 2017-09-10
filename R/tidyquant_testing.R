
library(timetk)
library(tidyquant)



# Tidy Quant Walkthrough --------------------------------------------------
# https://github.com/mdancho84/journal/blob/master/r_in_finance/R% --------

sp500 <- tq_index("SP500")
sp500

stock_prices <- sp500 %>%
  tq_get(get  = "stock.prices", 
         from = "2007-01-01", 
         to   = "2017-01-01")
stock_prices

sp500_returns <- stock_prices %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted, transform_fun = periodReturn, 
               period = "daily", type = "log", col_rename = "dlr")
sp500_returns

object.size(stock_prices)

sp500 %>% 
  left_join()
  mutate(mkt_cap = weight * shares_held)


# timetk testing ----------------------------------------------------------
# https://business-science.github.io/timetk/


FB_tbl <- FANG %>%
  filter(symbol == "FB")
FB_tbl

idx <- tk_index(FB_tbl)
head(idx)

FB_xts <- tk_xts(FB_tbl, silent = TRUE)



#  ------------------------------------------------------------------------



# Loads tidyquant, tidyverse, lubridate, xts, quantmod, TTR 
# following this tutorial on tidyquant:

# https://cran.r-project.org/web/packages/tidyquant/vignettes/TQ02-quant-integrations-in-tidyquant.html

library(tidyquant)


data(FANG)
#FANG$symbol %>% unique
FANG %>% group_by(symbol) %>% 
  summarise(count = n())

FANG_annual_returns <- FANG %>% 
  group_by(symbol) %>% 
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = 'yearly',
               type       = 'arithmetic')


FANG_annual_returns %>% 
  ggplot(aes(x = date, y = yearly.returns, fill = symbol)) +
  geom_bar(stat = 'identity') +
  facet_wrap( ~ symbol, ncol = 2)

# get daily log returns

FANG_daily_log_returns <- FANG %>% 
  group_by(symbol) %>% 
  tq_transmute(select      = adjusted,
               mutate_fun  = periodReturn,
               period      = 'daily',
               type        = 'log',
               col_rename  = 'daily.returns')
FANG_daily_log_returns %>% 
  ggplot(aes(x = daily.returns)) +
  geom_density(alpha = 0.5) +
  facet_wrap( ~ symbol, ncol = 2) +
  labs(title = 'FANG: Charting daily log returns')

# chart daily price series

FANG_daily <- FANG %>% 
  group_by(symbol)

FANG_daily %>% 
  ggplot(aes(x = date, y = adjusted, color = symbol)) +
  geom_line(size = 1) +
  facet_wrap( ~ symbol, scales = 'free_y') # scales = free_y means make each chart its own scale

# get rolling correlations
spy_tlt <- 
  c('SPY','TLT') %>% 
  tq_get(get  = "stock.prices",
         from = "2013-01-01", 
         to   = "2016-12-31") 

spy_tlt %>% 
  ggplot(aes(x=date, y=adjusted, color = symbol)) + 
  geom_line(size = 1) +
  facet_wrap( ~ symbol, scales = 'free_y')

spy_tlt[spy_tlt$symbol == 'TLT',]
spreaded <-  spread(spy_tlt, key = symbol, value = adjusted, fill = NA, drop = FALSE)

t <- spy_tlt %>% 
  filter(date == '2013-01-02')

t %>% 
  spread(key = symbol, value = adjusted) %>% 
  select(date, 7:ncol(t)) 



# ONE TICKER AT A TIME ----------------------------------------------------

tickers <- c('SPY','TLT')
from = '2005-01-01'
to   = '2017-07-31'

adj = list()

for (ticker in tickers)
{
  adj[[ticker]] <-  tq_get(ticker,
                           get  = "stock.prices",
                           from,
                           to) %>% 
    select(date, adjusted)
  
}

do.call('left_join', adj)

Reduce(function(...) merge(..., all=T), adj)

# this seems to work
adj %>%
  Reduce(function(dtf1,dtf2) left_join(dtf1,dtf2,by="date"), .) 




# Performance Analysis ----------------------------------------------------
# https://cran.r-project.org/web/packages/tidyquant/vignettes/TQ05-performance-analysis-with-tidyquant.html
install.packages("TTR")

list_symbols <- c('SPY','EFA','EEM','TLT','GLD')

monthly_returns <- list_symbols %>% 
  tq_get(get = 'stock.prices',
         from = '2010-01-01',
         to   = '2016-12-31') %>% 
  group_by(symbol) %>% 
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = 'monthly',
               col_rename = 'returns') 

monthly_returns %>% 
  ggplot(aes(date, returns, color = symbol)) +
  geom_bar()

monthly_ranks <- 
  monthly_returns %>% 
  group_by(date) %>% 
  mutate(my_ranks = order(returns,decreasing=TRUE))

# rank on each date, working  
monthly_ranks <- 
  monthly_returns %>% 
  group_by(date) %>% 
  mutate(my_ranks = min_rank(returns)) 



monthly_ranks %>%
  select(date, symbol, my_ranks) %>%
  spread(symbol, my_ranks) 

#  tq_transmute(select = returns,
#               mutate_fun = rank)

monthly_returns %>% 
  filter(date == '2010-01-29')

# split stacked df into a list, then merge together
# Ra %>% 
#   split(Ra$symbol) %>% 
#   Reduce(function(dtf1,dtf2) full_join(dtf1,dtf2,by="date"), .)
#   #do.call(cbind, .)

# this works
monthly_returns_xts <-
  as_xts(monthly_returns_long, date_col = date)

cor(monthly_returns_xts)

monthly_returns_long <- monthly_returns %>% 
  spread(symbol, returns)

monthly_returns_long %>% 
  as.xts %>% 
  cov



monthly_returns_long %>% 
  select(-date) %>% 
  apply(1,rank) %>% 
  t %>% 
  as.tibble %>% 
  cbind(.,monthly_returns_long$date)

#  ------------------------------------------------------------------------

stocks <- data.frame(time = as.Date('2009-01-01') + 0:9,
                     X = rnorm(10, 0, 1),
                     Y = rnorm(10, 0, 2),
                     Z = rnorm(10, 0, 4))
stocksm <- gather(stocks, stock, price, -time)
spread.stock <- spread(stocksm, stock, price)
head(spread.stock)


Rb <- '^GSPC' %>% 
  tq_get(get = 'stock.prices',
         from = '2010-01-01',
         to   = '2016-12-31') %>% 
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = 'monthly',
               col_rename = 'Rb')
Rb

RaRb <- Ra %>% 
  left_join(Rb, on = "date")

RaRb_capm <- RaRb %>% 
  tq_performance(Ra = Ra,
                 Rb = Rb,
                 performance_fun = table.CAPM)

RaRb_capm %>% 
  select(Alpha, Beta, InformationRatio)

# sharpe

Ra %>% tq_performance(Ra = Ra,
                      performance_fun = SharpeRatio)


# Portfolios --------------------------------------------------------------

wts_map <- tibble(
  symbols = c("SPY", "TLT"),
  weights = c(0.5, 0.5)
)
wts_map

returns_monthly <- c('SPY','TLT') %>% 
  tq_get(get = 'stock.prices',
         from = '2006-01-01',
         to   = '2016-12-31') %>% 
  group_by(symbol) %>% 
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = 'monthly',
               col_rename = 'Ra')

portfolio_returns <-
  returns_monthly %>% 
  tq_portfolio(assets_col = symbol,
               returns_col = Ra,
               weights     = wts_map,
               col_rename  = 'portfolio_returns') 

# %>%
#                #wealth.index = TRUE) %>%  # if we want a wealth curve instead of returns
#   ggplot(aes(date, portfolio_returns)) +
#   geom_line(size=1)
# 

baseline_returns_monthly <- "^GSPC" %>%
  tq_get(get  = "stock.prices",
         from = "2006-01-01",
         to   = "2016-12-31") %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "spx_returns")
baseline_returns_monthly

merged <- portfolio_returns %>%  
  left_join(baseline_returns_monthly, on="date")
merged

merged %>% tq_performance(Ra = portfolio_returns,
                          Rb = NULL,
                          performance_fun = table.AnnualizedReturns)

# SP500 -------------------------------------------------------------------

tq_index("SP500") %>%
  tq_get(get = "stock.prices")

tq_get("SP500", get = "stock.index") 

# Get Fama French ---------------------------------------------------------
library(xts)
fama_french <- tq_get("KFRENCH/FACTORS_M", get = "quandl", to = "2017-02-24")

api_key <-  "-UVzwX5xnzsm6x68A1Qq"
quandl_api_key(api_key)

fama_french_3_M <- "KFRENCH/FACTORS_M" %>%
  tq_get(get      = "quandl",
         collapse = "monthly")
fama_french_3_M

Quandl("KFRENCH/FACTORS_M") 

# joins -------------------------------------------------------------------

library(nycflights13)
airlines

flights2 <- flights %>% 
  select(year:day, hour, origin, dest, tailnum, carrier)

flights2 %>% 
  select(-origin, -dest) %>% 
  left_join(airlines, by = 'carrier')

airports %>% 
  
