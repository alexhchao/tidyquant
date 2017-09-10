# portfolio optimization (not tidyquant)
# https://www.r-bloggers.com/portfolio-optimization-using-r-and-plotly/
#install.packages("PortfolioAnalytics")
library(PortfolioAnalytics)
library(quantmod)
library(PerformanceAnalytics)
library(zoo)
library(plotly)

# Get data
getSymbols(c("MSFT", "SBUX", "IBM", "AAPL", "^GSPC", "AMZN"))

# Assign to dataframe
# Get adjusted prices
prices.data <- merge.zoo(MSFT[,6], SBUX[,6], IBM[,6], AAPL[,6], GSPC[,6], AMZN[,6])

# Calculate returns
returns.data <- ROC(prices.data,1)
returns.data <- na.omit(returns.data)

# Set names
colnames(returns.data) <- c("MSFT", "SBUX", "IBM", "AAPL", "^GSPC", "AMZN")

# Save mean return vector and sample covariance matrix
meanReturns <- colMeans(returns.data)
covMat <- cov(returns.data)

# Start with the names of the assets
port <- portfolio.spec(assets = c("MSFT", "SBUX", "IBM", "AAPL", "^GSPC", "AMZN"))

# Box
port <- add.constraint(port, type = "box", min = 0.05, max = 0.8)

# Leverage
port <- add.constraint(portfolio = port, type = "full_investment")


# Get minimum variance portfolio
minvar.port <- add.objective(port, type = "risk", name = "var")

# Generate random portfolios
rportfolios <- random_portfolios(port, permutations = 500000, rp_method = "sample")

# Optimize
minvar.opt <- optimize.portfolio(returns.data, minvar.port, optimize_method = "random", 
                                 rp = rportfolios)

# Generate maximum return portfolio
maxret.port <- add.objective(port, type = "return", name = "mean")

# Optimize
maxret.opt <- optimize.portfolio(returns.data, maxret.port, optimize_method = "random", 
                                 rp = rportfolios)

# Generate vector of returns
minret <- 0.06/100
maxret <- maxret.opt$weights %*% meanReturns

vec <- seq(minret, maxret, length.out = 100)


# using portfolio analytics package
# https://cran.r-project.org/web/packages/PortfolioAnalytics/vignettes/ROI_vignette.pdf

#  ------------------------------------------------------------------------


install.packages("ROI")
library(ROI)
library(ROI.plugin.glpk)
library(ROI.plugin.quadprog)
library(ROI.plugin.symphony)


# Get data
list_symbols <- c("SPY","EFA","EEM","TLT","GLD") 
data <- new.env()
getSymbols(list_symbols,env = data, auto.assign = TRUE)

# Assign to dataframe
# Get adjusted prices
#prices.data <- merge.zoo(MSFT[,6], SBUX[,6], IBM[,6], AAPL[,6], GSPC[,6], AMZN[,6])
#map(data, merge)


from = '2016-01-01'
to = '2017-08-31'
freq <- 'months'


my_list <- list()
for (ticker in list_symbols)
{
 my_list[[ticker]] <- get_adj_close_from_yhoo(ticker,
                          from,
                          to,
                          freq=freq)
}

# combines from list to one df
prices.df <- do.call(merge, my_list)
colnames(prices.df) <- list_symbols

# Calculate returns
returns.data <- ROC(prices.df,1)
returns.data <- na.omit(returns.data)

funds <- colnames(returns.data) 
portf_minvar <- portfolio.spec(assets=funds)
portf_minvar <- add.constraint(portfolio=portf_minvar, type="full_investment")
portf_minvar <- add.objective(portfolio=portf_minvar, type="risk", name="var")

# run the optim
opt_gmv <- optimize.portfolio(R=returns.data, portfolio=portf_minvar, 
                              optimize_method="ROI", trace=TRUE)

# back testing

bt_minvar <- optimize.portfolio.rebalancing(R=returns.data, portfolio=portf_minvar, 
                                            optimize_method="ROI",
                                            rebalance_on="months",
                                            training_period=12)

bt_minvar
bt_minvar$R

# good but doesnt show details of the backtest

list_cov = list()
list_wts <- list()
lback <-  12
idx <-  returns.data %>%  index
for (i in seq(lback+1,nrow(returns.data)))
{
  #print(i)
  print(idx[i])
  lookback_returns <- returns.data[(i-lback):i,]
  list_cov[[as.character(idx[i])]] <- cov(lookback_returns)  
  list_wts[[as.character(idx[i])]] <- optimize.portfolio(R=lookback_returns, 
                        portfolio=portf_minvar, 
                        optimize_method="ROI", 
                        trace=TRUE)$weights
  
}

weights <- do.call(rbind,
        list_wts)

portfolio_returns <-  (lag(weights,1) * returns.data[lag(weights,1) %>%  index] ) %>%  
  rowSums %>% t %>% t
colnames(portfolio_returns) <-  'returns'

portfolio_returns[is.na(portfolio_returns)] <- 0



#Return.cumulative(portfolio_returns, geometric = TRUE)

equity_curve <- cumprod(portfolio_returns +1)
equity_curve %>%  plot(type = 'l')
#df3 %>%  mean(na.rm=T)*12
#df3 %>%  sd(na.rm=T)*sqrt(12)

spy_rescaled <- rescale_equity(data$SPY$SPY.Adjusted)
plot(spy_rescaled)
spy_rescaled <- spy_rescaled[endpoints(spy_rescaled,on='months')]

merge(spy_rescaled, model$equity_curve)

#  ------------------------------------------------------------------------
# roll it into once function
#  ------------------------------------------------------------------------
#source("\\R\\utility_functions.R")
# to do:
# Max diversification
# inverse vol
# get stats

model <- backtest_portfolio_optimization(list_symbols = c("SPY","EFA","EEM","TLT","GLD"),
                                         from = '2010-01-01',
                                         to = '2017-08-31',
                                         freq = 'months')

as.data.frame(model$equity_curve, row.names = index(portfolio_returns))
# Get data
list_symbols <- c("SPY","EFA","EEM","TLT","GLD") 
data <- new.env()
getSymbols(list_symbols,env = data, auto.assign = TRUE)

# Assign to dataframe
# Get adjusted prices
#prices.data <- merge.zoo(MSFT[,6], SBUX[,6], IBM[,6], AAPL[,6], GSPC[,6], AMZN[,6])
#map(data, merge)

from = '2016-01-01'
to = '2017-08-31'
freq <- 'months'



#  ------------------------------------------------------------------------

model <- backtest_portfolio_optimization(from = '2010-01-01')

# plot
model$returns %>% 
  left_join(model$bench) %>% 
  select(index, equity, bench_equity)  %>% 
  ggplot(aes(index),size=1) +
  geom_line(aes(y = equity, color="portfolio")) +
  geom_line(aes(y = bench_equity, color="benchmark")) 
  



#  ------------------------------------------------------------------------


model$returns %>% nrow 
model$equity_curve %>% length

rownames(data.frame(model$equity_curve)) <- rownames(model$returns)


# yes this works! get the cum returns given returns
df <- tk_tbl(model$returns) 
df %>% 
  mutate(equity = cumprod(returns +1))

cumprod(portfolio_returns +1)
cumprod(model$returns +1)
x <- model$returns

data.frame(
  
)
