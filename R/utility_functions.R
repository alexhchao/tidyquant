



backtest_portfolio_optimization <- function(list_symbols = c("SPY","EFA","EEM","TLT","GLD"),
                                            from = '2016-01-01',
                                            to = '2017-08-31',
                                            freq = 'months',
                                            benchmark = 'SPY')
{
  
  my_list <- list()
  for (ticker in list_symbols)
  {
    my_list[[ticker]] <- get_adj_close_from_yhoo(ticker,
                                                 from,
                                                 to,
                                                 freq=freq)
  }
  
  # benchmark
  bench <- get_adj_close_from_yhoo(benchmark,
                                   from,
                                   to,
                                   freq=freq)
  colnames(bench) <- 'bench_returns'
  bench_returns <- ROC(bench,1)
  
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
  
  list_cov = list()
  list_wts <- list()
  lback <-  12
  idx <-  returns.data %>%  index
  for (i in seq(lback+1,nrow(returns.data)))
  {
    #print(i)
    print(idx[i])
    lookback_returns <- returns.data[(i-lback):i,]
    list_cov[[as.character(idx[i])]] <- cov(lookback_returns)  # regular historical cov matrix
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
  
  portfolio_returns.tbl <- tk_tbl(portfolio_returns) 
  portfolio_returns.tbl <- portfolio_returns.tbl %>% 
    mutate(equity = cumprod(returns +1))
  
  bench.tbl <- tk_tbl(bench_returns) 
  bench.tbl <- bench.tbl %>% 
    filter(index >= portfolio_returns.tbl$index[1])
  bench.tbl[1,2] <- 0 # hack, must be easier way to set this to 0
  bench.tbl <- bench.tbl %>% 
    mutate(bench_equity = cumprod(bench_returns +1))
  
  out <- list()
  #out$equity_curve <- cumprod(portfolio_returns +1)
  out$returns <- portfolio_returns.tbl
  out$weights <- weights
  out$bench <- bench.tbl
  
  out
}



get_adj_close_from_yhoo <- function(ticker, from_date, to_date, freq='days',lag_days=0)
{
  from_date <- as.Date(from_date)
  to_date   <- as.Date(to_date)
  
  spy_price <- getSymbols(ticker, src = 'yahoo', from = from_date, to = to_date, auto.assign = F)
  if (lag_days >0)
    spy_price <- lag(spy_price,lag_days)
  output <- spy_price[,6]
  # ADD NEW PARAMETER, WEEKS, MONTHS, YEARS
  output[endpoints(output,on=freq)]
  #convert_xts_to_df(  )    
  # convert_xts_to_df( spy_price[,6] )
}


returns2price <- function(returns,initialwealth=100)
{
  returns <- convert_mat_to_xts(returns)
  x <- returns
  x[1]=0 # zero out first column since we start here
  #x <- na.locf(x) # THIS IS WRONG, DONT CARRY FORWARD MISSING RETURNS
  x[is.na(x)]<- 0 # UPDATED 5-17, SET NA TO 0
  y <- cumprod(x+1)
  #z <- cbind(x,y)
  #colnames(z) <- c('returns','wealth')
  #index(y) <- index(returns)
  y * initialwealth
  
}



convert_xts_to_df <- function(mat.xts)
{
  core_data <- as.data.frame(mat.xts)
  core_data
}

convert_df_to_xts <-function(mat.df)
{
  mat.df <- apply(mat.df, 2, unlist)
  index <- as.Date(as.numeric(rownames(mat.df)),origin="1899-12-30")
  mat.xts <- as.xts(mat.df, index )
  mat.xts
}

convert_mat_to_xts <- function(mat)
{
  
  #convert_df_to_xts(matrix.to.frame( mat ))
  convert_df_to_xts(convert_mat_to_df(mat))
}

convert_mat_to_df <- function( mat )
{
  r = nrow(mat)
  c = ncol(mat)
  data <- as.data.frame( mat[2:r,2:c] )
  
  if (c <= 2)
  {
    data <- t(data)
  }
  
  colnames(data) <- mat[1,2:c]
  rownames(data) <- mat[2:r,1]
  data
}


getstats_from_series <- function(equity,
                                 freq = 'months')
{
  if (freq == 'months')
  {
    n <- 12
  }
  else if (freq == 'days')
  {
    n <- 252
  }
  rets <- ROC(equity,n=1,type='discrete')
  
  cagr <- apply(rets,2, function(x) mean(x, na.rm=T)*sqrt(n))
  vol <- apply(rets,2, function(x) sd(x, na.rm=T)*sqrt(n))
  sharpe <- cagr / vol
  
  #maxDD <- sapply(models, function(x) compute.max.drawdown(x$equity) )
  maxDD <- maxDrawdown(rets)
  stats <- data.frame()
  stats <- rbind(cagr,vol,sharpe,maxDD)
  
  stats
}

# COMPUTE CAGR FROM PRICE SERIES
compute.cagr2 <- function(equity)
{
  as.double( (as.numeric(last(equity,1))/as.numeric(first(equity,1))) ^(1/compute.nyears(equity)) - 1 )
}

compute.nyears <- function(x)
{
  as.double(diff(as.Date(range(index(x)))))/365
}

# RESCALE FUNCTION
rescale_equity <- function(price_series,inital_value = 100)
{
  #price_series <- _to_xts(price_series)
  daily_rets <- ROC(price_series,1,type='discrete')
  daily_rets[1,]<-0
  new_equity <- cumprod(1+daily_rets)*inital_value
  new_equity
  #convert_xts_to_df(new_equity)   
}

