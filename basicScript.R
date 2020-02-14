# load_libraries ----------------------------------------------------------
library("quantmod")
library("dygraphs")
library("data.table")
library("highcharter")

# set_defaults ------------------------------------------------------------

setDTthreads(0L)

# getData -----------------------------------------------------------------


getSymbols(Symbols='AAPL', src="av", output.size="full", adjusted=TRUE, api.key='your API key')

col <- c('AAPL.Open',
         'AAPL.High',
         'AAPL.Low',
         'AAPL.Close')

AAPL %>% hchart()

names(AAPL) <- c("Open","High","Low","Close","Volume","Adjusted")

dat <- iex::last(symbols = c("AAPL"), fields  = c("symbol", "price", "size")) 
