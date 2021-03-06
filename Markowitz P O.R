# SYSTEMATISK INVESTERING: MAXIMUM SHARPE PORTFOLIO
# Bettingsektor och Biotech.

install.packages("quantmod")
install.packages("corrplot")
install.packages("PerformanceAnalytics")

library(quantmod)
library(corrplot)
library(PerformanceAnalytics)

Ticker <- read_excel("Ticker.xlsx")
View(Ticker)

library(readxl)
library(timeSeries)
library(quantmod)
library(corrplot)
library(PerformanceAnalytics)

con = gzcon(url('https://github.com/systematicinvestor/SIT/raw/master/sit.gz', 'rb'))
source(con)
close(con)

## H�mtar Ticker

# L�kemedelsbolag med FAS II/III
data <- new.env()
Portfolio <- c("ONCO.ST", "BIOA-B.ST", "OASM.ST", "CAMX.ST", "ORX.ST", "IMMU.ST")
getSymbols(Portfolio, auto.assign = TRUE, from='2017-01-02', env = data)
bt.prep(data, align='keep.all')

#*****************************************************************
# Utv�rderar bolagen sedan 2018-01-02
#*****************************************************************
par(mar=c(5,2,2,3)+0.1)
par(mfrow=c(1,1))
plota.matplot(scale.one(data$prices))

#*****************************************************************
# R�knar ut returns f�r portf�ljen
#*****************************************************************
prices <- data$prices
portfolio.ret <- prices / mlag(prices) - 1
View(portfolio.ret)

#*****************************************************************
#                        Kodar strategier    
#******************************************************************                     

obj = portfolio.allocation.helper(data$prices, 
                                  periodicity = 'months', lookback.len = 20, 
                                  min.risk.fns = list(
                                    EW=equal.weight.portfolio,
                                    MV=min.var.portfolio,
                                    MC=min.corr.portfolio,
                                    MS=max.sharpe.portfolio(),
                                    ERC = equal.risk.contribution.portfolio
                                  )
)

models = create.strategies(obj,data)$models

#*****************************************************************
# Create Report
#******************************************************************    
strategy.performance.snapshoot(models, T, 'Backtesting Asset Allocation Portfolios')

#*****************************************************************
# Create Report
#******************************************************************       
plotbt.custom.report.part2(models$MC)       
plotbt.custom.report.part2(models$MS)       
plotbt.custom.report.part2(models$MV) 


## H�mtar Ticker

# Betting tema
data <- new.env()
Portfolio1 <- c("LEO.ST", "NLAB.ST", "KIND-SDB.ST", "ASPIRE.ST", "EVO.ST", "CTM.ST")
getSymbols(Portfolio1, auto.assign = TRUE, from='2017-01-02', env = data)
bt.prep(data, align='keep.all')

#*****************************************************************
# Utv�rderar bolagen sedan 2017-01-02
#*****************************************************************
par(mar=c(5,2,2,3)+0.1)
par(mfrow=c(1,1))
plota.matplot(scale.one(data$prices))

#*****************************************************************
# R�knar ut returns f�r portf�ljen
#*****************************************************************
prices <- data$prices
portfolio.ret <- prices / mlag(prices) - 1
View(portfolio.ret)

#*****************************************************************
#                        Kodar strategier    
#******************************************************************                     

obj = portfolio.allocation.helper(data$prices, 
                                  periodicity = 'months', lookback.len = 20, 
                                  min.risk.fns = list(
                                    EW=equal.weight.portfolio,
                                    MV=min.var.portfolio,
                                    MC=min.corr.portfolio,
                                    MS=max.sharpe.portfolio(),
                                    ERC = equal.risk.contribution.portfolio
                                  )
)

models = create.strategies(obj,data)$models

#*****************************************************************
# Create Report
#******************************************************************    
strategy.performance.snapshoot(models, T, 'Backtesting Asset Allocation Portfolios: Gambling')

#*****************************************************************
# Create Report
#******************************************************************       
plotbt.custom.report.part2(models$MC)       
plotbt.custom.report.part2(models$MS)       
plotbt.custom.report.part2(models$MV) 