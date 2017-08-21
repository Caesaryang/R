library(PerformanceAnalytics)
library(stockPortfolio)
library(formattable)

Start_date = "2017-3-6"
End_date = "2017-4-20"

US <- getReturns(c("XLY","AMZN","HELE","NYT","XLE","JPM","GS","LHCG","AFAM","LUV","KMT","TXN","AMAT","APOG","PKG","UDR","AVB","GXP"), 
                 freq="day", 
                 start = Start_date, end = End_date) 

US_benchmark_raw <- getReturns("SPY", 
                               freq="day", 
                               start = Start_date,end = End_date) 

US_weights <- c(0.0702,0.0685,0.0258,0.0248,0.0195,0.0841,0.0884,0.0702,0.0697,0.0652,0.0645,0.1289,0.1293,0.014,0.0274,0.0146,0.0148,0.0202) 
US <- US$R
US_benchmark <- US_benchmark_raw$R  
US_cumulative_stock <- Return.cumulative(US, geometric = TRUE)


#All the portfolio Metric
US_daily_returns     <- Return.portfolio(US, US_weights, geometric = TRUE)
US_cumulative_return <- Return.cumulative(US_daily_returns, geometric = TRUE)
US_annualized_return <- Return.annualized(US_daily_returns, scale = 252)
US_StdDev            <- StdDev.annualized(US_daily_returns, scale = 252)
US_beta              <- CAPM.beta(US_daily_returns, US_benchmark, Rf = 0)
US_alpha             <- CAPM.alpha(US_daily_returns, US_benchmark, Rf = 0)
US_maxDrawdown       <- maxDrawdown(US_daily_returns, geometric = TRUE)
US_SharpeRatio       <- SharpeRatio(US_daily_returns, Rf=0, FUN="StdDev")
US_TreynorRatio      <- TreynorRatio(US_daily_returns, US_benchmark, Rf = 0, scale = 252)
US_InformationRatio  <- InformationRatio(US_daily_returns, US_benchmark, scale = 252)
US_Sortino           <- SortinoRatio(US_daily_returns)
US_Kelly             <- KellyRatio(US_daily_returns)
US_NetSelectivity    <- NetSelectivity(US_daily_returns, US_benchmark)


df <- data.frame(US_Fund = US_daily_returns, SP500 = US_benchmark)
df2 <- data.frame(US, SP500 = US_benchmark)

charts.PerformanceSummary(R=df, main = "Performance Summary of the U.S. Fund", colorset = redfocus)
chart.RiskReturnScatter(R=df, main = "Annualized Return and Risks - The U.S. Fund", colorset = redfocus) 
chart.CumReturns(US, main = "The U.S. Fund", legend.loc = "left")

#Portfolio and benchmark Performance
chart.RiskReturnScatter(R=df, main = "Annualized Return and Risks - The U.S. Fund", colorset = redfocus)
#Each stocks' performance
chart.RiskReturnScatter(R=df2, main = "Annualized Return and Risks - The U.S. Fund", colorset = rainbow6equal)



DF <- data.frame(Sector=c("Consumer Discretionary", "", 
                          "Consumer Staples", "", 
                          "Energy", 
                          "Financials", "", 
                          "Health Care", "",
                          "Industrial", "",
                          "Information Technology", "",
                          "Materials", "",
                          "Real Estate", "",
                          "Utilities"),
                 Name=c("Consumer Discret Sel Sect SPDR ETF",
                        "Amazon.com, Inc.",
                        "Helen of Troy Limited",
                        "The New York Times Company",
                        "Energy Select Sector SPDR ETF",
                        "JPMorgan Chase & Co.",
                        "The Goldman Sachs Group, Inc.",
                        "LHC Group, Inc.",
                        "Almost Family, Inc.",
                        "Southwest Airlines",
                        "Kennametal Inc.",
                        "Texas Instruments Inc.",
                        "Applied Materials Inc.",
                        "Apogee Enterprises Inc.",
                        "Packaging Corporation of America",
                        "UDR, Inc.",
                        "AvalonBay Communities, Inc.",
                        "Great Plains Energy Incorporated"),
                 Change=percent(US_cumulative_stock["Cumulative Return",]))

formattable(DF, list(
  Change=formatter(
    "span",
    style = x ~ style(color = ifelse(x < 0 , "red", "green")),
    x ~ icontext(ifelse(x < 0, "arrow-down", "arrow-up"), x)))
)





DF2 <- data.frame(Portfolio = "The U.S. Fund",
                  Metrics=c("Annulized Return",
                            "Standard Deviation", 
                            "CAPM Beta",
                            "CAPM Alpha", 
                            "Max Drawdown",
                            "Sharpe Ratio",
                            "Treynor Ratio",
                            "Information Ratio",
                            "Sortino Ratio",
                            "Kelly Ratio",
                            "NetSelectivity"),
                  
                  Value=accounting(c(US_annualized_return,
                                     US_StdDev,
                                     US_beta,
                                     US_alpha,
                                     US_maxDrawdown,
                                     US_SharpeRatio,
                                     US_TreynorRatio,
                                     US_InformationRatio,
                                     US_Sortino,
                                     US_Kelly,
                                     US_NetSelectivity),  digits = 2L))

formattable(DF2)
