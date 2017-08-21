library(PerformanceAnalytics)
library(stockPortfolio)
library(formattable)

Start_date = "2017-2-13"
End_date = "2017-4-20"

global <- getReturns(c("ECH","ARGT","EWD","EWG","ERUS","MCHI","EWH","EWY","EWA","UAE","KSA","SPY","EWC"), 
                     freq="day", 
                     start = Start_date, end = End_date)

global_benchmark_raw <- getReturns("ACWI", 
                                   freq="day", 
                                   start = Start_date, end = End_date) 

global_weights <- c(0.015,0.015,0.047,0.047,0.047,0.055,0.055,0.055,0.055,0.015,0.015,0.355,0.225) 
global <- global$R
global_benchmark <- global_benchmark_raw$R  
global_cumulative_country <- Return.cumulative(global, geometric = TRUE)


#All the portfolio Metric
global_daily_returns     <- Return.portfolio(global, global_weights, geometric = TRUE)
global_StdDev            <- StdDev.annualized(global_daily_returns, scale = 252)
global_cumulative_return <- Return.cumulative(global_daily_returns, geometric = TRUE)
global_annualized_return <- Return.annualized(global_daily_returns, scale = 252)
global_beta              <- CAPM.beta(global_daily_returns, global_benchmark, Rf = 0)
global_alpha             <- CAPM.alpha(global_daily_returns, global_benchmark, Rf = 0)
global_maxDrawdown       <- maxDrawdown(global_daily_returns, geometric = TRUE)
global_SharpeRatio       <- SharpeRatio(global_daily_returns, Rf=0, FUN="StdDev")
global_TreynorRatio      <- TreynorRatio(global_daily_returns, global_benchmark, Rf = 0, scale = 252)
global_InformationRatio  <- InformationRatio(global_daily_returns, global_benchmark, scale = 252)
global_Sortino           <- SortinoRatio(global_daily_returns)
global_Kelly             <- KellyRatio(global_daily_returns)
global_NetSelectivity    <- NetSelectivity(global_daily_returns, global_benchmark)

#Create Chart and Graph
df <- data.frame(Global_Fund = global_daily_returns, ACWI = global_benchmark)
df2 <- data.frame(global, AWCI = global_benchmark)

table.AnnualizedReturns(global_daily_returns, scale = 252)
charts.PerformanceSummary(R=df, main = "Performance Summary of the Global Fund", colorset = redfocus)
chart.RiskReturnScatter(R=df, main = "Annualized Return and Risks - The Global Fund", colorset = redfocus) 
chart.CumReturns(global, main = "The Global Fund", legend.loc = "left")
chart.RiskReturnScatter(R=df2, main = "Annualized Return and Risks - The Global Fund", colorset = rainbow6equal)



DF <- data.frame(Region=c("South America", "", 
                          "Europe", "", "",
                          "Asia and Australia", "", "", "", 
                          "Middle East and Africa", "", 
                          "North America", ""),
                 Country=c("Chile",
                           "Argentina",
                           "Sweden",
                           "Germany",
                           "Russia",
                           "China",
                           "Hong Kong",
                           "Korea",
                           "Australia",
                           "UAE",
                           "Saudi Arabia",
                           "the U.S.",
                           "Canada"),
                 Change=percent(global_cumulative_country["Cumulative Return",]))

formattable(DF, list(
  Change=formatter(
    "span",
    style = x ~ style(color = ifelse(x < 0 , "red", "green")),
    x ~ icontext(ifelse(x < 0, "arrow-down", "arrow-up"), x)))
)





DF2 <- data.frame(Portfolio = "The Global Fund",
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
                  
                  Value=accounting(c(global_annualized_return,
                                     global_StdDev,
                                     global_beta,
                                     global_alpha,
                                     global_maxDrawdown,
                                     global_SharpeRatio,
                                     global_TreynorRatio,
                                     global_InformationRatio,
                                     global_Sortino,
                                     global_Kelly,
                                     global_NetSelectivity),  digits = 2L))

formattable(DF2)





