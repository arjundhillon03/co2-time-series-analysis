#' ---
#' title: Time Series Week NNN
#' author: Arjun Dhillon
#' date: 05/03/2025
#' ---


# CO2 Dataset -------------------------------------------------------------
#Creating a dataframe with CO2 values and corresponding dates
co2.df = data.frame(
    ds=zoo::as.yearmon(time(co2)),#converting time formate to year-month
    y=co2)
m_co2 = prophet::prophet(co2.df)#Fitting prophet model to CO2

?co2
## Forecast CO2 ------------------------------------------------------------
#Forecasting the data frame using prophet function
#In this case 'period = 8' which means 8 quarters as the 'freq = quarter'
forecast_8quarter = prophet::make_future_dataframe(m_co2, periods=8, freq="quarter")
#Generating predictions
plot_8quarter = predict(m_co2, forecast_8quarter)
#Plotting the forecasted results along with the original data frame
plot(m_co2,plot_8quarter)

#Repeating for 4 year prediction
forecast_4years = prophet::make_future_dataframe(m_co2, periods=4, freq="year")
plot_4years = predict(m_co2, forecast_4years)
plot(m_co2,plot_4years)

#Repeating for 10 year prediction
forecast_10years = prophet::make_future_dataframe(m_co2, periods=10, freq="year")
plot_10years = predict(m_co2, forecast_10years)
plot(m_co2,plot_10years)


## Linearity ---------------------------------------------------------------
# Fit a simple linear model: CO2 ~ time
lm_fit <- lm(y ~ ds, data = co2.df)

summary(lm_fit)




## Trend/Seasonality CO2 ---------------------------------------------------------

#Use the prophet_plot_components function to see the trend plots
#and seasonality plots for each forecast
prophet::prophet_plot_components(m_co2, plot_8quarter)
prophet::prophet_plot_components(m_co2, plot_4years)
prophet::prophet_plot_components(m_co2, plot_10years)


# Temperature Dataset -----------------------------------------------------
#Below is a data set I got online

## R Scratchpad ------------------------------------------------------------
install.packages("prophet")
my_data <- read.csv("data/3950847.csv")
install.packages("tidyverse")

## CSV File/DataFrame Creation ----------------------------------------------------------------
df <- read.csv("data/3950847.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
df$ds <- as.Date(paste(df$DATE, "2010"), format = "%d-%b %Y")
df$y <- df$DLY.TAVG.NORMAL
temp_df <- df[, c("ds", "y")]
temp_df

## Cleaning/Fixing Data -----------------------------------------------------------
df[is.na(df$ds), "DATE"]
df <- read.csv("data/3950847.csv", header = TRUE, sep = ",", quote = "\"", stringsAsFactors = FALSE)
df <- df %>%
    separate_rows(DATE, sep = " ")
df$ds <- as.Date(paste0(df$DATE, "-2010"),format = "%b-%d-%Y")
df$y <- df$DLY.TAVG.NORMAL
df <- subset(df, !is.na(ds))
CleanTemp_df <- df[, c("ds","y")]
CleanTemp_df






## Forecasting Clean Data----------------------------------------------------------
m <- prophet::prophet(CleanTemp_df)

Forecast_6week = prophet::make_future_dataframe(m, periods=6, freq="week")
plot_6week = predict(m, Forecast_6week)
plot(m, plot_6week)

Forecast_3month = prophet::make_future_dataframe(m, periods=3, freq="month")
plot_3month = predict(m, Forecast_3month)
plot(m, plot_3month)

Forecast_6month = prophet::make_future_dataframe(m, periods=6, freq="month")
plot_6month = predict(m, Forecast_6month)
plot(m, plot_6month)

Forecast_1year = prophet::make_future_dataframe(m, periods=1, freq="year")
plot_1year = predict(m, Forecast_1year)
plot(m, plot_1year)

Forecast_5year = prophet::make_future_dataframe(m, periods=5, freq="year")
plot_5year = predict(m, Forecast_5year)
plot(m, plot_5year)


## Trend/Seasonality Plots -------------------------------------------------

prophet::prophet_plot_components(m_co2, plot_6week)
prophet::prophet_plot_components(m_co2, plot_3month)
prophet::prophet_plot_components(m_co2, plot_6month)
prophet::prophet_plot_components(m_co2, plot_1year)
prophet::prophet_plot_components(m_co2, plot_5year)

# 4. Etiquette ------------------------------------------------------------
install.packages("astsa")
