# Yahoo-stocks
Yahoo-stocks
 We first analyze stocks data and choose indicators that could be significant to our model. 
The data is available for 20 unique dates(quarterly). So we will build a linear regression model each for first 15 quarters.
Now we have 15 sets of coefficients for each 15 dates, we will use ARIMA model to forecast the coefficients for the next 4 quarters
Using these forecasted coefficients, we will predict the Price returns of stocks for the last 4 quarters
Next divide the data into 5 buckets ordered by the actual price returns and the forecasted price returns
Take mean of actual and forecasted price returns and compare the results. This will show the performance of our model.
