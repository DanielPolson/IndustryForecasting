# Industry Forecasting

All data taken from the Canadian Labour Force Survey: https://www.statcan.gc.ca/en/survey/household/3701

RShiny App to view forecasting: https://t0v3pz-daniel-polson.shinyapps.io/project/

In my analysis, I looked to model and forecast the proportion of the Canadian population employed in each industry. Since the number of surveyed individuals is not constant or necessarily directly related to the total population size, it is more logical to model this way. I chose to use a seasonal ARIMA model for each industry for a number of reasons. The seasonal component is extremely useful for modeling certain industries. Upon inspecting the *Elementary and secondary schools* graph from either my graphs or RShiny link, you can see that employment falls heavily during each summer. Another important reason is that these models can be fit individually to all industries quickly in R, and they tend to be a good fit.

Notable trends and projections show significant increases in proportion of Canadians working in *Computer Systems Design and Related Services*, *Residential Building Construction*, and *Ambulatory Health Care Services*. Notable decreases include *Wood Product Manufacturing*, *Telecommunications*, and *Farms and Support Activities*. A number of other services took sharp turns either way during COVID-19 and are forecasted to continue returning to normal. 

Make sure to save the .pdf's and zoom in to each graph for a better viewing experience.
