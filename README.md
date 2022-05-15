# ANN_EqSelection
This model provides the user with a set of applications used to build up equity portfolios by taking advantage of Artificial Neural Networks. 
Backtesting and performance evaluation systems are also included.

The model consists in six different R scripts, of which three used to construct the final equity portfolio, and the remaining three used for backtesting and
performance evaluation.

# ANN_simulation.R
This is the main script used to train the ANN. It first load all the price and fundamental data from Yahoo Finance, to then compute several indicators, both 
technical and fundamental ones. In total, we have over than 80 indicators, considering that the technical ones are calculated using different time horizons. 
Multicollinearity is not assumed to affect the performance of the neural network. The stocks used are the approx. 1500 component of the iShares MSCI World ETF.
Here, the neural network tries to solve a classification problem: for each stock, it estimates the probability that the first goes into the top 50% stocks with
the highest price increase within the next 30 days.

# ANN_portfolio.R
This code takes as input the ANN previously saved, and it uses its estimations to construct the final portfolio, which is made by 15 equities equally weighted. 
The selection, of the 15 stocks over all the ones expected to be on the top 50% in the next month, is made by sorting them by average daily return over the past
30 days. We then select the top 45. Over those, we build up random 15-stock portfolios, to select the one that maximise its Sharpe Ratio. This operation requires,
under each round, to estimate the variance-covariance matrix, in this case by using a bootstrapping method.

# functions.R
All the scrips make an extensive usage of functions. They are all stored here.

# ANN_hypotetical.R
This code is used to identify the best parameters of the ANN, by generating ANNs on random dates and with random parameters. The dataset is splitted into 
training and test one, and the random ANN is tested against the latter and the results are then saved to compare with the other simulations' results.

# ANN_portfolio_returns.R
Code used to construct and test random portfolios over a one-month time horizon. Estimation date is randomly chosen, and the performance is evaluated against 
its benchmark, for the same date and time horizon.

