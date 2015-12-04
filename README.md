## Capstone Project of Math 191: Predicting foreign exchange rate

### Methodology
We employ a variety of dimensionality reduction techniques:

1. Principal componenet analysis (PCA)
2. Diffusion map
3. Laplacian eigenmaps

We also utilize different regression techniques:

1. Ordinary multiple linear regression
2. KNN regression
3. Ridge regression

### Evaluation

We use the following four statistics to evaluate the performance of the models:

1. Average profit and loss
2. Total profit and loss
3. Cumulative profit and loss
4. Sharpe ratio for each currency and portfolio

### Interesting problems
- **Number of principal components to keep**

There are two possible ways to do this. The first way is that, for each iteration, choose k such that the top-k principal components explain the 80% of the variance. This is more of a hand-waving way.Another more sophisticated way is to use Marcenko-Pastur distribution. For each iteration, we use Marcenko-Pastur distribution to determine the number of principal components we want to keep. This might turn into a small project by itself.

- **How to choose k in KNN regression**

A rule of thumb that people often use is to let k equal to the square root of the number of training samples. Is there any other way of doing this? A method that has some sort of theoretical support would be favorable, just like Marcenko-Pastur and PCA.

- **Predicting power of residuals**

Given a built model, we can obtain a vector of residuals. Does this vector have any predicting power? This seems to be correlated with the __mean reversion assumption__

- **Better ways of handling `NA` in the dataset**

A naive way is just dropping all the `NA`s in the dataset, which still leaves us with sufficient amount of data to build the models. Is there any other better way of doing this? We can consider linear/non-linear extrapolation.
