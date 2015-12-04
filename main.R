# Load libraries
library('FNN');
library('MASS'); # packages used for ridge regression


# Load header files
source('dimred_functions.r');
source('reg_functions.r');
source('misc_functions.r');


# loading data...
exchange_rate = read.csv('exchange_rate.csv',sep=',',header=TRUE, na.strings='');
exchange_rate = exchange_rate[,2:19];
# print(dim(exchange_rate));
# print(is.numeric(exchange_rate));

exchange_rate = na.omit(exchange_rate);
# print(dim(exchange_rate));
# print(is.numeric(exchange_rate));

exchange_rate = as.matrix(exchange_rate);
# print(dim(exchange_rate));
# print(is.numeric(exchange_rate));
tickers = c("AUD","BRL","CAD","CNY","EUR","INR","JPY","KRW","MXN","NZD","NOK","RUB","SGD","ZAR","SEK","CHF","GBP","USD");

xrate = exchange_rate[,1:18]; 
# don't forget to transfor the original matrix into the return matrix!
# I forgot to do that last night and my sharpe ratio was like 300 and I was so happy :-D
xrate = log(xrate[2:nrow(xrate),] / xrate[1:(nrow(xrate)-1),]);


main = function() {
	#parameters
	dimred_method = 'pca';
	reg_method = 'lm';

	# set up matrices to record performance
	daily_pnl = matrix(data=NA,nrow=(nrow(xrate)-101),ncol=18);
	colnames(daily_pnl) = tickers;

	stats = matrix(data=NA,nrow=18,ncol=4);
	rownames(stats) = tickers;
	colnames(stats) = c('average_pnl','yearly_pnl','total_pnl','sharpe');

	# calculation and generate daily_pnl
	for (i in 101:(nrow(xrate)-1)) {
		X_lowdim = dimreduc(xrate[(i-100):i,],dimred_method); # first reduce the dimension
		for (j in 1:18) { #loop through all currencies
			y_hat = regress(X_lowdim[1:100,], xrate[(i-99):i,j,drop=FALSE], X_lowdim[101,,drop=FALSE],reg_method);
			daily_pnl[i-100,j] = sign(y_hat) * xrate[i+1,j];
		}
	}

	# calculate performance statistic
	calculate_performance(daily_pnl,stats);
}

ridge = function() {
	# set up matrices to record performance
	daily_pnl = matrix(data=NA,nrow=(nrow(xrate)-101),ncol=18);
	colnames(daily_pnl) = tickers;

	stats = matrix(data=NA,nrow=18,ncol=4);
	rownames(stats) = tickers;
	colnames(stats) = c('average_pnl','yearly_pnl','total_pnl','sharpe');

	# calculation and generate daily_pnl
	for (i in 101:(nrow(xrate)-1)) {
		for (j in 1:18) {
			y_hat = regress(xrate[(i-100):(i-1),],xrate[(i-100):(i-1),j,drop=FALSE],xrate[i,,drop=FALSE],'ridge');
			daily_pnl[i-100,j] = sign(y_hat) * xrate[i+1,j];
		}
	}

	calculate_performance(daily_pnl,stats);
}

residualpredict = function() {
	# set up matrices to record performance
	daily_pnl = matrix(data=NA,nrow=(nrow(xrate)-101),ncol=18);
	colnames(daily_pnl) = tickers;

	stats = matrix(data=NA,nrow=18,ncol=4);
	rownames(stats) = tickers;
	colnames(stats) = c('average_pnl','yearly_pnl','total_pnl','sharpe');

	# calculation and generate daily_pnl
	for (i in 101:(nrow(xrate)-1)) {
		X_lowdim = dimreduc(xrate[(i-100):(i-1),],'pca'); # dimension of 100 * 5
		for (j in 1:18) {
			y_hat_temp = regress(X_lowdim[1:100,], xrate[(i-99):i,j,drop=FALSE], X_lowdim[1:100,],'lm'); 
			residual = y_hat_temp - xrate[(i-99):i,j,drop=FALSE]; # there will be 100 residuals
			y_hat = regress(residual[1:99,1,drop=FALSE], xrate[(i-98):i,j,drop=FALSE], residual[100,1,drop=FALSE],'lm');
			daily_pnl[i-100,j] = sign(y_hat) * xrate[i+1,j];
		}
	}
	
	calculate_performance(daily_pnl,stats);
}