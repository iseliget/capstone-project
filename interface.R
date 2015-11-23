library('FNN');


# open pdf device
pdf('output.pdf',width=22,height=12);

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


#------------FUNCTIONS FOR REGRESSIONS START------------#
regress = function(X_train, y_train, X_test, method) {
	if (method == 'lm') {
		#print ('Performing linear regression......');
		linear_regression(X_train, y_train, X_test);
		# print ('Linear regression done!');
	}
	else if (method == 'knn') {
		print ('Performing kNN regression with k=10......');
		#knn function here with k=10;
		print ('kNN regression done!');
	}
}

linear_regression = function(X_train, y_train, X_test) {
	# Training X matrix: X_train  of size n by p
	# Training y vector: y_train  of size n by 1
	# Test X matrix: X_test  of size k by p

	# Performs multiple linear regression (without an intercept) on the training data, and applied the obtained coefficients to a test matrix.

	colnames(y_train) = 'y'

	both = data.frame(X_train, y  = y_train);

	regObj = lm(y ~ . , data = both)  # with intercept  beta_0

	# regObj = lm(y ~ . +0, data = both)  # without intercept  beta_0

	# print(summary( regObj ) );   # see the summary of the regression

	rsq = summary( regObj )$r.squared;		# print( paste('R_square= ', rsq) );

	# fit_residuals = resid( regObj );  # if you ever need the residuals from the training fit

	# print(X_test)
	# print(data.frame(X_test) )

	# apply the beta's to X_test and compute your prediction:
	X_test = data.frame(X_test);

	y_hat_obj = predict(regObj, newdata = X_test, se.fit = TRUE)

	y_hat = y_hat_obj$fit;

	coefficients = summary(regObj)$coefficients;  # if you ever need the actual coefficients (beta_i) i=0,1,...,p
	# print(coefficients)

	return(y_hat)
}

knn = function(X_train, y_train, X_test) {
	knnObj = knn.reg(train=X_train, test=X_test, y=y_train, k=10);
	y_hat = knnObj$pred;

	return (y_hat);
}
#------------FUNCTIONS FOR REGRESSION END---------------#


#------------FUNCTIONS FOR DIMENSION REDUCTION START----------#
dimreduc = function(X_highdim, method, n) {
	if (method == 'PCA' || method == 'pca') {
		pca(X_highdim,n);
		# print ('Principal Component Analysis done!');
	}
	else if (method =='kpca' || method == 'kPCA') {
		# print ('Performing kernal PCA......');
		# #kpca(X_train);
		# print ('Kernal PCA done!');
	}
	else if (method == 'difmap') {
		# print ('Performing Diffusion Map......');
		# #difmap(X_train);
		# print ('Diffusion Map done!');
	}
	else if (method == 'laplacian' || method == 'Laplacian') {
		# print ('Performing Laplacian Eigenmaps......');
		# #laplacian(X_train);
		# print ('Laplacian Eigenmaps done!');
	}
	else {
		# print ('ENTER THE FOLLOWING ARGUMENTS AS A STRING:');
		# print ('Principal Component Analysis: Enter \'pca\'');
		# print ('Kernal PCA: Enter \'kpca\'');
		# print ('Diffusion Map: Enter \'difmap\'');
		# print ('Laplacian Eigenmaps: Enter \'laplacian\'')
	}
}

pca = function(X_highdim, n) {
	pcaObj = prcomp(X_highdim,scale=TRUE,center=TRUE,retx=TRUE);
	dim_reduc_matrix = pcaObj$rotation[,1:n];
	X_lowdim = X_highdim %*% dim_reduc_matrix;

	return (X_lowdim);
}
#------------FUNCTIONS FOR DIMENSION REDUCTION END------------#

compute_Sharpe_Ratio = function(x){
	sh = mean(x, na.rm=TRUE) / sd(x, na.rm=TRUE) * sqrt(252);  
	return(sh);
}

main = function() {
	# set up matrices to record performance
	daily_pnl = matrix(data=NA,nrow=(nrow(xrate)-101),ncol=18);
	colnames(daily_pnl) = tickers;

	stats = matrix(data=NA,nrow=18,ncol=4);
	rownames(stats) = tickers;
	colnames(stats) = c('average_pnl','yearly_pnl','total_pnl','sharpe');

	#calculation and generate daily_pnl
	for (i in 101:(nrow(xrate)-1)) {
		X_lowdim = dimreduc(xrate[(i-100):i,],'pca',5); # first reduce the dimension
		for (j in 1:18) { #loop through all currencies
			y_hat = regress(X_lowdim[1:100,],xrate[(i-99):i,j,drop=FALSE],X_lowdim[101,,drop=FALSE],'lm');
			daily_pnl[i-100,j] = sign(y_hat) * xrate[i+1,j];
		}
	}

	print(head(daily_pnl));

	#calculate performance statistics
	stats[,'average_pnl'] = apply(daily_pnl,2,mean);
	stats[,'yearly_pnl'] = apply(daily_pnl,2,mean)*252;
	#stats[,'total_pnl'] = apply(daily_pnl,2,cumsum);
	stats[,'sharpe'] = apply(daily_pnl,2,compute_Sharpe_Ratio);

	print(stats);
}

dev.off();