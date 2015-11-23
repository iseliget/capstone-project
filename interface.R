library('FNN');


# loading data...
exchange_rate = read.csv('exchange_rate.csv',sep=',',header=TRUE, na.strings='');
exchange_rate = exchange_rate[,2:19];
print(dim(exchange_rate));
print(is.numeric(exchange_rate));

exchange_rate = na.omit(exchange_rate);
print(dim(exchange_rate));
print(is.numeric(exchange_rate));

exchange_rate = as.matrix(exchange_rate);
print(dim(exchange_rate));
print(is.numeric(exchange_rate));
tickers = c("Date","AUD","BRL","CAD","CNY","EUR","INR","JPY","KRW","MXN","NZD","NOK","RUB","SGD","ZAR","SEK","CHF","GBP","USD");

xrate = exchange_rate[,1:18]; 
#JPY 8th column

#> dim(exchange_rate)
#[1] 1460 19
#> test = na.omit(exchange_rate)
#> dim(test)
#[1] 875  19

pca = function(X_highdim, n) {
	pcaObj = prcomp(X_highdim,scale=TRUE,center=TRUE,retx=TRUE);
	dim_reduc_matrix = pcaObj$rotation[,1:n];
	X_lowdim = X_highdim %*% dim_reduc_matrix;

	return (X_lowdim);
}

knn = function(X_train, y_train, X_test) {
	knnObj = knn.reg(train=X_train, test=X_test, y=y_train, k=10);
	y_hat = knnObj$pred;

	return (y_hat);
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

	y_hat = y_hat_obj$fit

	coefficients = summary(regObj)$coefficients;  # if you ever need the actual coefficients (beta_i) i=0,1,...,p
	# print(coefficients)

	return(y_hat)
}

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

main = function() {
	for (i in 101:(nrow(xrate)-1)) {
		X_lowdim = dimreduc(xrate[(i-100):i,],'pca',5);
		y_hat = regress(X_lowdim[1:100,],xrate[(i-99):i,8,drop=FALSE],X_lowdim[101,,drop=FALSE],'lm');
	}
}