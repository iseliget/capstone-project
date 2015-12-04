# This file contains regression interface and functions

# Regression interface
regress = function(X_train, y_train, X_test, method) {
	if (method == 'lm') {
		linear_reg(X_train, y_train, X_test);
	}
	else if (method == 'knn') {
		knn_reg(X_train,y_train,X_test,k=10);
	}
	else if (method == 'ridge') {
		ridge_reg(X_train,y_train,X_test);
	}
}

linear_reg = function(X_train, y_train, X_test) {
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

knn_reg = function(X_train, y_train, X_test, k) {
	knnObj = knn.reg(train=X_train, test=X_test, y=y_train, k);
	y_hat = knnObj$pred;

	return (y_hat);
}

ridge_reg = function(X_train, y_train, X_test) {
	colnames(y_train) = 'y';
	both = data.frame(X_train, y=y_train);
	ridgeObj = lm.ridge(y~.,data=both,lambda=5);
	coefficients = ridgeObj$coef;
	y_hat = X_test %*% coefficients;

	return (y_hat);
}