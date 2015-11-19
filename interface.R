library('FNN');

knn = function{X_train, y_train, X_test} {
	knnObj = knn.reg(train=X_train, test=X_test, y=y_train, k=10);
	y_hat = knnObj$pred;
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

knn = function(X_train, y_train, X_test) {

}

dimreduc = function(X_highdim, method) {
	if (method == 'PCA' || method == 'pca') {
		# print ('Performing Principal Component Analysis......');
		# #pca(X_train);
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
		# print ('Performing linear regression......');
		linear_regression(X_train, y_train);
		# print ('Linear regression done!');
	}
	else if (method == 'knn') {
		print ('Performing kNN regression with k=10......');
		#knn function here with k=10;
		print ('kNN regression done!');
	}
}