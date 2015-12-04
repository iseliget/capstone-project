library('FNN');
library('MASS'); # packages used for ridge regression


# open pdf device
#pdf('output.pdf',width=22,height=12);

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
		linear_regression(X_train, y_train, X_test);
	}
	else if (method == 'knn') {
		knn(X_train,y_train,X_test,k=10);
	}
	else if (method == 'ridge') {
		ridge(X_train,y_train,X_test);
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

knn = function(X_train, y_train, X_test, k) {
	knnObj = knn.reg(train=X_train, test=X_test, y=y_train, k);
	y_hat = knnObj$pred;

	return (y_hat);
}

ridge = function(X_train, y_train, X_test) {
	colnames(y_train) = 'y';
	both = data.frame(X_train, y=y_train);
	ridgeObj = lm.ridge(y~.,data=both,lambda=5);
	coefficients = ridgeObj$coef;
	y_hat = X_test %*% coefficients;

	return (y_hat);
}

#------------FUNCTIONS FOR REGRESSION END---------------#


#------------FUNCTIONS FOR DIMENSION REDUCTION START----------#
dimreduc = function(X_highdim, method) {
	if (method == 'PCA' || method == 'pca') {
		pca(X_highdim);
	}
	else if (method =='kpca' || method == 'kPCA') {
		# print ('Performing kernal PCA......');
		# #kpca(X_train);
		# print ('Kernal PCA done!');
	}
	else if (method == 'difmap') {
		diffusionmap(X_highdim,0.5);
	}
	else if (method == 'laplacian' || method == 'Laplacian') {
		laplacian(X_train);
	}
	else if (method == 'none') {
		return (X_highdim); #basically does nothing
	}
	else {
		# print ('ENTER THE FOLLOWING ARGUMENTS AS A STRING:');
		# print ('Principal Component Analysis: Enter \'pca\'');
		# print ('Kernal PCA: Enter \'kpca\'');
		# print ('Diffusion Map: Enter \'difmap\'');
		# print ('Laplacian Eigenmaps: Enter \'laplacian\'')
	}
}

pca = function(X_highdim) {
	pcaObj = prcomp(X_highdim,scale=TRUE,center=TRUE,retx=TRUE);
	# choose n such that it explains 80% of the variance
	# d = 0; variance_explained = 0.0;
	# while (variance_explained < 0.8) {
	# 	d = d + 1;
	# 	variance_explained = sum((pcaObj$sd[1:d])^2)/sum((pcaObj$sd)^2);
	# };
	q = pcaObj$sdev;
	# print( cumsum(q))
	# print( cumsum(q^2) / sum( q^2) )
	cumProp = cumsum(q^2) / sum(q^2);
	d = min( which ( cumProp > 80/100 ) );

	dim_reduc_matrix = pcaObj$rotation[,1:d];
	X_lowdim = X_highdim %*% dim_reduc_matrix;

	return (X_lowdim);
}

diffusionmap = function(X_highdim, alpha=0.75) {

	#Create Gaussian kernel matrix
	DIST = dist(X_highdim, method = "euclidean", diag = FALSE, upper = TRUE, p = 2);
	DIST = as.matrix(DIST);
	K = DIST^2;
	K = (-1/alpha)*K;
	K = exp(K);

	#Create diffusion matrix. Recall that diffusion matrix P = D^(-1) %*% K
	#Where D is diagonal consisting row-sums of K
	D = matrix(data=0,nrow=dim(K)[1],ncol=dim(K)[2]);
	for (i in 1:dim(K)[1]) {
		D[i,i] = sum(K[,i]);
	}

	#Create matrix P
	P = solve(D) %*% K;#solve calculates the inverse of D

	#Calculate eigenvectors of D
	eigen_P = eigen(P);

	# remember that the first eigenvector is always trivial
	eigenvectors_P = eigen_P$vectors[,2:3];
	eigenvectors_P = as.matrix(eigenvectors_P);

	return (eigenvectors_P);
	#colnames(test) = c('x','y','type');
	#plot = ggplot(test,aes(y,x));
	#print(plot + geom_point(aes(colour=factor(type))));
}

laplacian = function(X_highdim) {
	# construct adjacency matrix using k-nearest neighbor
}

#------------FUNCTIONS FOR DIMENSION REDUCTION END------------#

compute_Sharpe_Ratio = function(x){
	sh = mean(x, na.rm=TRUE) / sd(x, na.rm=TRUE) * sqrt(252);  
	return(sh);
}

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

	# calculate performance statistics
	cum_daily_pnl = apply(daily_pnl,2,cumsum);
	#plot(cum_daily_pnl[,1,drop=FALSE],type='l');
	stats[,'average_pnl'] = apply(daily_pnl,2,mean);
	stats[,'yearly_pnl'] = apply(daily_pnl,2,mean)*252;
	stats[,'total_pnl'] = cum_daily_pnl[dim(cum_daily_pnl)[1],,drop=FALSE];
	stats[,'sharpe'] = apply(daily_pnl,2,compute_Sharpe_Ratio);
	stats= round(stats,5);
	print('Statistics:')
	print(stats);

	# calculate portfolio sharpe ratio
	temp = rowSums(daily_pnl);

	porto_sharpe = compute_Sharpe_Ratio(temp);
	print('Portfolio Sharpe Ratio:')
	print(porto_sharpe);

	# plot portfolio cumulative pnl
	temp3 = cumsum(temp);
	plot(temp3,type='l',main=paste('Cumulative pnl,',dimred_method,reg_method,sep=' '),xlab='Days',ylab='Cumulative pnl');
}

ridge_reg = function() {
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

	# calculate performance statistics
	cum_daily_pnl = apply(daily_pnl,2,cumsum);

	stats[,'average_pnl'] = apply(daily_pnl,2,mean);
	stats[,'yearly_pnl'] = apply(daily_pnl,2,mean)*252;
	stats[,'total_pnl'] = cum_daily_pnl[dim(cum_daily_pnl)[1],,drop=FALSE];
	stats[,'sharpe'] = apply(daily_pnl,2,compute_Sharpe_Ratio);
	stats= round(stats,5);
	print(stats);

	# calculate portfolio sharpe ratio
	temp = rowSums(daily_pnl);
	porto_sharpe = compute_Sharpe_Ratio(temp);
	print('Portfolio Sharpe Ratio:')
	print(porto_sharpe);
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

	#calculate performance statistics
	cum_daily_pnl = apply(daily_pnl,2,cumsum);

	stats[,'average_pnl'] = apply(daily_pnl,2,mean);
	stats[,'yearly_pnl'] = apply(daily_pnl,2,mean)*252;
	stats[,'total_pnl'] = cum_daily_pnl[dim(cum_daily_pnl)[1],,drop=FALSE];
	stats[,'sharpe'] = apply(daily_pnl,2,compute_Sharpe_Ratio);
	stats= round(stats,5);

	print(stats);
}
#dev.off();