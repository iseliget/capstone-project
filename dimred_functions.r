# This file contains dimension reduction interface and functions

# Dimension reduction interface
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