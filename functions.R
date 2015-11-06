dimred = function(X_highdim, num_of_cp) {
    pca_result = prcomp(high_dim_train, scale=TRUE, center=TRUE, rtex=TRUE);
    pc_vectors = pca_result$rotation[,1:num_of_cp];
    X_lowdim = X_highdim %*% pc_vectors;
}