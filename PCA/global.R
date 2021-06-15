## --- func that takes matrix and #factors as inputs & outputs recovery
factor_recover <- function(inp_matrix,    # input original matrix
                           k){            # k = num of dimensions to reduce inp_matrix to
  
  std_mat = scale(inp_matrix)
  pc1 = princomp(std_mat, scores = TRUE)
  
  reco_mat = pc1$scores[, 1:k] %*% t(pc1$loadings[,1:k])
  
  return(list(reco_mat, std_mat))
  
} # func ends

## --- define func to RMSE calculation
rmse_calc <- function(inp_matrix, k){     # same inputs as with factor_recover()
  
  a0 = factor_recover(inp_matrix, k); a0     # invoking a user-defined func inside a user-defined func. :)
  error_mat = a0[[1]] - a0[[2]]
  error_sq = error_mat * error_mat       
  rmse = sqrt(mean(error_sq))
  
  return(rmse)
}    # func ends


## --- func that wraps and loops over the other 2
build_rmse_mat <- function(inp_mat){
  
  # drop non-numeric-able colms
  a0 = apply(inp_mat, 2, function(x) {as.numeric(x)})
  a01 = apply(as.data.frame(a0[1,]), 2, function(x){!is.na(x)}); a01
  a1 = which(a01); a1
  inp_mat1 = inp_mat[, a1] %>% apply(., 2, function(x) as.numeric(x)); 
  # head(inp_mat1)
  std_A = scale(inp_mat1)
  
  # drop NaNs due to const colms
  a0 = apply(std_A, 2, function(x) {is.nan(x)}); dim(a0)      
  a1 = which(a0[1,]); as.numeric(a1)
  if (length(a1) > 0) {   std_A = std_A[,-a1] }
  
  # now apply funcs
  n = ncol(std_A); n
  rmse_mat = matrix(1:n, n, 2); head(rmse_mat)
  colnames(rmse_mat) = c("num_dimensions", "RMSE")
  
  for (i in 1:n){
    rmse_mat[i, 2] = round(rmse_calc(std_A, i), 3)
  }
  
  rmse_mat = as.data.frame(rmse_mat) 
  rownames(rmse_mat) = colnames(std_A)
  return(list(rmse_mat, std_A))
} # func ends

## --- func to calc % variance explained by each PCA compt
var_expl <- function(X){
  
  X2 = scale(X) #New scaled dataset with mean of 0 and sd of 1
  total.var <-sum(diag(cov(X2))); total.var # Calc total variance in scaled data
  
  e.cor<-eigen(cor(X)) #For the correlation matrix (i.e., scaled and centered)
  # e.cov<-eigen(cov(X))#For the covariance matrix
  eigenvalues<-e.cor$values #or eigenvalues<-e.cov$values for the unscaled data
  eigenvectors<-e.cor$vectors
  scores<-X2%*%eigenvectors #New Variables
  
  # Create empty vectors
  prop.var <- rep(NA,ncol(X))
  cum.var<-rep(NA,ncol(X)) 
  
  #Calculate proportion of variance explained and cumulative variance explained
  for (i in 1:ncol(X)) { prop.var[i] <- var(scores[,i])/total.var }
  for (i in 1:ncol(X)) {cum.var[i] <- sum(prop.var[1:i]) }
  
  varExpl = data.frame(component=seq(1:ncol(X)), compt.var=round(prop.var,4)*100, cumul.var=round(cum.var,4)*100); head(varExpl)
  return(varExpl)
} # func ends

### --- func to display 2D-biplot in PCA scores tab
build_display_biplot <- function(inp_mat){
  
  # build a ggplotly biplot
  inp_df = as.data.frame(inp_mat)
  a0 = apply(inp_df, 2, function(x) {is.numeric(x)})
  a1 = as.numeric(which(a0))
  inp_df1 = inp_df[, a1]
  pc1 = princomp(inp_df1, scores = TRUE)
  pc2 = as.data.frame(pc1$scores[,1:2])
  if (is.null(rownames(inp_df1))) {rownames(inp_df1) = seq(1:nrow(inp_df1))}
  rownames(pc2) = rownames(inp_df1)
  
  plot2 = ggplot(data=pc2, aes(x=pc2[,1], y=pc2[,2])) + 
    geom_point(aes(text=rownames(pc2)), colour="blue") +
    xlab("PCA compt1") + ylab("PCA compt2") + ggtitle("PCA 2-D plot")
  
  fig1 = ggplotly(plot2)
  fig1 # Yay
  
} # func ends


pca_outputs <- function(df0){
  
  pc1 = df0 %>% princomp(., scores = TRUE)
  
  loadings = pc1$loadings %>% as.matrix(.) %>% round(.,3); #head(loadings)
  #print(loadings[, 1:k])   # show in PCA loadings tab
  scores = pc1$scores %>% round(.,3)
  #print(scores[1:20, 1:k])  # show in PCA scores tab
  return(list(loadings,scores))
} # func ends



# func to build heatmap, for displayt in PCA loadings tab
build_pca_loadings_heatmap <- function(df0, k=2){
  
  pc1 = princomp(df0, scores=TRUE)
  loadings = pc1$loadings %>% as.matrix() %>% round(.,3)   
  
  a0 = expand.grid(rownames(loadings[,1:k]), colnames(loadings[,1:k])); dim(a0)
  a1 = as.vector(unlist(loadings[,1:k]))
  a0$Z = a1; #head(a0)
  
  ## Heatmap 
  p0 <- ggplot(a0, aes(Var2, Var1, fill= Z)) + 
    geom_tile() +
    #scale_fill_gradient(low="red", high="blue") 
    scale_fill_viridis(discrete=FALSE)
  
  fig = ggplotly(p0)
  fig
  
}