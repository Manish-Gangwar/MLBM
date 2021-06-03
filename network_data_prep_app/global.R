  ## --- functionize dataframe to adjacency mat for network-an app input 
  df2adjacency <- function(input_df, cutoff_percentile=0.25,id_var){
    rownames(input_df) <-  make.names(input_df[,id_var], unique=TRUE)
    #rownames(input_df) <- input_df[,id_var]
    input_df[,id_var] <- NULL
    # first, retain only metric colms
    a0_logi = apply(input_df, 2, function(x) {is.numeric(x)}); a0_logi
    df0 = input_df[, a0_logi]
    n1 = nrow(df0); n1
    
    # calc dist mat
    dist_mat = dist(as.matrix(scale(df0)))
    
    # build full square-shaped dist matrix
    full_dmat = matrix(0, n1, n1)
    diag(full_dmat) = 1  # created shell matrix
    
    counter0 = 1
    for (i in 1:(n1-1)){
      
      counter1 = counter0 + (n1 - i) - 1; counter1
      vals = dist_mat[counter0: counter1]; vals
      full_dmat[(i+1):n1, i] = vals; full_dmat
      counter0 = counter1+1; counter0
      
    } # 0.01s for 32x32 mat
    
    # populate upper triangular part also
    for (row0 in 1:(n1-1)){
      for (colm0 in (row0+1):n1){
        full_dmat[row0, colm0] = full_dmat[colm0, row0] } }  
    
    ## set threshold in percentile terms (can be slider based widget in shiny app)
    #thresh0=0.25  # say. top 25% closest rows only form links
    thresh1 = quantile(unlist(full_dmat), cutoff_percentile,na.rm = TRUE) %>% as.numeric(); thresh1
    adj_mat = map_dfc(full_dmat, function(x) {1*(x < thresh1)}) # 0.03s
    adj_mat1 = matrix(adj_mat, n1, n1) # adj_mat1[1:8,1:8]
    
    rownames(adj_mat1) = rownames(input_df)
    colnames(adj_mat1) = rownames(input_df)
    
    return(adj_mat1)
    
  } # func ends
  
  # test-drive above
  # system.time({ adj0 = df2adjacency(input_df, 0.33,"car") }) # 0.05s
  # adj0[1:8,1:8] # view a few
  
  summry_df <- function(df){
    # select data type
    summ_df <- data.frame()
    summ_df <- df %>% 
      summarise_all(class) %>% 
      gather() %>% rename("Variable"="key","Datatype"="value")
    mean <- df %>%
      summarise(across(where(is.numeric), mean, na.rm= TRUE))%>%t()
    sd <- df %>%
      summarise(across(where(is.numeric), sd, na.rm= TRUE))%>%t()
    min <- df %>%
      summarise(across(where(is.numeric), min, na.rm= TRUE))%>%t()
    max <- df %>%
      summarise(across(where(is.numeric), max, na.rm= TRUE))%>%t()
    mode <- sapply(df,function(x) getmode(x))
    
    t1 <- as.data.frame(cbind(mean,sd,min,max,mode))
    names(t1) <- c('Mean',"SD","Min","Max","Mode")
    t2 <- tibble::rownames_to_column(t1, "Variable")%>% mutate_if(is.numeric, round, digits=2)
    t3 <- left_join(summ_df,t2,by=c("Variable"))
    return(t3)
  }
  
  getmode <- function(v) {
uniqv <- unique(v)
uniqv[which.max(tabulate(match(v, uniqv)))]
}

  