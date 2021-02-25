valfn <- function(x) if(is.null(x) | is.na(x) | x=="") return("Input data is incorrect.")


sample_data <- function(dataset_name){
              # if(dataset_name=='sleep'){
              #   return(VIM::sleep)
              # }
              # if (dataset_name=="diabetes"){
              #   return(VIM::diabetes)
              #   
                get(dataset_name,'package:VIM')
              }
              
#}


data_frame_str <- function(data){
  df_str <- data.frame(variable = names(data),
             class = sapply(data, class),
             first_values = sapply(data, function(x) paste0(head(x),  collapse = ", ")),
             unique_value_count = sapply(data,function(x) length(unique(x))),
             row.names = NULL) 
  return(df_str)
}

display_missing_percentage <- function(data){
  # count total, missing value & its percentage 
  missing.values <- data %>%
    tidyr::gather(key = "key", value = "val") %>%
    mutate(isna = is.na(val)) %>%
    group_by(key) %>%
    mutate(total = n()) %>%
    group_by(key, total, isna) %>%
    summarise(num.isna = n()) %>%
    mutate(pct = num.isna / total * 100)
  
  if(sum(missing.values$isna)==0){
    
    percentage.plot <- missing.values %>%
      ggplot() +
      geom_bar(aes(x = reorder(key, desc(pct)), 
                   y = pct, fill=isna), 
               stat = 'identity', alpha=0.8) +
      #scale_x_discrete(limits = levels) +
      scale_fill_manual(name = "", 
                        values = c('steelblue', 'tomato3'), labels = c("Present", "Missing")) +
      coord_flip() +
      labs(title = "Percentage of missing values", x =
             'Variable', y = "% of missing values")
    return(percentage.plot)
    
  }else{
    
    levels <- (missing.values  %>% filter(isna == T) %>% arrange(desc(pct)))$key
    
    percentage.plot <- missing.values %>%
      ggplot() +
      geom_bar(aes(x = reorder(key, desc(pct)), 
                   y = pct, fill=isna), 
               stat = 'identity', alpha=0.8) +
      scale_x_discrete(limits = levels) +
      scale_fill_manual(name = "", 
                        values = c('steelblue', 'tomato3'), labels = c("Present", "Missing")) +
      coord_flip() +
      labs(title = "Percentage of missing values", x =
             'Variable', y = "% of missing values") 
     
    return(percentage.plot)
  }
  
}


#df<-VIM::sleep
miss_cols <- function(df){
  list_vars = list()
  miss_cols <- sapply(df,function(x) sum(is.na(x)))
  miss_cols <- names(miss_cols[miss_cols!=0])
  miss_cols_class <- sapply(df[miss_cols], function(x) class(x))
  miss_cols_int <- names(miss_cols_class[miss_cols_class=="integer" | miss_cols_class=="numeric"])
  miss_cols_cat <- names(miss_cols_class[miss_cols_class=="character"|miss_cols_class=="factor"])
  
  list_vars[[1]] <- miss_cols_int
  list_vars[[2]] <- miss_cols_cat
  
  return(list_vars)
}



#-----Imputer
imputer <- function(df,num_method,cat_method=NULL,int_cols,cat_cols,original_flag,replacement_ind){
 # print(int_cols)
 # print(cat_cols)
 # print(original_flag)
  df_copy <- df
  return_data <- list()
  
  if(num_method=="mean"){
    if(!is.null(int_cols)){
      replaced_by  <- sapply(df_copy[int_cols],function(x) round(mean(x,na.rm = TRUE),2))
      replaced_by <- as.data.frame(replaced_by)
      colnames(replaced_by) <- c("replaced by")
    }else{
      replaced_by<-NULL
    }
    
    
   # print(replaced_by)
    imputed_df <- sapply(df_copy[int_cols], function(x) impute(x, mean))
    imputed_df <- round(imputed_df,2)
    colnames(imputed_df) <- paste0("imputed_",int_cols)
    imputed_df_final <- cbind(df_copy,imputed_df)
    
    if(original_flag==TRUE){
      return(list(imputed_df_final,replaced_by))
    }else{
      imputed_df_final[,int_cols] <- NULL
      return(list(imputed_df_final,replaced_by))
    }
  
  }
  
  
  if(num_method=="median"){
    if(!is.null(int_cols)){
      replaced_by  <- sapply(df_copy[int_cols],function(x) round(mean(x,na.rm = TRUE),2))
      replaced_by <- as.data.frame(replaced_by)
      colnames(replaced_by) <- c("replaced by")
      
    }else{
      replaced_by<-NULL
    }
   # print(replaced_by)
    imputed_df <- sapply(df_copy[int_cols], function(x) impute(x, median))
    colnames(imputed_df) <- paste0("imputed_",int_cols)
    imputed_df_final <- cbind(df_copy,imputed_df)
    
    if(original_flag==TRUE){
      return(list(imputed_df_final,replaced_by))
    }else{
      imputed_df_final[,int_cols] <- NULL
      return(list(imputed_df_final,replaced_by))
    }
  }
  
  
  if(num_method=="complete_case"){
    # replaced_by  <- sapply(df_copy[int_cols],function(x) median(x,na.rm = TRUE))
    # print(replaced_by)
    # imputed_df <- sapply(df_copy[int_cols], function(x) impute(x, median))
    # colnames(imputed_df) <- paste0("imputed_",int_cols)
    # imputed_df_final <- cbind(df_copy,imputed_df)
    # 
    # if(original_flag==TRUE){
    #   return(list(imputed_df_final,replaced_by))
    # }else{
    #   imputed_df_final[,int_cols] <- NULL
    #   return(list(imputed_df_final,replaced_by))
    
      #} 
    imputed_df <- df_copy[complete.cases(df_copy),]
    dim_after_imp <- as.data.frame(dim(df_copy)[1]-dim(imputed_df)[1])
    rownames(dim_after_imp) <- c("no of rows deleted")
    colnames(dim_after_imp) <- c("count")
    return(list(imputed_df,dim_after_imp))
  }
  
  if(num_method=="knn"){
    imputed_df <- VIM::kNN(df_copy,imp_var = TRUE)
    temp <- imputed_df %>% select(ends_with("_imp"))
    temp_df <- as.data.frame(sapply(temp, function(x) sum(x)))
    colnames(temp_df) <- c("No of values replaced")
    rownames(temp_df) <- str_replace(rownames(temp_df),"_imp","")
    
    if(replacement_ind==TRUE){
      return(list(imputed_df,temp_df))
    }else{
      imputed_df <- imputed_df %>% select(-ends_with("_imp"))
      return(list(imputed_df,temp_df))
    }
    
    
    
  }
  
  
}


#Function 6 : data transformation

MinMaxScaler <- function(colm0, a=0, b=1){
  Max0 = max(colm0); Min0 = min(colm0)
  colm1 = sapply(colm0, function(x) {(x-Min0)*(b-a)/(Max0 - Min0)})
  colm1 = round(colm1,2)
  return(colm1)
}

RobustScaler <- function(colm0){
  q1 = as.numeric(quantile(colm0, 0.25,na.rm = TRUE))
  q3 = as.numeric(quantile(colm0, 0.75,na.rm = TRUE))
  colm1 = sapply(colm0, function(x) {(x - q1)/(q3 - q1)})
  colm1 = round(colm1,2)
  return(colm1)
}

StdScaler <- function(colm0){
  colm1 = scale(colm0)
  colm1 = round(colm1,2)
  return(colm1)
}


data_transform <- function(df,method,cols,original_flag=TRUE){
  if(method=="standard"){
    transformed_df <- as.data.frame(sapply(df[,cols], function(x) StdScaler(x)))
    colnames(transformed_df) <- paste0("std_",cols)
    transfomed_df_final <- cbind(df,transformed_df)
    
    if(original_flag==TRUE){
      return(transfomed_df_final)
    }else{
      transfomed_df_final[,cols] <- NULL
      return(transfomed_df_final)
    }
  }
  
  
  if(method=="minmax"){
    transformed_df <- as.data.frame(sapply(df[,cols], function(x) MinMaxScaler(x)))
    colnames(transformed_df) <- paste0("norm_",cols)
    transfomed_df_final <- cbind(df,transformed_df)
    
    if(original_flag==TRUE){
      return(transfomed_df_final)
    }else{
      transfomed_df_final[,cols] <- NULL
      return(transfomed_df_final)
    }
  }
  
  
  if(method=="robust"){
    transformed_df <- as.data.frame(sapply(df[,cols], function(x) RobustScaler(x)))
    colnames(transformed_df) <- paste0("robust_",cols)
    transfomed_df_final <- cbind(df,transformed_df)
    
    if(original_flag==TRUE){
      return(transfomed_df_final)
    }else{
      transfomed_df_final[,cols] <- NULL
      return(transfomed_df_final)
    }
  }
  
}

getmode <- function(v){
  v=v[nchar(as.character(v))>0]
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

