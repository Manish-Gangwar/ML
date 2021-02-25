# basic code for a one-hot encoding and descrtiptive stats wala app 
df <- read.csv("D://churn_data.csv")
if (!require("purrr")){install.packages("purrr"); library("purrr")}
if (!require("fastDummies")){install.packages("fastDummies"); library("fastDummies")}

#Function 1: Upload data function------
upload_data <- function(file){
  df <- read.csv(file)
  return(df)
}


dataf <- upload_data('https://vincentarelbundock.github.io/Rdatasets/csv/carData/Salaries.csv')


#Function 2: select numeric cols-----
select_numeric_cols <- function(df){
  num_df <- df %>% select_if(is.numeric)
  return(num_df)
}

data_n <- select_numeric_cols(dataf)

#Function 3: select cat cols-----
select_cat_cols <- function(df){
  cat_df <- df %>% select_if(is.character)
  return(cat_df)
}

data_c <- select_cat_cols(dataf)



convert_cat_to_num <- function(df){
  
}
#Function 4 : plot missing vallues----
 display_missing_percentage <- function(data){
  # count total, missing value & its percentage 
  missing.values <- data %>%
    gather(key = "key", value = "val") %>%
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

#Function 5: return colnames with missing values--------
df<- data
miss_cols <- function(df){
  list_vars = list()
  miss_cols <- sapply(df,function(x) sum(is.na(x)))
  miss_cols <- names(miss_cols[miss_cols!=0])
  miss_cols_class <- sapply(data[miss_cols], function(x) class(x))
  miss_cols_int <- names(miss_cols_class[miss_cols_class=="integer"])
  miss_cols_cat <- names(miss_cols_class[miss_cols_class=="character"|miss_cols_class=="factor"])
  
  list_vars[[1]] <- miss_cols_int
  list_vars[[2]] <- miss_cols_cat
  
  return(list_vars)
}




miss_cols_list <- miss_cols(data)
int_cols <- miss_cols_list[[1]]
miss_cols_char <- miss_cols_list[[2]]
intcol


imputer <- function(df,num_method,cat_method=NULL,int_cols,cat_cols,original_flag){
  df_copy <- df
  replaced_by  <- sapply(df_copy[int_cols],function(x) mean(x,na.rm = TRUE))
  imputed_df <- sapply(df_copy[int_cols], function(x) impute(x, mean))
  colnames(imputed_df) <- paste0("imputed_",int_cols)
  imputed_df_final <- cbind(df_copy,imputed_df)
  
  if(original_flag==TRUE){
    return(imputed_df_final)
  }else{
    imputed_df_final[,int_cols] <- NULL
    return(imputed_df_final)
  }
  
}



sapply(data,function(x) sum(is.na(x)))
sapply(data[miss_cols_int],function(x) mean(x,na.rm = TRUE))

p <- sapply(data[miss_cols_int], function(x) impute(x, mean))

colnames(p) <- paste0("imputed_",miss_cols_int)
colnames(p)
q <- cbind(df,p)



# VIM Exploration
a <- aggr(data, plot = FALSE)
plot(a, numbers = TRUE, prop = FALSE)


x_imputed <- kNN(data)


#Function 6 : data transformation

MinMaxScaler <- function(colm0, a=0, b=1){
  Max0 = max(colm0); Min0 = min(colm0)
  colm1 = sapply(colm0, function(x) {(x-Min0)*(b-a)/(Max0 - Min0)})
  return(colm1)
}

RobustScaler <- function(colm0){
  q1 = as.numeric(quantile(colm0, 0.25))
  q3 = as.numeric(quantile(colm0, 0.75))
  colm1 = sapply(colm0, function(x) {(x - q1)/(q3 - q1)})
  return(colm1)
}

StdScaler <- function(colm0){
  colm1 = scale(colm0)
  return(colm1)
}

df <-sleep

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



t <- data_transform(df,cols=c("mpg","cyl","disp"),method = "minmax",original_flag = TRUE)
t1 <- data_transform(df,cols=c("mpg","cyl","disp"),method = "standard",original_flag = FALSE)
