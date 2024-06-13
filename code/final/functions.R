
#Define function that takes an income of given year, and returns the expenses.
#We ignore savings, debts and taxes, but adjust the incomes to the corresponding real price/wage value of 1688.
get_disposable_income <- function(x,x_year){
  conversion_factor <- 1
  if(x_year>=1700){
    conversion_year <- subset(monetary_conversions,year==x_year)
    conversion_factor <- (conversion_year$pound+conversion_year$shilling/20+conversion_year$pence/240)
  }
  disposable_income <- x-exp((log(x/conversion_factor)*income_to_expenditure_coefficients[2]+income_to_expenditure_coefficients[1])+income_to_expenditure_coefficients[3]^2*1/2)*conversion_factor
  
  return(disposable_income)  
}

#Define function that linearly extrapolates missing values of an (assumed to be continous) function f(x)=y
#Used to linearly extrapolate missing income quantiles of the social tables.
get_extrapolation <- function(x_pred,probs,vals,col_names=c("quantile","disposable_income")){
  
  approximations <- approx(x=probs,y=vals,xout=x_pred)
  
  x_prob <- approximations[[1]]
  y_val <- approximations[[2]]
  
  joint_data <- cbind.data.frame(x_prob,y_val)
  colnames(joint_data) <- col_names
  
  return(joint_data)
  
}

#Function to name columns- Input is a data frame, output is the data frame with column from the second input nameVec
name_data_frame <- function(X,nameVec){
  
  colnames(X) <- nameVec  
  
  return(X)  
}

#Function that takes a vector input, and returns a boolean answer on whether the sum of its elements is 0 or not
is_zero_sum <- function(x){
  if(sum(x)==0){
    return(TRUE)  
  }else{
    return(FALSE)
  }  
}

# Function that takes a table with draws from a posterior distribution as an input ( one column per parameter)
#and returns a summary of the distribution
distribution_summary_2 <- function(data,columns_to_summarize){
  
  data_dist <- data[,columns_to_summarize] %>% as.matrix(.)
  
  data_other_variable_names <- colnames(data) %>% .[-columns_to_summarize]
  data_other_variables <- data[,-columns_to_summarize] %>% as.data.frame()
  colnames(data_other_variables) <- data_other_variable_names
  
  median <- apply(data_dist,2,FUN=median)
  
  dist_75_interval <- apply(data_dist,2,FUN=quantile,probs=0.75) %>% as.numeric(.)
  dist_25_interval <- apply(data_dist,2,FUN=quantile,probs=0.25) %>% as.numeric(.)
  dist_2_5_interval <- apply(data_dist,2,FUN=quantile,probs=0.025) %>% as.numeric(.)
  dist_97_5_interval <- apply(data_dist,2,FUN=quantile,probs=0.975) %>% as.numeric(.)
  
  distribution_final <- cbind.data.frame(dist_2_5_interval,dist_25_interval,median,dist_75_interval,dist_97_5_interval)
  
  return(distribution_final)
  
}


#Compute mean of a vector from a quantile range of its values
mean_quantile_range <- function(x,q_min=0,q_max=1){
  
  q_lower <- quantile(x,q_min)
  q_upper <- quantile(x,q_max)
  
  return(mean(x[x>=q_lower & x<=q_upper]))
}

#Compute mean for a vector from a quantile range over a asceding vector of quantiles

quantile_means <- function(x,q){
  mean_vec <- numeric(length(q)-1)
  name_vec <- character(length(q)-1)
  n_of_households <- numeric(length(q)-1)
  for(i in 1:(length(q)-1)){  
    mean_vec[i] <- mean_quantile_range(x,q[i],q[i+1])  
    name_vec[i] <- paste0("from_",as.character(q[i]),"_to_",as.character(q[i+1]))
    n_of_households[i] <- (q[i+1]-q[i])*length(x)
  }
  data <- cbind.data.frame(name_vec,mean_vec,n_of_households)
  colnames(data) <- c("quantile","income","n_of_households")
  return(data)
}
