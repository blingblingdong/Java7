#' DS：宣告一個DescriptiveStatistics物件
#'
#' 快速，整潔的使用java中的DescriptiveStatistics
#'
#' @param vector 傳入一個向量數列。
#' @param name 傳入一個字段。
#' @return 一個DescriptiveStatistics物件。
#' @examples
#' DS(c(1,2,3,4,5,6,7,8,9,10),"test")
DS <- function(vector, name){
  # 將向量轉換java array
  jarray <- .jarray(as.double(vector))
  # 宣告一個DescriptiveStatistics物件
  ds <- .jnew("DescriptiveStatistics", jarray, name)
  return(ds)
}

#' LR：宣告一個LinearRegression物件
#'
#' 快速，整潔的使用java中的LinearRegression
#'
#' @param v1 傳入一個向量數列作為因變量。
#' @param v2 傳入一個向量數列作為自變量。
#' @param name 傳入一個字段。
#'
#' @return 一個LinearRegression物件。
#'
#' @examples
#' LR(c(1,2,3,4,5,6,7,8,9,10),c(1,2,3,4,5,6,7,8,9,10),"test")
LR <- function(v1, v2, name){
  # 將向量轉換java array
  v1 <- .jarray(as.double(v1))
  v2 <- .jarray(as.double(v2))
  # 宣告一個LinearRegression物件
  lr <- .jnew("LinearRegression", v1, v2, name)
  return(lr)
}

#` LR_DS：宣告一個LinearRegression物件
#'
#' 以DescriptiveStatistics物件作為參數，快速，整潔的使用java中的LinearRegression
#'
#' @param DS 傳入一個DescriptiveStatistics物件。
#' @param vector 傳入一個向量數列作為自變量。
#'
#' @return 一個LinearRegression物件。
#' @examples
#' LR_DS(DS(c(1,2,3,4,5,6,7,8,9,10),"test"),c(1,2,3,4,5,6,7,8,9,10))
LR_DS <- function(DS,vector){
  # 將向量轉換java array
  array <- .jarray(as.double(vector))
  # 宣告一個LinearRegression物件
  lr <- .jnew("LinearRegression",DS$getData(), array,DS$getName())
  return(lr)
}

#' LR_predict：使用LinearRegression物件進行預測
#'
#' 使用LinearRegression中的method predict進行預測
#'
#' @param lr 傳入一個LinearRegression物件。
#' @param x 傳入一個數字預測。
#'
#' @return 一個預測值。
#' @examples
#' LR_predict(LR(c(1,2,3,4,5,6,7,8,9,10),c(1,2,3,4,5,6,7,8,9,10),"test"),1)
LR_predict <- function(lr, x){
  # 將向量轉換java array
  x <- as.double(x)
  # 宣告一個LinearRegression物件
  y <- lr$predict(x)
  return(y)
}

#' AN：宣告一個ANOVA物件
#'
#' 快速，整潔的使用java中的Anova
#'
#' @param list_of_vector 傳入一個含有多個向量的list。
#' @param name 傳入一個字段。
#'
#' @return 一個ANOVA物件。
#'
#' @examples
#' AN(list(c(1,2,3,4,5,6,7,8,9,10),c(1,2,3,4,5,6,7,8,9,10)),"test")
AN <- function(list_of_vector, name){
  # 將向量轉換java array
  list_of_vector <- lapply(list_of_vector, as.double)
  list_of_array <- lapply(list_of_vector, .jarray)
  D2 <- .jarray(list_of_array,"[D")
  # 宣告一個Analysis物件
  an <- .jnew("Anova",D2, name)
  return(an)
}

