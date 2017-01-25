# 一. 課堂中的自訂排序函數加入 decreasing = 的參數（預設為 FALSE）
# 1. 宣告函數
exchange.sort <- function(input_vector,decreasing = FALSE) {
  vector_length <- length(input_vector)
  
  for (i in 1:(vector_length - 1)) {
    for (j in (i + 1):vector_length) {
      if (decreasing == FALSE){
        if (input_vector[i] > input_vector[j]) {
          temp <- input_vector[i]
          input_vector[i] <- input_vector[j]
          input_vector[j] <- temp
        }
      }else{
        if (input_vector[i] < input_vector[j]) {
          temp <- input_vector[i]
          input_vector[i] <- input_vector[j]
          input_vector[j] <- temp
        }
      }
    }
  }
  return(input_vector)
}
# 2. Create an input or parameters
unsorted_vector <- round(runif(10) * 100)
# 3. Call both the created function and the built-in one to verify that they are the same. 
exchange.sort(unsorted_vector, decreasing = F)
sort(unsorted_vector, decreasing = F)
exchange.sort(unsorted_vector, decreasing = T)
sort(unsorted_vector, decreasing = T)


# 二. 建立「樣本標準差」函數
# 1. 宣告函數，宣告自建函數名稱為「My_ssd（sample standard deviation）」
My_ssd <- function(input_vector){
  # 觀察整個ssd公式中，有兩處為「不隨著iterator迭代的constant」，分別是"分母N-1"和平均數，
  # 故對此兩常數進行assignment。
  sample_number <- length(input_vector)-1
  sample_mean <- sum(input_vector)/length(input_vector)
  # 再觀察整個ssd公式中，需要迭代的部分只有"x-平均數"，
  # 故設計含有迭代循環的迴圈
  square_diff_i <- 0
  for(i in input_vector){
    square_diff_i <- square_diff_i + (i-sample_mean)^2
    ssd_input <- ((1/sample_number)*square_diff_i)^0.5
  }
  return(ssd_input)
}
# 2. Create an input or parameters
unsorted_vector
# 3. # 3. Call both the created function and the built-in one to verify that they are the same.
My_ssd(unsorted_vector)
stats::sd(unsorted_vector)


# 三. 建立「計算BMI」函數
# 三-1. 建立傳統迴圈進行BMI運算。
# 1. 宣告函數
My_bmi <- function(height,weight){
  if(length(height)==length(weight)){
    bmi_vec <- vector(mode = mode(weight[1]/(height[1]/100)^2),length = length(height))
    for(i in 1:length(height)){
      bmi_vec[i] <- weight[i]/(height[i]/100)^2
    }
    return(bmi_vec)
  }else{
    print("warning: The length of vector about height is not equal to it about weight. Please check out them above.")
  }
}
# 2. Create an input or parameters
heights <- c(173, 168, 171, 189, 179)
weights <- c(65.4, 59.2, 63.6, 88.4, 68.7)
heights_and_weights <- data.frame(heights, weights)
# 3. function call
My_bmi(heights,weights)

# 三-2. 建立核心運算函數，以及搭配apply family進行BMI運算。
# 在寫過和觀察迴圈後，可以發現「迴圈本身就是一個『重複動作』」！
# 因此，R程式語言中，只要給予含核心運算部分之函數，
# 丟入「apply family」中，後者就可以代替執行類似迴圈之運算。
# 1. 宣告函數
My_bmi_for_apply <- function(height,weight){
  bmi <- weight/(height/100)^2
  print(bmi)
}
# 2. Create an input or parameters
heights <- c(173, 168, 171, 189, 179)
weights <- c(65.4, 59.2, 63.6, 88.4, 68.7)
heights_and_weights <- data.frame(heights, weights)
# 3. function call
mapply(My_bmi_for_apply,heights,weights)

# 將三-1與三-2結果擇一新增入Data.frame："heights_and_weights"，形成新Column。
library("magrittr", lib.loc="~/R/win-library/3.3")
# 以三-1為例：
BMI_data <- My_bmi(heights,weights) %>%
            cbind(heights_and_weights,BMI=.)
# 以三-2為例：
BMI_data <- mapply(My_bmi_for_apply,heights,weights) %>%
            cbind(heights_and_weights,BMI=.)
