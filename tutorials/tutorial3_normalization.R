#data normalizayion example
# Age vector
age <- c(25, 35, 50)

# Salary vector
salary <- c(200000, 1200000, 2000000)

# Data frame created using age and salary
df <- data.frame( "Age" = age, "Salary" = salary, stringsAsFactors = FALSE)
df
#Age Salary
# 25 200000
# 35 1200000
# 50 2000000


#min max normalize function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}


#apply function
dfNorm <- as.data.frame(lapply(df, normalize))
dfNorm


new_normalize <- function(x, new_max=1,new_min=0) # see how we define the max min values
{
  a= (((x-min(x))* (new_max-new_min))/(max(x)-min(x)))+new_min
  return(a)
}

dfNorm1 <- as.data.frame(lapply(df[1:2], new_normalize))
dfNorm1



