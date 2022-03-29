library(class)
library(caret)
library(readxl)   # this is the package for reading xlsx files


wbcd<-read_excel("wisc_bc_data.xlsx") 


str(wbcd)


wbcd <- wbcd[-1]


table(wbcd$diagnosis)

wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"), labels = c("Benign", "Malignant"))


round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1) 

summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}


wbcd_norm <- as.data.frame(lapply(wbcd[2:31], normalize))

wbcd_train <- wbcd_norm[1:469, ]
wbcd_test <- wbcd_norm[470:569, ]


wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels$diagnosis, k = 21)

CrossTable(x = wbcd_test_labels$diagnosis, y = wbcd_test_pred, prop.chisq = FALSE)


