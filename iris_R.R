#iris dataset classification
#importing dataset
dataset = read.csv('Book1.csv')
dataset = dataset[c(1,3,5,7,9)]
colnames(dataset) <- c ("sepal_len", "sepal_wid", "petal_len","petal_wid","class")

#convert the class column into numeric factors
dataset$class = factor(dataset$class,levels = c("Iris-setosa","Iris-versic","Iris-virgin"), labels = c(0,1,2))

#splitting dataset into trainingset and test set
library(caTools)
set.seed(1234)
split = sample.split(dataset$class,SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

#Fitting knn to training set and predicting the results of the test set
library(class)  #library for KNN classification
y_pred = knn(train = training_set[,-5],
             test = test_set[,-5],
             cl = training_set[,5],
             k=9)

#make confusion matrix and calcutate accuracy
cm = table(test_set[,5],y_pred)
cm

#100% accuracy


#visualizing training data
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.1)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.1)
X3 = seq(min(set[, 3]) - 1, max(set[, 3]) + 1, by = 0.1)
X4 = seq(min(set[, 4]) - 1, max(set[, 4]) + 1, by = 0.1)

grid_set = expand.grid(X1, X2, X3, X4)
colnames(grid_set) = c('sepal_length', 'sepal_width','petal_len','petal_width')
#y_grid = knn(train = training_set[, -3], test = grid_set, cl = training_set[, 3], k = 5)
y_grid = knn(train = training_set[, -5], test = grid_set, cl = training_set[, 5], k = 9)
plot(set[, c(1,2)],
     main = 'K-NN (Training set)',
     xlab = 'Sepal_length', ylab = 'Sepal_width',
     xlim = range(X1), ylim = range(X2))
#contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
#points(grid_set[c(1,2)], pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'blue'))
points(set, pch = 21, bg = ifelse(set[, 5] == 1, 'green4', 'red3'))

#visualization for test set
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.1)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.1)
X3 = seq(min(set[, 3]) - 1, max(set[, 3]) + 1, by = 0.1)
X4 = seq(min(set[, 4]) - 1, max(set[, 4]) + 1, by = 0.1)

grid_set = expand.grid(X1, X2, X3, X4)
colnames(grid_set) = c('sepal_length', 'sepal_width','petal_len','petal_width')
#y_grid = knn(train = training_set[, -3], test = grid_set, cl = training_set[, 3], k = 5)
y_grid = knn(train = training_set[, -5], test = grid_set, cl = training_set[, 5], k = 9)
plot(set[, c(1,2)],
     main = 'K-NN (Test set)',
     xlab = 'Sepal_length', ylab = 'Sepal_width',
     xlim = range(X1), ylim = range(X2))
#contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
#points(grid_set[c(1,2)], pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'blue'))
points(set, pch = 21, bg = ifelse(set[, 5] == 1, 'green4', 'red3'))

