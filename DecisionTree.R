data(iris)
names(iris)
#[1] "Sepal.Length" "Sepal.Width"  "Petal.Length" "Petal.Width"  "Species"  
table(iris$Species)

#setosa versicolor  virginica 
#50         50         50
#install if necessary
install.packages("ggplot2")
library(ggplot2)
qplot(Petal.Width, Sepal.Width, data=iris, colour=Species, size=I(4))



#building the classification tree
#install if necessary
install.packages("tree")
library(tree)
tree1 <- tree(Species ~ Sepal.Width + Petal.Width, data = iris)
summary(tree1)

tree2 <- tree(Species ~ ., data = iris)
summary(tree2)

#Classification tree:
               # tree(formula = Species ~ Sepal.Width + Petal.Width, data = iris)
#Number of terminal nodes:  5 
#Residual mean deviance:  0.204 = 29.57 / 145 
#Misclassification error rate: 0.03333 = 5 / 150
plot(tree1)
text(tree1)

library(rpart.plot)


plot(iris$Petal.Width,iris$Sepal.Width,pch=19,col=as.numeric(iris$Species))
partition.tree(tree1,label="Species",add=TRUE)
legend(1.75,4.5,legend=unique(iris$Species),col=unique(as.numeric(iris$Species)),pch=19)



graph <- qplot(Petal.Width, Sepal.Width, data=iris, colour=Species, size=I(4))
graph + geom_hline(aes(yintercept=2.65)) + geom_vline(aes(xintercept=0.8)) + geom_vline(aes(xintercept=1.75)) + geom_vline(aes(xintercept=1.35))



tree1 <- tree(Species ~ Sepal.Width + Sepal.Length + Petal.Length + Petal.Width, data = iris)
summary(tree1)

#Classification tree:
  tree(formula = Species ~ Sepal.Width + Sepal.Length + Petal.Length + 
         Petal.Width, data = iris)
#Variables actually used in tree construction:
 # [1] "Petal.Length" "Petal.Width"  "Sepal.Length"
#Number of terminal nodes:  6 
#Residual mean deviance:  0.1253 = 18.05 / 144 
#Misclassification error rate: 0.02667 = 4 / 150


  
  plot(tree1)
  text(tree1)
  
  
  
  
  #Petal.Length < 2.45
  iris[iris$Petal.Length<2.45,5]
  #[1] setosa setosa setosa setosa setosa setosa setosa setosa setosa setosa setosa setosa setosa setosa setosa setosa setosa
  #[18] setosa setosa setosa setosa setosa setosa setosa setosa setosa setosa setosa setosa setosa setosa setosa setosa setosa
  #[35] setosa setosa setosa setosa setosa setosa setosa setosa setosa setosa setosa setosa setosa setosa setosa setosa
  #Levels: setosa versicolor virginica
  #we get all 50 setosa
  length(iris[iris$Petal.Length<2.45,5])
  #[1] 50
  iris[iris$Petal.Length>2.45&iris$Petal.Width>1.75,5]
  #[1] versicolor virginica  virginica  virginica  virginica  virginica  virginica  virginica  virginica  virginica 
  #[11] virginica  virginica  virginica  virginica  virginica  virginica  virginica  virginica  virginica  virginica 
  #[21] virginica  virginica  virginica  virginica  virginica  virginica  virginica  virginica  virginica  virginica 
  #[31] virginica  virginica  virginica  virginica  virginica  virginica  virginica  virginica  virginica  virginica 
  #[41] virginica  virginica  virginica  virginica  virginica  virginica 
  #Levels: setosa versicolor virginica
  #most of the virginica
  length(iris[iris$Petal.Length>2.45&iris$Petal.Width>1.75,5])
  #[1] 46
  #2 misclassifications
  iris[iris$Petal.Length>2.45&iris$Petal.Width<1.75&iris$Petal.Length>4.95,5]
  #[1] versicolor versicolor virginica  virginica  virginica  virginica 
  #Levels: setosa versicolor virginica
  #viewing the Petal.Length of all species
  boxplot(formula=Petal.Length ~ Species, data=iris, xlab="Species", ylab="Petal length")
  
  
  
  
  #install if necessary
  install.packages('rpart')
  library(rpart)
  rpart1 <- rpart(Species ~ ., data=iris, method="class")
  #, split, n, loss, yval, (yprob)
#* denotes terminal node

#1) root 150 100 setosa (0.33333333 0.33333333 0.33333333)  
#2) Petal.Length< 2.45 50   0 setosa (1.00000000 0.00000000 0.00000000) *
#  3) Petal.Length>=2.45 100  50 versicolor (0.00000000 0.50000000 0.50000000)  
#6) Petal.Width< 1.75 54   5 versicolor (0.00000000 0.90740741 0.09259259) *
#  7) Petal.Width>=1.75 46   1 virginica (0.00000000 0.02173913 0.97826087) *
  
  
  
  #plot decision tree
  fancyRpartPlot(rpart1, main="Iris")
  
  
  rpart.plot(rpart1)
  rpart.plot(rpart1, type =1, extra=101)
  