############# libraries used ############
# for methods
library(MASS)  #help(qda)
library(pROC)
# for assumption-checking 
library(mvnormalTest)  # of multivariate normality; or library(MVN) or (mvnTest)
library(MVTests)  # of constant covariance; or library(biotools) or (biotools) or (heplots)
# for data organization
library(dplyr)
# for visuals
library(ggformula)

# added lines to specify version of iris data set
iris <- MVTests::iris  # added to require the version of the iris data set with capitalized Species names
levels(iris$Species)

############# Review data #############
data(iris)
n = dim(iris)[1]; n
names(iris)
levels(iris$Species)
K=length(levels(iris$Species)); K

gf_boxplot(Petal.Length ~ Species, data = iris,fill=c("skyblue","navy","purple"))

############# set-up groups for cross-validation #############
nfolds = 10
groups = c(rep(1:nfolds,length=n))
set.seed(4)
cvgroups = sample(groups,n)

############# apply LDA, using one predictor to classify Species (Model L1) #############
#methodapplied = "LDA"
#modelapplied = (Species~Petal.Length)
############# apply QDA, using one predictor to classify Species (Model Q1) #############
#methodapplied = "QDA"
#modelapplied = (Species~Petal.Length)
############# apply LDA, using all 4 predictors to classify Species (Model L4) #############
#methodapplied = "LDA"
#modelapplied = (Species~.)
############# apply QDA, using all 4 predictors to classify Species (Model Q4) #############
methodapplied = "QDA"
modelapplied = (Species~.)

# look at error, re-predicting data used to fit the model
y = iris$Species
if (methodapplied == "LDA") {
  modelfit = lda(modelapplied, data=iris) } else if (methodapplied == "QDA") {
  modelfit = qda(modelapplied, data=iris)
}
predclass = predict(modelfit,data=iris)$class
table(y,predclass)
fitError = sum(y != predclass)/n; fitError

# Honest prediction via cross-validation
CVpredclass = rep("NA",n)
# loop through cvgroups, to compute honest predicted values for CV measure
for (ii in 1: nfolds) {    # ii is an easier string to search for index
  groupii = (cvgroups == ii)
  trainset = iris[!groupii,]  # all data EXCEPT for group ii
  testset = iris[groupii, ]   # data in group ii
  
  if (methodapplied == "LDA") {
    modelfitii = lda(modelapplied, data=trainset) } else if (methodapplied == "QDA") {
    modelfitii = qda(modelapplied, data=trainset)
  }
  
  predicted = as.character(predict(modelfitii, newdata=testset)$class)   # predict for test set
  CVpredclass[groupii] = predicted              # store in ordered locations
}

# compute CV measure as misclassification rate
table(y,CVpredclass)
CVError = sum(CVpredclass!=y)/n; CVError

#CVErrorL1 = CVError
#CVErrorQ1 = CVError
#CVErrorL4 = CVError
#CVErrorQ4 = CVError


############# Model selection ############# 
CVErrorL1   #lda with 1 predictor
CVErrorQ1   #qda with 1 predictor
CVErrorL4   #lda with 4 predictors
CVErrorQ4   #qda with 4 predictors

#equal covariance matrices are definitely NOT reasonable, 
#  so we prefer models Q1 and Q4 (QDA) to the LDA models 
#  and model Q4 has a slightly lower CV than does model Q1, 
#  so we select model Q4


############# Checking Assumptions ############# 
#assumptions for models using all four predictors

# full Xmatrix
xvar = iris %>% 
  select(Sepal.Length,Sepal.Width,Petal.Length,Petal.Width)
# Xmatrix within each class
xSetosa = iris %>% 
  filter(Species == "Setosa") %>% 
  select(Sepal.Length,Sepal.Width,Petal.Length,Petal.Width)
xVersicolor = iris %>% 
  filter(Species == "Versicolor") %>% 
  select(Sepal.Length,Sepal.Width,Petal.Length,Petal.Width)
xVirginica = iris %>% 
  filter(Species == "Virginica") %>% 
  select(Sepal.Length,Sepal.Width,Petal.Length,Petal.Width)

# check for multivariate normality
mhz(xSetosa)$mv.test
mhz(xVersicolor)$mv.test
mhz(xVirginica)$mv.test
#multivariate normality of the predictors is (close to) reasonable

# check for equal covariance
BoxM(xvar,iris$Species)
# equal covariance matrices are definitely NOT reasonable, 
#  so QDA is the better option



############# ROC of LDA and QDA predictions for two-level response ############# 
# just using the predictor values since decide off linear function of predictor
iris$Virginica = as.numeric(iris$Species == "Virginica")
# fit models
ldafitVirg = lda(Virginica~.-Species, data=iris)
qdafitVirg = qda(Virginica~.-Species, data=iris)
# posterior probability of Virginica species
ldaprob = predict(ldafitVirg,data=iris)$posterior[,2]
qdaprob = predict(qdafitVirg,data=iris)$posterior[,2]
# ROC curve for LDA fit of Virginica on four predictors
lda.roc = roc(response=iris$Virginica, 
              predictor=ldaprob)
plot.roc(lda.roc)  
# ROC curve for QDA fit of Virginica on four predictors
qda.roc = roc(response=iris$Virginica, 
              predictor=qdaprob)
plot.roc(qda.roc)  

