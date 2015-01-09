#'Building Model with top 10 feature
#'
#'This function develops a prediction algorithm based on top 10 features
#'in "x" that are most predictive of "y"
#'
#'@param x a n X p matrix of n observation and p predictors
#'@param y is a vector of length n respresenting the response
#'@return a vector of coefficients from the fitted model with top 10 features
#'@author Abhishek Kapoor
#'@details
#'This function run univariate  regression of y on each predictor in x and
#'calulate a p-values indicating  the significance  of the association. The
#'final set of 10 predictors is taken from the 10 smallest p-values
#'@seealso \code {lm}
#'@export
#'@importFrom stats lm 

top10predict <- function (x,y){
  p<- ncol (x)
    if (p <10) 
    stop ("there are less than 10 predictors")
  pvalues <- numeric (p)
  for (i in seq_len(p)){
    
        fit<- lm(y~x[,i])
        summ<- summary (fit)
    pvalues[i]<- summ$coefficients[2,4]
  }
ord<- order (pvalues)

ord <- ord [1:10]
x10<- x[,ord]
fit<- lm(y~x10)
coef(fit)
}


#'Predicting with top 10 features
#'
#'This fuction takes a set of coefficients produced by the \code{top10predict}
#'function and makes prediction for each value provided in the input "X" matrix.
#'
#'@param X a n x 10 matrix containing n new observations
#'@param b a vector of coefficients obtained from the \code{top10predict} function
#'@return a numeric vector containing predicited values 
#'@export

predict10 <- function (X,b){
  X<- cbind (1,X)
  drop (X %% b)
  
}