\name{fairselect}
\alias{fairselect}
\title{Feature Annealed Independence Rule}
\concept{feature selection, classification}


\usage{
fairselect(training,testing,method)



}


\description{
 This function does binary classification on the high dimensional data using FAIR. See reference for more details.
}

\arguments{
  
\item{training}{training dataset should be in the form of matrix. It is used to build models.}
  \item{testing}{testing dataset should be in the form of matrix. It is used to check model accuracy.}
  \item{method}{method has two options: "ttest" or "oracle". They are different criteria for feature selection on high dimensional data.} } 



\details{
  See Fan, J. and Fan, Y. (2008) for more details. 
This R package is about implementing the Features Annealed Independence Rules proposed by Fan, J. and Fan, Y. (2008) on high dimensional classification. When the number of covariates p is way larger than the sample size n, classification is difficult since it is not easy to know which variables are important and which are unimportant. Because of the high dimension issue, the Fisher discriminant analysis gives poor performance and estimation of this matrix is nearly impossible. Bickel and Levina (2004) propose the independence rule by using only the diagonal elements of the variance covariance matrix to construct the Fisher's rule. 

Fan, J. and Fan, Y. (2008) show that this still needs to be improved because the classification effect is not satisfactory.  To enhance the classification power, Fan, J. and Fan, Y (2008) propose the Features Annealed Independence Rules. In their opinion, the unsatisfactory performance of the independence rule is due to the inclusion of unimportant predictors(features). As a matter of fact, these unimportant features accumulate the errors and add to much noise. To extract important features in the first step is the key to mitigate this problem. They proposed the Features Annealed Independence Rules. Two methods are used and named as "t test" and "oracle". The first method is about ranking features according to the absolute value of the t statistic for each predictor in the training sample. The second method is about ranking features according to the absolute value of the sample mean difference for each predictors in the training sample. These two methods to some extent reflect whether the predictors are important or not in the classification process.
} 


\value{
  A list containing the following components:
  

\item{value}{the minimum misclassification error by employing the model onto the testing data.}
  
\item{feature}{the optimal number of important features to select.}
  
\item{m1}{the sample mean of the first class in the training data.}
  
\item{m2}{the sample mean of the second class in the training data.}
  
\item{cova}{variance covariance matrix of the training data.}
 
}





\references{

Fan, J. and Fan, Y. (2008) High-Dimensional Classification Using Features 
Annealed Independence Rules. \emph{Annals of Statistics}, 36(6), 2008. 
Bickel,P.J. and Levina,E. (2004) Some theory for Fisher's linear discriminant function, "naive Bayes," and some alternatives 
when here are many more variables than observations. \emph{Bernoulli}, 10, 989-1010, 2004. 
}




\examples{
x=matrix(rnorm(30*100),nrow=30)
x[,1]=rbinom(30,1,prob=0.5)
y=matrix(rnorm(30*100,0,1),nrow=30)
y[,1]=rbinom(30,1,prob=0.5)
training=x
testing=y
newdata=rnorm(99)
a=fairselect(x,y,"ttest")
b=fairselect(x,y,"oracle")
print(a)
print(b)


classify(newdata,a)
} 

\keyword{FAIR}
