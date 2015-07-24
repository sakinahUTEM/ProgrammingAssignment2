
## Matrix inversion is ussually a costly computation and 
## there may be some benefit to caching the inverse of a matrix
## rather tham compute it repeatedly.

## makeCacheMatrix: This function creates a special "matrix" object 
##                  that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
  #store the CacheMatrix value 
  #iniliaze the CacheMatrix to NULL
  CacheMatrix<-NULL
  
  #creating the Matrix 
  setMatrix<-function (y){
    x<<-y
    CacheMatrix<<-NULL
  }
  
  #getting the value of the Matrix
  getMatrix<-function() x
  
  #inverting the Matrix and store in CacheMatrix
  setInverseMatrix<-function(solve) CacheMatrix<<-solve
  
  #getting the inverted matrix from CacheMatrix
  getInverseMatrix<-function() CacheMatrix
  
  # return the result of the created function
  list(setMatrix=setMatrix,
       getMatrix=getMatrix,
       setInverseMatrix=setInverseMatrix,
       getInverseMatrix=getInverseMatrix)
  
}


## cacheSolve     : This function computes the inverse of the special "matrix" 
##                  returned by makeCacheMatrix above. If the inverse has already 
##                  been calculated (and the matrix has not changed), then the 
##                  cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

  #attempt to get the inverse of the matrix stored in cacheMatrix
  CacheMatrix<-x$getInverseMatrix()
  
  #Return the inverted matrix from CacheMatrix if exists 
  if(!is.null(CacheMatrix)){
    massage(" Getting Cache Data")
    #display the matrix
    return(CacheMatrix)
  }
  
  #creating new matrix if it does not exists
  matx<-x$getMatrix()
  #calculate the inverse of the matrix 
  CacheMatrix<-solve(matx, ...)
  #set the value of inverted matrix 
  x$SetInverseMatrix(CacheMatrix)

  #display the matrix
 return(CacheMatrix)
}
