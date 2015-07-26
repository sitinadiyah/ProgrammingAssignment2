## Course : Coursera - R Programming
## Programming Assignment 2
## Author: Siti Nadiyah



## function makeCacheMatrix creates a special matrix object and can cache its inverse
## and returns a list of functions


makeCacheMatrix <- function(x = matrix()) {
# assigning inverseMatrix as NULL value
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  
  # get function to get Matrix
  # set function to set a new Matrix
  # setInverse with Inverse Matrix solution
  # getInverse to get InverseMatrix
  
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getInverse<-function() m
  
  #return a list of functions
  list(set=set, get=get,
       setmatrix=setmatrix,
       getInverse=getInverse)
}

## function cacheSolve computes the inverse of the special "matrix" created by 
## function makeCacheMatrix above

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getInverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  # gettting matrix and calculating inverse
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}



 