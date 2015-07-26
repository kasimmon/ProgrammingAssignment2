## R Programming Assignment 2, Kirsten Simmons, 7/26/15
## Goal is to create a function that caches the inverse of a matrix, allowing it to be used in future calculations without 
## computing the values


#The first function creates a matrix object that is capable of cacheing its inverse.
makeCacheMatrix<-function(x=matrix()){
  m<-NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get<-function() x
  setInverse<-function(solve) m <<- inverse
  getInverse<-function()m
  list(get=get, setInverse=setInverse, getInverse=getInverse)
}

#The second function computes the inverse of the matrix created by the previous function.  If the inverse has already
#been calculated, this function will retrieve the inverse from the cache.
cacheSolve<-function(x, ...) {
  m<-x$getInverse()
  if(!is.null(m)){
    message("Getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data, ...)
  x$setInverse(m)
  m
}
