## Put comments here that give an overall description of what your
## functions do
## These functions cache the inverse of a matrix

## Write a short comment describing this function
## makeCacheMatrix makes a matrix object to cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  j<-NULL
  set<-function(y){
    x<<-y
    j<<-NULL
  }
  
  get<-function()x
  setInverse<-function(inverse)j<<-inverse
  getInverse<-function()j
  list(set=set,get=get,
       setInverse=setInverse,
       getInverse=getInverse)
}


## Write a short comment describing this function
## cacheSolve receives the matrix object from makeCacheMatrix and computes its inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  j<-x$getInverse()
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  mat<-x$get()
  j<-solve(mat,...)
  x$setInverse(j)
  j
}
