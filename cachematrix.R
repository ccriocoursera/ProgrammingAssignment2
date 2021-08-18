## This is performed in fulfillment of the Week 3 assignment.
## The main objective here is to create a pair of functions intended for caching the matrix inverse.
## Two primary functions will be utilized.


## The first function is the makeCacheMatrix.
## This function creates a special matrix object that can cache its inverse.
makeCacheMatrix <- function(a = matrix()) {
  ## This is for the initialization of inverse property as null.
  c <- NULL                  
  ## This is for setting the matrix. The set function will then be defined and subsequently result to assignment of new.
  set <- function(b) {      
    ## This is for saving matrix within function to parent environment.
    a <<- b                  
    ## This is for resetting the state when needed.
    k <<- NULL               
  }
  ## This is for getting the matrix. The get function will be defined and will be used to get the matrix x.
  get <- function()                                     
  ## This is for setting the inverse of the matrix. The value of k is assigned to its parent environment.
  setInverse <- function(inverse) c <<- inverse         
  ## This is for getting the inverse of the matrix. This will return the property of inverse.
  getInverse <- function() c   
  ## This will return the list of methods. This will be used to determine those functions with $ operation
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function aims to compute the special matrix inverse which was returned through the usage of makeCacheMatrix.
## It should be noted that if the inverse has already been calculated or if the matrix has not been changed;
## Then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(a, ...) {
  ## This is for returning the matrix which is the inverse of c.
  c <- a$getInverse()
  ## This will evaluate whether the inverse is null. 
  if(!is.null(c)) {
    message("getting cached data")
    ## This will return the value of the inverse if it happens to be set.
    return(c)
  }
  ## This is for getting the matrix from the object
  data <- a$get()
  ## This is for calculating the value of the inverse.
  c <- solve(data, ...)
  ## This is for setting the inverse to the object.
  a$setInverse(c)
  ## This is for returning the matrix which is the inverse of c.
  c
}

