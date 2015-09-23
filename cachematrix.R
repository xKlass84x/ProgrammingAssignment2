## The logic applied here is fundamentally equivalent to the one used as example for this assignment.
## MakeCacheMatrix will take the input and will create the matrix and its inverse will be then computed 
## by cacheSolve function. the only element of noveltry are in makeCacheMatrix.

## Write a short comment describing this function

## This function in an object consisting of 4 functions: set(),get(),set_inv() and get_inv().
## The first 2 functions allow to store and return the values of the matrix while the last 2 functions store
## and retrieve the value of the inverse of the matrix provided by cacheSolve function (it means that after you find the inverse function with cacheSolve(x) you can then get it back with x$get_inv function). Here i added (even thought was not requested)
## a first check on the matrix provided so if the input provided is not a squared (2) non-singular (3) matrix (3)
## the function will stop

makeCacheMatrix <- function(x = matrix()) {
            if(!is.matrix(x)){
            print("Please insert a matrix and try again.")
            stop}
  
            else if(det(x)==0) {
            print("Please insert a nonsingular matrix (with a determinand different from 0) and try again.")
            stop}
              
            else if(nrow(x)!=ncol(x)){
            print("Please insert a squared matrix and try again.")
            stop}
  
        m <- NULL
        
set <- function(y) {
    
        x <<- y
        m <<- NULL
}
get <- function() x
set_inv <- function(inversed) m <<- inversed
get_inv <- function() m

list(set = set, get = get,
     set_inv = set_inv,
     get_inv = get_inv)
}

## Write a short comment describing this function

## cacheSolve provide the inverse matrix of the one created with makeCacheMatrix.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

m <- x$get_inv()

            if(!is.null(m)) {
            message("getting cached data")
            return(m)
            }

data <- x$get()
m <- solve(data)
x$set_inv(m)
m
}

