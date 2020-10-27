## Put comments here that give an overall description of what your
## functions do

## XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
## As part of the Programming Assignment 2: Lexical Scoping, I am creating
## a pair of functions to compute the inverse of a matrix by caching the result
## within a lexical scope of each of the function.
## Lexical scopes will allow to function within a function and new 'user-defined'
## objects (or data types) to store data within an environment that is different
## from the current environment.
## This process of caching the inverse of the matrix creates more efficiency
## by avoiding the repeated computation process, which might be more costly
## and more memory-intensive.
## XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


## Write a short comment describing this function

## XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
## The "makeCacheMatrix" function will build up a new matrix for the purpose 
## of caching its inverse:
## The inverse matrix is cached inside the object 'inv', within the main 
## environment.
## XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        setMatrix <- function(y) {
                x <<- y
                inv <<- NULL
        }
        getMatrix <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(setMatrix = setMatrix,
             getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function

## XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
## The function "cacheSolve" calculates the inverse of the matrix that is 
# returned by the makeCacheMatrix function.
## XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$getMatrix()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
