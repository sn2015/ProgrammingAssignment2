## These functions can prevent time consuming calculations 
## caching invers matrix for a given matrix and return solution
## from the memory when needed 
## Check if invers matrix for the given matrix already exists
## return solution if it was earlier calculeted, 
## calculate solution and store it if matrix "x" is first time here 

## this function has as input matrix "x"
## as output a special object (a list) with 4 functions 
## which set, store and return the matrix "x" and the invers matrix "s"   

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set=set, get=get, 
             setsolve=setsolve,
             getsolve=getsolve)
}

## This funtion checks if invers matrix for the given matrix already exists in 
## previouse function 
## "s" is an invers matrix for "x"
## returns solution if it was earlier calculeted, 
## calculate solution if this matrix is first time here 

cacheSolve <- function(x, ...) { ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
