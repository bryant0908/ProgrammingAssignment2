## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        b<-NULL ## introduce an empty matrix
        set<- function(y){ ##save function
        x <<-y
        b<<-NULL
        }
        get<-function() x
        setInverse <- function(inverse)b<<-inverse ## saving the value of the inverse
        getInverse <- function()b ## returns the value of the inverse
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          b <- x$getInverse()
  if(!is.null(b)){ ## returns the recent saved value of the inverse if not empty
  message("Opening Cache")
  return(b)  
  }
  mat <- x$get()
  b <- solve(mat,...) ## computes for the inverse of the matrix
  x$setInverse(b) ## saves the inversed matrix
  b
}
