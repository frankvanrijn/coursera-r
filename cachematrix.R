## Two functions that make it possible to store the inverse
## of an always inversible (=square!) matrix.

## This function contains the four function set, get, setsolve, getsolve.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## Changes the matrix stored in the main function
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
    ## Returns matrix x stored in main function
  get <- function() x
    ## setsolve stores it in the variable 'm' into the main function makeCacheMatrix
  setsolve <- function(solve) m <<- solve
    ## getsolve returns it
  getsolve <- function() m
    ## line 17 stores the four functions so they can be accessed by subsetting this list
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
        ## Verifies if m is in the memory and if so returns it
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
        ## If value of m was not in the memory, the following happens
        ## data gets the matrix 
  data <- x$get()
        ## m calculates the inverse of that matrix
  m <- solve(data, ...)
        ## and the inverse is stored in object assigned with function makeCacheMatrix
  x$setsolve(m)
  m
}

## Evaluate code in the following steps:
## 1) store function in a to be able to access. In this case I choose 2x2 matrix 
##    filled with 5,6,7,8.
## >a<-makeCacheMatrix(matrix(5:8, 2, 2))
## 2) get familiar with the data that is used, a$get() will print the matrix
## >a$get()
## 3) find inverse
## >cachesolve(a)
## 4) find inverse again. If it was stored properly, "getting cached data" should show
## >cachesolve(a)
## 5) start at step 1) with a different matrix and check if this goes well too
##    this step doublechecks if setsolve function is calculated again if the value changes
