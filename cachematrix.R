## Functions to cache a matrix, avaoiding (computationally expensive) recalculation

## Function to store matrix to cache.
## Creates special matrix object which can cache its inverse

## Main steps:
## get the matrix
## set the inverse of the matrix
## get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL  ## create empty vector
  set <- function(y) {
    x <<- y ## pass x to parent environment
    i <<- NULL ## pass i to parent env.
  }
  get <- function() x  ## function to set, get matrix and its inverse
  setinverse <- function(solve) i <<- solve ## pass result to parent env.
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse) 
}

## Checks if the inverse of x exists and returns it if it does exist.

## check if inverse exists
## if exists, returns it; if not, calculates it
cacheSolve <- function(x = matrix(), ...) {   ## Return a matrix inverse of 'x'
  i <- x$getinverse() ## assign i through getting it from env.
  if(!is.null(i)) {  ## check if i (the inverse matrix) already exists
    message("getting cached data") ## if it does print message and return i
    return(i)
  }
  inverse <- x$get() ## if i did not exist, calculate it with solve()
  i <- solve(inverse, ...)
  x$setinverse(i) ## pass i to setinverse
  return(i)        ## return i
}

## TESTING DATA
a <- c(1, 2, 2)
b <- c(5, 1, 4)
c <- c(6, 2, 3)
ma <- rbind(a, b, c)

## TESTING FUNCTIONS
gh <- makeCacheMatrix(ma)
cacheSolve(gh)

## Testing solve() function inside main functions
## returns TRUE if calc. is fine
identical(solve(ma), cacheSolve(makeCacheMatrix(ma)))
