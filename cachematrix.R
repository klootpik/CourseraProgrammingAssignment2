## Put comments here that give an overall description of what your
## functions do

## The function 'makeCacheMatrix' creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  #  setmean <- function(mean) m <<- mean
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The function 'cacheSolve' function computes the inverse of the special "matrix" returned by 
## 'makeCacheMatrix' above. If the inverse has already been calculated (and the matrix has not 
## changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

## Testing and checking from here. First I make 2 matrices, then I use the functions made above and do some things
## to check if they work correctly. That is, returning an inverse matrix and using cache data when available.

TestMatrix <- matrix(1:4, 2, 2)
TestInverseMatrix <- solve(TestMatrix)
# inverse of inverse = original matrix again, just a little check. Executing of following two lines should return the
# same result:
solve(TestInverseMatrix)
TestMatrix

# making two lists using 'makeCacheMatrix'and the two matrices above:
ListTestMatrix <- makeCacheMatrix(TestMatrix)
ListTestInverseMatrix <- makeCacheMatrix(TestInverseMatrix)

# to check if cacheSolve indeed returns the inverse of TestMatrix, also TestInverseMatrix is printed. 
# The results of executing the following two lines should be the same:
cacheSolve(ListTestMatrix)
print(TestInverseMatrix)

## as well as vice versa:
cacheSolve(ListTestInverseMatrix)
print(TestMatrix)

## At last I check whether indeed the cache data is returned, when the same matrix list has been used 
## earlier as an argument in the cacheSolve function:

cacheSolve(ListTestMatrix)    # result: inverse matrix + message 'getting cached data'
cacheSolve(ListTestMatrix)    # result: inverse matrix + message 'getting cached data'
cacheSolve(ListTestInverseMatrix)  # result: inverse matrix + message 'getting cached data'
