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

## testing and checking. First I make 2 matrices, then I use the functions made above 
## to check their working:

matrix1 <- matrix(1:4, 2, 2)
inversematrix1 <- solve(matrix1)
# inverse of inverse = original matrix again, just a check:
solve(inversematrix1)


# should return the same matrix as inversematrix1
cacheSolve(makeCacheMatrix(matrix1))
print(inversematrix1)

# should return the same matrix as matrix1 (because inverse of inverse = original)
cacheSolve(makeCacheMatrix(inversematrix1))
print(matrix1)
