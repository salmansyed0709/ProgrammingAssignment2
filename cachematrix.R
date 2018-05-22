## Matrix inversion is usually a costly computation and there may be some benefit to
## caching the inverse of a matrix rather than compute it repeteadly

## Below are 2 functions that can be used to create a special object that 
##stores a matrix and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }

  get<-function() x
  setInverse<-function(inverse) inv<<-inverse
  getInverse<-function() inv
  list(set=set, get = get, setInverse=setInverse, getInverse=getInverse)
  
}


# The given function will always returns the inverse of the matrix.
# It will check first wather the inverse of the given matrix has been computed before or not. 
# If it inverse already been calculated it will get the result and skip the computation. 
# If its not been calculated then it will compute the inverse and will set the value in the cache via
# setInverse function.

# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getInverse()
  if(!is.null(inv)){
    message("Getting cached data")
    return(inv)
  }
  matrix<-x$get()
  inv<-solve(matrix, ...)
  x$setInverse(inv)
  inv
}

#Checking with eample

#x = rbind(c(1, -1/2), c(-1/2, 1))
#m = makeCacheMatrix(x)

#m$get()

#      [,1] [,2]
#[1,]  1.0 -0.5
#[2,] -0.5  1.0

#cacheSolve(m)

#        [,1]      [,2]
#[1,] 1.3333333 0.6666667
#[2,] 0.6666667 1.3333333
