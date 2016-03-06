## Cached Inverse of a Matrix
## These two functions work together to speed up the calculation of
## the inverse of a matrix by caching it

## Creates a cache matrix, containing functions to:
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse
## - get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inver<- NULL
  set<-function(y)
  {
    x<<-y
    inver<<-NULL
  }
  get<-function()x
  set.inver<-function(solve) inver<<-solve
  get.inver<-function() inver
  list(set=set, get=get, set.inver=set.inver, get.inver=get.inver)
}



## Calculates the inverse of the matrix created using makeCacheMatrix
## but only if it hasn't been calculated before. Otherwise it returns
## the cache inverse
cacheSolve <- function(x, ...) {
        inver<-x$get.inver()
        if(!is.null(inver))
        {
          message("getting cached data")
          return (inver)
        }
        data<-x$get()
        inver<-solve(data,...)
        x$set.inver(inver)
        inver
}
