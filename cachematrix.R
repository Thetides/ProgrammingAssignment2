## Cache givin matrix and find the inverse of the cached matrix

## Caches matrix

makeCacheMatrix <- function(x = matrix()) {
  
  m<-NULL
  ## set the value of the matrix globally
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  ## getting the value of the matrix
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}
## Inverses the cached matrix

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
