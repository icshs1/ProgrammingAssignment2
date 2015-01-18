## This is a source program for Assigement 2 of R-Programming, Coursera
## It is progammed by Hyunsik Shim

## This is example using the following functions 
## a<-matrix(1:4,2,2) ## a1<-makeCacheMatrix(a) 
## cashSolve(a1)  
##first try - give inverse of a matrix 
## cashSolve(a1) 
##second try - give caching message 
## This function creates a special "matrix" object that can cache its inverse 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <- y
    m <- NULL
  }
  get <- function() x
  set_inverse_matrix <- function(inverse_matrix) m <- inverse_matrix
  get_inverse_matrix <- function() m
  list(set = set, get = get,
       set_inverse_matrix = set_inverse_matrix,
       get_inverse_matrix = get_inverse_matrix)        
  
}
## This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$get_inverse_matrix()
  if(!is.null(m)) { ## If the inverse has already been calculated 
    message("getting cached matrix")
    return(m)  ## retrieve the inverse from the cache.
  }
  ## calculate inverse matrix using solve()
  data <- x$get()    
  m <- solve(data)
  x$set_inverse_matrix(m)
  m        
} 

