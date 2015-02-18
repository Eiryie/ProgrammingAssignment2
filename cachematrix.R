## Together, the makeCacheMatrix and cacheSolve functions create a faster method 
## for calculating the inverse of potentially large matrices by taking advantage
## of R's lexical scoping rules. 

## makeCacheMatrix defines and runs functions to:
  ## set the value of a matrix
  ## get the value of the matrix
  ## set the value of the inverted matrix
  ## get the value of the inverted matrix

makeCacheMatrix  <-  function(x = matrix()) {
  m  <-  NULL	## creates an empty object to hold the inverted matrix result
  set <- function(y){	
    x <<- y		## replaces any existing cached matrix 'x' from a previous 
    ## call of makeCacheMatrix in case the data has changed
    m <<- NULL	## resets m to null for the new call of makeCacheMatrix
    }
  get <- function()  x	## returns the matrix 'x' and assigns as 'get'
  setmatrix <- function(solve)  m <<- solve		## applies solve function 
  ## to invert the matrix and caches it as 'm' 
  getmatrix <- function() m	## returns the inverted matrix.  
  list(set = set, get = get, setmatrix = setmatrix,  getmatrix = getmatrix) 	
  ## returns a list of the functions that will be passed to cacheSolve
  }

## cacheSolve checks to see if the inverted matrix has already been created.
## If so, it returns the cached matrix.
## If not, it calculates and caches a new inverted matrix.

cacheSolve <- function(x=matrix(), ...) {
  m <- x$getmatrix()	## calls the matrix defined in makeCacheMatrix 
  if(!is.null(m)){		## if m is not null (exists and equals current data), 
    ## initiates cache sequence
    message("getting cached data")	## informs user function is pulling cache data
    return(m)	## returns the m matrix from cache and ends the function
  }
  matrix  <-  x$get()	## if m is null (didn't exist or changed data), 
  ## calls a new matrix from makeCacheMatrix
  m <- solve(matrix, ...)	## applies solve function to invert the new matrix 
  x$setmatrix(m)	## caches the new matrix
  m	## returns the new inverted matrix
}
