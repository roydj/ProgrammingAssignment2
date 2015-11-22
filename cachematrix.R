## FUNCTION LIST:
## makeCacheMatrix() : This function creates the matrix object that will be used for caching its inverse
## cacheSolve() : This function computes the inverse of the matrix created by makeCacheMatrix()
## testStub() : This function can be used as a stub to test the matrix inverse/caching functionality


######################
### CORE FUNCTIONs ###
######################

## This function creates the matrix object that will be used for caching its inverse
makeCacheMatrix <- function(d_Matrix = matrix())
{
  d_invMatrix <- NULL
  set <- function(y) 
  {
    d_Matrix <<- y
    d_invMatrix <<- NULL
  }
  get <- function() d_Matrix
  setInverse <- function(inverse) d_invMatrix <<- inverse
  getInverse <- function() d_invMatrix
  
  list(
    set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
}


## This function computes the inverse of the matrix created by makeCacheMatrix()
## And it also uses the caching strategy
cacheSolve <- function(x, ...)
{
  # Return a matrix that is the inverse of 'x'
  invM <- x$getInverse()
  if (!is.null(invM)) message("<< getting cached data >>")
  else
  {
    message("<< calculating data >>")
    mat <- x$get()
    invM <- solve(mat, ...)
    x$setInverse(invM)
    #invM
  }
  return(invM)
}


##########################
### TEST STUB FUNCTION ###
##########################

## This function can be used as a stub to test the matrix inverse/caching functionality
## The input to this function is the height of the square matrix
testStub = function(size)
{
  #creating a random square matrix based on the input size
  set.seed(1110201)
  r = rnorm(size*size)
  testMat = matrix(r, nrow = size, ncol = size)
  
  tempObj = makeCacheMatrix(testMat)
  
  start.time = Sys.time()
  message("<< invoking  inverse - 1st iteration >>")
  i1 <- cacheSolve(tempObj)
  dur = Sys.time() - start.time
  print(dur)
  #print(i1)
  
  start.time = Sys.time()
  message("<< invoking  inverse - 2nd iteration >>")
  i2 <- cacheSolve(tempObj)
  dur = Sys.time() - start.time
  print(dur)
  #print(i2)
}


##########################
### SAMPLE TEST OUTPUT ###
##########################

# > testStub(100)
# << invoking  inverse - 1st iteration >>
#   << calculating data >>
#   Time difference of 0 secs
# << invoking  inverse - 2nd iteration >>
#   << getting cached data >>
#   Time difference of 0 secs
#
# > testStub(1000)
# << invoking  inverse - 1st iteration >>
#   << calculating data >>
#   Time difference of 1.071815 secs
# << invoking  inverse - 2nd iteration >>
#   << getting cached data >>
#   Time difference of 0 secs
#
# > testStub(1500)
# << invoking  inverse - 1st iteration >>
#   << calculating data >>
#   Time difference of 3.29442 secs
# << invoking  inverse - 2nd iteration >>
#   << getting cached data >>
#   Time difference of 0 secs
# 
# > testStub(5000)
# << invoking  inverse - 1st iteration >>
#   << calculating data >>
#   Time difference of 1.9131 mins
# << invoking  inverse - 2nd iteration >>
#   << getting cached data >>
#   Time difference of 0 secs
