makeCacheMatrix <- function(x = matrix()) {
    #create a matrix using the makeCache matrix
    #cache the matrix using the cacheSolve matrix
    
    i <- NULL
    set = function(y) {
        x <<- y
        i <<- NULL
        
    }
    get = function() x
    setinv = function(inverse) i <<- inverse 
    getinv = function() i
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
    #The input to cacheSolve matrix should come from the matrix created using makeCacheMatrix()
 
    i = x$getinv()
    
    # if the inverse is already cached
    if (!is.null(i)){
        # return from the cache
        message("getting cached data")
        return(i)
    }
    
    # else, inverse it 
    mat.data = x$get()
    i = solve(mat.data, ...)
    
    # cache the inverse
    x$setinv(i)
    
    return(i)
}

testmatrixinv <- function(mat){

    temp = makeCacheMatrix(mat)
    
    start.time = Sys.time()
    cacheSolve(temp)
    dur = Sys.time() - start.time
    print(dur)
    
    start.time = Sys.time()
    cacheSolve(temp)
    dur = Sys.time() - start.time
    print(dur)
    
  
}

matrixinversetest<-function()
{
    set.seed(1110201)
    r = rnorm(1000000)
    mat1 = matrix(r, nrow=1000, ncol=1000)
    testmatrixinv(mat1)
  
   
}