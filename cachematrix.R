## Put comments here that give an overall description of what your
## functions do
# The following functions are used to create a special object that 
# stores a matrix and caches its inverse. 

## Write a short comment describing this function
# The first function, makeCacheMatrix creates a special “matrix”, 
# which is really a list containing a function to:
        # 1. set the value of the matrix
        # 2. get the value of the matrix
        # 3. set the value of the inverse
        # 4. get the value of the inverse

makeCacheMatrix <- function(a = matrix()) {
        n <- NULL
        set <- function(b) {
                a <<- b
                n <<- NULL
        }
        get <- function() a
        setinverse <- function(inverse) n <<- inverse
        getinverse <- function() n
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Write a short comment describing this function
#This function computes the inverse of the special “matrix” 
# returned by makeCacheMatrix above. If the inverse has 
# already been calculated (and the matrix has not changed), 
# then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(a, ...) {
        n <- a$getinverse()
        if (!is.null(n)) {
                message("getting cached data")
                return(n)
        }
        data <- a$get()
        n <- solve(data, ...)
        a$setinverse(n)
        n
        ## Return a matrix that is the inverse of 'n'
}

# this is example to test the functions
X <- matrix(c(2,3,4,7,4,6,8,9,7),3,3)
X1 <- makeCacheMatrix(X)
cacheSolve(X1)

# should return follwoing
#             [,1]        [,2]        [,3]
# [1,] -0.37681159 -0.01449275  0.44927536
# [2,]  0.21739130 -0.26086957  0.08695652
# [3,]  0.02898551  0.23188406 -0.18840580
