## The following functions are used to calculate the inverse of an
## invertible matrix, and cache the result for faster subsequent
## retrieval.
##
## Example:
##
## > my_matrix <- makeCacheMatrix(matrix(c(1,0,5,2,1,6,3,4,0), 3, 3))
##
## > my_matrix$get()
##      [,1] [,2] [,3]
## [1,]    1    2    3
## [2,]    0    1    4
## [3,]    5    6    0
##
## > cacheSolve(my_matrix)
##      [,1] [,2] [,3]
## [1,]  -24   18    5
## [2,]   20  -15   -4
## [3,]   -5    4    1
##
#### A subsequent call to cacheSolve(my_matrix) will get the cached
#### value of the inverse of the matrix, rather than recalculating:
##
## > cacheSolve(my_matrix)
##   getting cached data
##      [,1] [,2] [,3]
## [1,]  -24   18    5
## [2,]   20  -15   -4
## [3,]   -5    4    1
##
#### We can replace the matrix with a new one.
#### Set the matrix to the inverse of itself:
##
## > my_matrix$set(cacheSolve(my_matrix))
##   getting cached data
##
## > my_matrix$get()
##      [,1] [,2] [,3]
## [1,]  -24   18    5
## [2,]   20  -15   -4
## [3,]   -5    4    1
##
#### Since we haven't calculated the inverse of this
#### new matrix yet, the first call to cacheSolve won't
#### find it in the cache:
##
## > cacheSolve(my_matrix)
##      [,1] [,2] [,3]
## [1,]    1    2    3
## [2,]    0    1    4
## [3,]    5    6    0
##
#### But subsequent calls will get the cached value:
##
## > cacheSolve(my_matrix)
##   getting cached data
##      [,1] [,2] [,3]
## [1,]    1    2    3
## [2,]    0    1    4
## [3,]    5    6    0


## Creates a cacheable matrix
makeCacheMatrix <- function(x = matrix()) {
        matrix_inverse <- NULL
        set <- function(y) {
                x <<- y
                matrix_inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) matrix_inverse <<- solve
        getinverse <- function() matrix_inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Find the inverse of a cacheable matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matrix_inverse <- x$getinverse()

        ## If the inverse of this matrix was in the cache,
        ## return the cached value to avoid an expensive
        ## solve operation
        if(!is.null(matrix_inverse)) {
                message("getting cached data")
                return(matrix_inverse)
        }
 
        ## The inverse was not cached, so find the inverse
        ## and cache the result:
        data <- x$get()
        matrix_inverse <- solve(data, ...)
        x$setinverse(matrix_inverse)
        matrix_inverse
}
