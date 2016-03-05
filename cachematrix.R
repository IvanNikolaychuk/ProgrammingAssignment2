## This functions are designed to compute the inverse from the given matrix.
## There is a caching mechanism, that prevents computing the inverse from the matrix
## each time (if matrix didn't change)

## This function is designed to deal with matrix and it's cached inverse-version.
## Here we can set and get a matrix itself and it's cached inverse-version.
## If a matrix changes to a new one, we clean the cache.
## Note, that before setting a matrix we validate if matrix is a square one.
## We do it, because solve function takes a determinant from the given matrix,
## and we can't take determinant form a non-square matrix
makeCacheMatrix <- function(matrix = matrix()) {
        cached <- NULL
        checkMatrixIsSquare(matrix)
        
        set <- function(newMatrix) {
                checkMatrixIsSquare(newMatrix)
                
                if (identical(matrix, newMatrix)) {
                        # if matrix are identical,
                        #  no need to clean cache and set new matrix
                        return()
                }
                matrix <<- newMatrix
                cached <<- NULL
                message("Matrix changed, cleaning cache")
        }
        
        get <- function()
                matrix
        
        setCached <- function(newCached)
                cached <<- newCached
        
        getCached <- function()
                cached
        
        
        
        list(
                set = set,
                get = get,
                setCached = setCached,
                getCached = getCached
        )
}


## Returns the inversed matrix of x, where x is a square matrix.
## Computes the inversed matrix only if there is a need to do it (if cache is empty)
## If there is a need to compute the inversed matrix, computes it and fills 
## cache with the resul of computation

cacheSolve <- function(x) {
        cached <- x$getCached()
        if (!is.null(cached)) {
                message("Getting data from cache")
                return(cached)
        }
        matrix <- x$get()
        
        inversedMatrix <- solve(matrix)
        x$setCached(inversedMatrix)
        
        return(inversedMatrix)
}


# The goal of this function is to validate where matrix is square or not.
# We should do it every time matrix in makeCacheMatrix changes, because
# feather we will take solve from this matrix. And it is not possible to
# do it for non-square matrix. 
# I know, that this assignment required writing of only 2 functions, but
# validating data before doing something is a good practise.
checkMatrixIsSquare = function(m) {
        rows = nrow(m)
        cols = ncol(m)
        if (cols != rows) {
                stop(
                        "We are working only with square matrix, as it is not
                        possible to get determinant from a non-square matrix"
                )
        }
 }