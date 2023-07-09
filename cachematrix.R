### In summary, this code allows you to create a cache for matrix inversion. 
### When you call cacheSolve on a makeCacheMatrix object, it checks if the inverse has already been computed and cached. 
### If it has, it returns the cached value. If not, it computes the inverse, caches it, and returns the result.

makeCacheMatrix <- function(x=matrix()){
        inversion <- NULL
        set <- function(y){
                x<<-y
                inversion<<-NULL
        }
        get_matrix <- function() x
        set_inverse <- function(Inverse) inversion <<- Inverse
        get_inverse <- function() inversion
        list(set = set, get_matrix = get_matrix,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}

cacheSolve <- function(x, ...) {
        inversion <- x$get_inverse()
        if (!is.null(inversion)) {
                message("Getting the cached data!")
                return(inversion)
        }
        data <- x$get_matrix()
        inversion <- solve(data, ...)
        x$set_inverse(inversion)
        inversion
}
