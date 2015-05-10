## testthat unit tests for cachematrix

test_that("matrix set/get works correctly", {
    original_matrix <- rbind(c(1, -1/4), c(-1/4, 1)) 
        
    m <- makeCacheMatrix()    
    m$set(original_matrix)
    
    expect_equal(m$get(), original_matrix)
})

test_that("matrix set via constructor works correctly", {
    original_matrix <- rbind(c(1, -1/4), c(-1/4, 1)) 
        
    m <- makeCacheMatrix(original_matrix)    
    
    expect_equal(m$get(), original_matrix)
})


test_that("inv matrix set/get works correctly", {
    original_matrix <- rbind(c(1, -1/4), c(-1/4, 1))
    inv_matrix <- solve(original_matrix)
    
    m <- makeCacheMatrix(original_matrix)
    m$setinv(inv_matrix)
    
    expect_equal(m$getinv(), inv_matrix)
})

test_that("cacheSolve delivers correct result", {
    original_matrix <- rbind(c(1, -1/4), c(-1/4, 1))
    inv_matrix <- solve(original_matrix)
    
    m <- makeCacheMatrix(original_matrix)
    solved <- cacheSolve(m)
    
    expect_equal(solved, inv_matrix)
})

test_that("cacheSolve does caching", {
    original_matrix <- rbind(c(1, -1/4), c(-1/4, 1))
    inv_matrix <- solve(original_matrix)
    
    m <- makeCacheMatrix(original_matrix)
    solved <- cacheSolve(m)    
    expect_equal(solved, inv_matrix)
    
    expect_message(solved <- cacheSolve(m), "getting cached data")   
    expect_equal(solved, inv_matrix)    
})
