# Normalize vector of numbers
# ie when normalizing variables for use in Knn Algo
# input -- vector of numbers
# output -- vector of numbers of same length with min zero and max 1
normalizeVar <- function(x) {
    num <- x - min(x)
    denom <- max(x) - min(x)
    return (num/denom)
}