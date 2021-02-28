#' tileSet
#'
#' @export

tileSet <- function(ratio = 1, levels = 4, seed = 1, filter = TRUE, min.stretch = 0.04) {
    tiles <- recursiveTiling(ratio, levels, seed, filter, min.stretch)
    return(tiles)
}

#' recursiveTiling
#'
#' @export

recursiveTiling <- function(ratio, levels, seed, filter, min.stretch) {
    level <- 1 
    tiles <- list(data.frame(
        x = c(0, 0, 1, 1), 
        y = c(0, 1 * ratio, 1 * ratio, 0), 
        name = 'center', 
        level = 1, 
        hash = 'initial'
    ))
    attr(tiles[[1]], 'ranges') <- list(
        xrange = range(tiles[[1]]$x), yrange = range(tiles[[1]]$y)
    )
    while (level < levels) {
        level <- level + 1
        tiles <- levelIteration(tiles, level, seed, filter, min.stretch)
    }
    return(tiles)
}

#' levelIteration
#'
#' @export

levelIteration <- function(tiles, level, seed, filter, min.stretch) {
    # ------ Iterate through each tile
    new_tiles <- lapply(1:length(tiles), function(tileNb) {
        tile <- tiles[[tileNb]]
        tiled_tile <- tileIteration(tile, level, seed = level*tileNb*seed, filter, min.stretch)
        return(tiled_tile)
    })
    # ------ Cat with mother tile
    tiles <- purrr::flatten(new_tiles)
    # ------ Return iteration 
    return(tiles)
}

#' tileIteration
#'
#' @export

tileIteration <- function(tile, level, seed, filter, min.stretch) {
    # ------- random X and Y and hash
    `%>%` <- magrittr::`%>%`
    set.seed(seed) 
    randX <- sample(seq(tile$x[1], tile$x[3], length.out = 100), 1)
    randY <- sample(seq(tile$y[1], tile$y[2], length.out = 100), 1)
    # ------- Divide tile in 4, with random vertical/horizontal/cross splitting
    if (filter) {
        split <- sample(c('horiz', 'vertic', 'both'))[1]
    } 
    else {
        split <- 'both'
    }
    tiled <- splitTile(randX, randY, tile, level, seed, dir = split)
    # ------- Check that new tiles are big enough; if not, reverse to original tile
    if (!checkTileSize(tiled, min.stretch) & min.stretch > 0) {
        tiled <- list(tile)
    }
    # ------- Return tiled
    return(tiled)
}

#' splitTile
#'
#' @export

splitTile <- function(X, Y, tile, level, seed, dir = 'both') {
    set.seed(seed)
    hash <- stringi::stri_rand_strings(length = 10, n = 1)
    if (dir == 'vertic') {
        a <- data.frame(
            x = c(tile$x[1], tile$x[1], X, X),
            y = c(tile$y[1], tile$y[2], tile$y[2], tile$y[1]), 
            name = 'bl', 
            level = level, 
            hash = hash
        )
        b <- data.frame(
            x = c(X, X, tile$x[3], tile$x[3]),
            y = c(tile$y[1], tile$y[2], tile$y[2], tile$y[1]), 
            name = 'tr', 
            level = level, 
            hash = hash
        )
        attr(a, 'ranges') <- attr(b, 'ranges') <- attr(tile, 'ranges')
        tiled <- list(a, b)
    } 
    else if (dir == 'horiz' ) {
        a <- data.frame(
            x = c(tile$x[1], tile$x[1], tile$x[3], tile$x[3]),
            y = c(tile$y[1], Y, Y, tile$y[1]), 
            name = 'bl', 
            level = level, 
            hash = hash
        )
        b <- data.frame(
            x = c(tile$x[1], tile$x[1], tile$x[3], tile$x[3]),
            y = c(Y, tile$y[2], tile$y[2], Y), 
            name = 'tl', 
            level = level, 
            hash = hash
        )
        attr(a, 'ranges') <- attr(b, 'ranges') <- attr(tile, 'ranges')
        tiled <- list(a, b)
    }
    else if (dir == 'both' ){
        a <- data.frame(
                x = c(tile$x[1], tile$x[1], X, X),
                y = c(tile$y[1], Y, Y, tile$y[1]), 
                name = 'bl', 
                level = level, 
                hash = hash
        )
        b <- data.frame(
                x = c(tile$x[1], tile$x[1], X, X),
                y = c(Y, tile$y[2], tile$y[2], Y), 
                name = 'tl', 
                level = level, 
                hash = hash
        )
        c <- data.frame(
                x = c(X, X, tile$x[3], tile$x[3]),
                y = c(Y, tile$y[2], tile$y[2], Y), 
                name = 'tr', 
                level = level, 
                hash = hash
        )
        d <- data.frame(
                x = c(X, X, tile$x[3], tile$x[3]),
                y = c(tile$y[1], Y, Y, tile$y[1]), 
                name = 'br', 
                level = level, 
                hash = hash
        )
        attr(a, 'ranges') <- attr(b, 'ranges') <- attr(tile, 'ranges')
        attr(c, 'ranges') <- attr(d, 'ranges') <- attr(tile, 'ranges')
        tiled <- list(a,b,c,d)
    }
    else {
        tiled <- tile
        attr(tiled, 'ranges') <- attr(tile, 'ranges')
    }
    return(tiled)
}
