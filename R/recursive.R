tileSet <- function(ratio = 2/3, levels = 4, seed = 1) {
    tiles <- recursiveTiling(ratio, levels, seed)
    return(tiles)
}

recursiveTiling <- function(ratio, levels, seed) {
    level <- 1 
    tiles <- list(data.frame(
        x = c(0, 0, 1, 1), 
        y = c(0, 1 * ratio, 1 * ratio, 0), 
        name = 'center', 
        level = 1, 
        hash = 'initial'
    ))
    while (level <= levels) {
        level <- level + 1
        tiles <- levelIteration(tiles, level, seed)
    }
    return(tiles)
}

levelIteration <- function(tiles, level, seed) {
    # ------ Iterate through each tile
    iteration <- lapply(1:length(tiles), function(tileNb) {
        tile <- tiles[[tileNb]]
        tiled_tile <- tileIteration(tile, level, seed = tileNb*seed)
        return(tiled_tile)
    })
    # ------ Cat with mother tile
    tiles <- c(tiles, purrr::flatten(iteration))
    # ------ Return iteration 
    return(tiles)
}

tileIteration <- function(tile, level, seed) {
    # ------- random X and Y and hash
    `%>%` <- magrittr::`%>%`
    set.seed(seed) 
    hash <- stringi::stri_rand_strings(length = 10, n = 1)
    randX <- sample(seq(tile$x[1], tile$x[3], length.out = 100), 1)
    randY <- sample(seq(tile$y[1], tile$y[2], length.out = 100), 1)
    # ------- Divide tile in 4
    tiled <- list(
        data.frame(
            x = c(tile$x[1], tile$x[1], randX, randX),
            y = c(tile$y[1], randY, randY, tile$y[1]), 
            name = 'bl', 
            level = level, 
            hash = hash
        ), 
        data.frame(
            x = c(tile$x[1], tile$x[1], randX, randX),
            y = c(randY, tile$y[2], tile$y[2], randY), 
            name = 'tl', 
            level = level, 
            hash = hash
        ), 
        data.frame(
            x = c(randX, randX, tile$x[3], tile$x[3]),
            y = c(randY, tile$y[2], tile$y[2], randY), 
            name = 'tr', 
            level = level, 
            hash = hash
        ), 
        data.frame(
            x = c(randX, randX, tile$x[3], tile$x[3]),
            y = c(tile$y[1], randY, randY, tile$y[1]), 
            name = 'br', 
            level = level, 
            hash = hash
        )
    )
    # ------- Check tiles
    tiles <- filterTiles(tiles)
    # ------- Drop random tiles
    tiles <- dropTiles(tiles)
    # ------- Return tiled
    return(tiled)
}

