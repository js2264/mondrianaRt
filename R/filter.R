#' checkTileSize
#' 
#' @export

checkTileSize <- function(tiled, min.stretch) {
    xrange <- min(sapply(tiled, function(tile) diff(range(tile$x))))
    yrange <- min(sapply(tiled, function(tile) diff(range(tile$y))))
    xOK <- {xrange / diff(attributes(tiled[[1]])$ranges$xrange)} >= min.stretch
    yOK <- {yrange / diff(attributes(tiled[[1]])$ranges$yrange)} >= min.stretch
    if (xOK & yOK) {
        isLargeEnough <- TRUE
    }
    else {
        isLargeEnough <- FALSE
    }
    return(isLargeEnough)
}