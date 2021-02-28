tileImage <- function(img, max.tiles = 10) {
    `%>%` <- magrittr::`%>%`
    # -------- Import image
    im <- imager::as.cimg(jpeg::readJPEG(img))
    # -------- get Edges data.frame
    edges <- getEdges(im)
    # -------- Get nodes by binned coords
    ratio <- dim(im)[1]/dim(im)[2]
    nodes <- getNodes(im, edges)
    nodes$y <- nodes$y * ratio
    # -------- Get tiles using nodes 
    level <- 0
    tiles <- list(data.frame(
        x = c(0, 0, 1, 1), 
        y = c(0, 1 * ratio, 1 * ratio, 0), 
        name = 'center', 
        level = 1, 
        hash = 'initial'
    ))
    while (level < max.tiles & level < nrow(nodes)) {
        level <- level + 1
        X <- nodes$x[level]
        Y <- nodes$y[level]
        dir <- nodes$direction[level]
        # ------ Get tile to divide
        whichTile <- getTile(X, Y, tiles)
        # ------- Divide tile in 4
        tiled <- splitTile(X, Y, tiles[[whichTile]], dir)
        # ------ Cat with mother tile
        tiles <- c(tiles, tiled)
    }
    # -------- Return tiles
    return(tiles)
}

getEdges <- function(im) {
    `%>%` <- magrittr::`%>%`
    df <- imager::cannyEdges(im) %>% 
        as.data.frame() %>% 
        dplyr::select(x, y) %>% 
        setNames(c('y', 'x')) %>%
        dplyr::mutate(isEdge = TRUE) %>% 
        dplyr::distinct()
    res <- matrix(FALSE, nrow = dim(im)[1], ncol = dim(im)[2]) %>%  
        as.data.frame() %>% 
        dplyr::mutate(y = 1:nrow(.)) %>%
        tidyr::gather(x, isEdge, -y) %>% 
        dplyr::mutate(x = as.numeric(stringr::str_replace(x, 'V', ''))) %>% 
        dplyr::left_join(df, c("y", "x")) %>% 
        dplyr::select(-isEdge.x) %>% 
        dplyr::mutate(
            isEdge = ifelse(is.na(isEdge.y), FALSE, TRUE),
            y = max(y) - y + 1,
            .keep = "unused"
        )
    return(res)
}

getNodes <- function(im, edges, nbins = 100) {
    `%>%` <- magrittr::`%>%` 
    # --- binarize the edges and get the top nodes
    list_coords <- edges %>% 
        dplyr::filter(isEdge) %>%
        dplyr::select(-isEdge) %>%
        dplyr::mutate(
            x = cut(x, seq(1, dim(im)[2], length.out = nbins+1), include.lowest = TRUE),
            y = cut(y, seq(1, dim(im)[1], length.out = floor(nbins/dim(im)[2]*dim(im)[1])+1), include.lowest = TRUE)
        )
    # --- Filter to nodes that are significant
    x.threshold <- quantile(sort(summary(list_coords$x)), 0.7)
    y.threshold <- quantile(sort(summary(list_coords$y)), 0.7)
    x.counts <- table(list_coords$x)[table(list_coords$x) >= x.threshold]
    y.counts <- table(list_coords$y)[table(list_coords$y) >= y.threshold]
    list_coords_2 <- list(
        x = interval2num(names(x.counts)[order(x.counts, decreasing = TRUE)])/dim(im)[2], 
        y = interval2num(names(y.counts)[order(y.counts, decreasing = TRUE)])/dim(im)[1]
    )
    list_coords_2$rank <- 1:lengths(list_coords_2)[1]

    # --- Get pairs of coords
    node <- 1
    coords <- data.frame('x' = list_coords_2$x[1], 'y' = list_coords_2$y[1])
    while (node <= nbins) {
        node <- node + 1
        # --- Set x
        nodex <- node
        x <- list_coords_2$x[nodex]
        # --- Set y
        nodey <- node
        y <- list_coords_2$y[nodey]
        # --- Set coords
        coords[node, ] <- c(x, y)
    }
    # --- Remove coords that are not in pairs
    nodes <- coords %>% 
        dplyr::filter(!is.na(x) & !is.na(y)) %>% 
        dplyr::distinct() 
    nodes$direction <- getNodesDirection(nodes, edges)
    return(nodes)
}

getNodesDirection <- function(nodes, edges) {
    directions <- sapply(1:nrow(nodes), function(K) {
        coords <- c(round(nodes$x[K]*dim(im)[2]), round(nodes$y[K]*dim(im)[2]))
        xvals.vert <- c((coords[1] - 5):(coords[1] + 5))
        yvals.vert <- c((coords[2] - 30):(coords[2] + 30))
        xvals.horiz <- c((coords[1] - 30):(coords[1] + 30))
        yvals.horiz <- c((coords[2] - 5):(coords[2] + 5))
        vert <- sum(edges$isEdge[edges$x %in% xvals.vert & edges$y %in% yvals.vert])
        horiz <- sum(edges$isEdge[edges$x %in% xvals.horiz & edges$y %in% yvals.horiz])
        if (vert > 1.5 * horiz) {
            dir <- 'vert'
        }
        else if (horiz > 1.5 * vert) {
            dir <- 'horiz'
        } 
        else {
            dir <- 'both'
        }
        return(dir)
    })
    return(directions)
}

#' getTile
#' 
#' @export

getTile <- function(X, Y, tiles) {
    res <- unlist(lapply(tiles, function(tile) {
        inXs <- dplyr::between(X, range(tile$x)[1], range(tile$x)[2])
        inYs <- dplyr::between(Y, range(tile$y)[1], range(tile$y)[2])
        res <- ifelse(inXs + inYs == 2, TRUE, FALSE)
        return(res)
    }))
    res <- which(res)[sum(res)]
    return(res)
}

#' checkImg
#' 
#' @export

checkImg <- function(im, edges, nodes, tiles) {
    im_plot <- as.data.frame(im, wide = "c") %>% 
        dplyr::mutate(rgb.val=rgb(c.1,c.2,c.3)) %>% 
        ggplot2::ggplot(ggplot2::aes(x = y, y = x)) + 
        ggplot2::geom_raster(ggplot2::aes(fill = rgb.val)) +
        ggplot2::scale_fill_identity() + 
        ggplot2::scale_y_reverse() + 
        ggplot2::theme_void() + 
        ggplot2::coord_fixed(expand = FALSE)
    edges_plot <- edges[edges$isEdge,] %>%
        ggplot2::ggplot(ggplot2::aes(x, y)) + 
        ggplot2::geom_point(col = 'white', size = 0.001) + 
        # ggplot2::theme_void() + 
        ggplot2::coord_fixed(xlim = c(1, dim(im)[2]), ylim = c(1, dim(im)[1]), expand = FALSE) + 
        ggplot2::theme(panel.background = ggplot2::element_rect('#383838'))
    nodes_plot <- nodes %>%
        dplyr::mutate(
            x = scales::rescale(x, c(min(x) * dim(im)[2], max(x) * dim(im)[2])), 
            y = scales::rescale(y, c(min(y) * dim(im)[1], max(y) * dim(im)[1])),
            rank = 1:nrow(.)
        ) %>% 
        ggplot2::ggplot(ggplot2::aes(x, y, col = rank)) + 
        ggplot2::geom_point(size = 0.001) + 
        ggplot2::theme_void() + 
        ggplot2::scale_colour_gradient(low = '#ffbb55', high = '#740101') +
        ggplot2::coord_fixed(xlim = c(1, dim(im)[2]), ylim = c(1, dim(im)[1]), expand = FALSE) + 
        ggplot2::theme(panel.background = ggplot2::element_rect('#8b8b8b')) + 
        ggplot2::theme(legend.position = 'none')
    tile <- plotTiles(tiles)
}
