plotTiles <- function(tiles, white.density = 0.5) {
    
    # ------ Plot dataframe
    df <- tiles %>% 
        do.call(rbind, .) %>%
        tidyr::unite(id, level, name, hash, sep = "_", remove = FALSE, na.rm = FALSE) %>% 
        dplyr::filter(id != '1_center_initial') %>%
        dplyr::mutate(
            id = factor(id, levels = unique(id)), 
            fill = sample(1:4, dplyr::n(), replace = TRUE), 
            fill = ifelse(1:length(fill) %in% sample(1:length(fill), size = length(fill)*white.density), '1', fill), 
            fill = factor(fill, levels = 1:4), 
            size = max(level) - level, 
            size = 0.01
        )

    # ------ Plot art
    p <- df %>%
        ggplot2::ggplot(ggplot2::aes(x = x, y = y, fill = fill, group = id)) +
        ggplot2::geom_polygon(col = '#0a0815') + 
        ggplot2::theme_void() + 
        ggplot2::theme(legend.position = 'none') + 
        ggplot2::coord_fixed() + 
        ggplot2::scale_fill_manual(values = c('#f0eeda', '#f7d440', '#c92c16', '#054eab'))
    
    return(p)
}