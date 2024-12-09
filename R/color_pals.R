# Basic color palettes to make available across all Giotto modules

#' @title getRainbowColors
#' @description Returns a number of rainbow colors spaced around the spectrum.
#' Only 100 unique colors will be supplied after which they are recycled.
#' @param n numeric. Number of colors wanted
#' @param slim numeric. Saturation. If two values are provided, a random
#' uniform distribution with the two values as min and max will be used.
#' @param vlim numeric. Value. If two values are provided, a random uniform 
#' distribution with the two values as min and max will be used.
#' @param seed integer. seed to use when randomizing saturation and value.
#' Default is 1234.
#' @return character vector of hexadecimal rainbow colors
#' @examples
#' getRainbowColors(100)
#' getRainbowColors(10, slim = c(0.5,1), vlim = c(0.3, 1))
#' getRainbowColors(10, slim = c(0.5,1), vlim = c(0.3, 1), seed = 11)
#' @export
#' @family basic color palette functions
getRainbowColors <- function(n, slim = 1, vlim = 1, seed = 1234) {
    if (length(slim) == 1) slim <- rep(slim, 2)
    if (length(vlim) == 1) vlim <- rep(vlim, 2)
    
    gwith_seed(seed = seed, {
        s <- runif(100, min = slim[1], max = slim[2])
        v <- runif(100, min = vlim[1], max = vlim[2])
    })

    n <- as.integer(n)
    if (n < 1L) .gstop("'n' colors wanted must be at least 1\n")
    rcols <- rev(
        grDevices::rainbow(100L, s = s, v = v, start = 0.1, end = 0.9)
    )

    if (n < 100L) {
        return(rcols[seq(1L, 100L, 100L / n)][seq(n)])
    }
    if (n == 100L) {
        return(rcols)
    }
    if (n > 100L) {
        return(rep(rcols, length.out = n))
    }
}





#' @title getDistinctColors
#' @description Returns a number of distinct colors based on the RGB scale.
#' When fewer than 444 colors are requested, colors are returned in an order
#' that maximizes differences. When more than 444 colors are requested,
#' a recycled sampling of the available colors is returned.
#' @param n number of colors wanted
#' @param seed seed to use when randomizing color order. Default is 1234
#' @return character vector of hexadecimal distinct colors
#' @export
#' @examples
#' getDistinctColors(500)
#' getDistinctColors(500, seed = 1)
#' @family basic color palette functions
getDistinctColors <- function(n, seed = 1234) {
    package_check("RColorBrewer")
    if (n < 1) .gstop("'n' colors wanted must be at least 1\n")

    qual_col_pals <- RColorBrewer::brewer.pal.info[
        RColorBrewer::brewer.pal.info$category == "qual",
    ]
    col_vector <- unique(unlist(mapply(
        RColorBrewer::brewer.pal,
        qual_col_pals$maxcolors,
        rownames(qual_col_pals)
    )))

    if (n > length(col_vector)) {
        # get all possible colors
        all_colors <- grDevices::colors()
        all_colors_no_grey <- grep(
            x = all_colors, pattern = "grey|gray",
            value = TRUE, invert = TRUE
        )
        grey_colors <- grep(
            x = all_colors, pattern = "grey",
            value = TRUE, invert = FALSE
        )
        admitted_grey_colors <- grey_colors[seq(1, 110, 10)]
        broad_colors <- c(all_colors_no_grey, admitted_grey_colors)

        local_seed(seed)
        # if too many colors requested, warn about recycling
        if (n > length(broad_colors)) {
            warning("\n not enough unique colors in R, maximum = 444 \n")
            col_vector <- sample(x = broad_colors, size = n, replace = TRUE)
        } else {
            col_vector <- sample(x = broad_colors, size = n, replace = FALSE)
        }
    } else {
        xxx <- grDevices::col2rgb(col_vector)
        dist_mat <- as.matrix(stats::dist(t(xxx)))
        diag(dist_mat) <- 1e10
        while (length(col_vector) > n) {
            minv <- apply(dist_mat, 1, function(x) min(x))
            idx <- which(minv == min(minv))[1]
            dist_mat <- dist_mat[-idx, -idx]
            col_vector <- col_vector[-idx]
        }
    }
    return(col_vector)
}







#' @title Create color scaling for a single color starting from black
#' @name getMonochromeColors
#' @param col hexadecimal color to scale scale towards
#' @param n number of colors to request in monochrome palette
#' @inheritDotParams grDevices::colorRampPalette -colors
#' @returns character vector
#' @examples
#' getMonochromeColors("green", n = 100)
#' @export
#' @family basic color palette functions
getMonochromeColors <- function(col, n = 256L, ...) {
    grDevices::colorRampPalette(colors = c("black", col), ...)(n)
}
