# Basic color palettes to make available across all Giotto modules

#' @title getRainbowColors
#' @description Returns a number of rainbow colors spaced around the spectrum.
#' Only 100 unique colors will be supplied after which they are recycled.
#' @param n number of colors wanted
#' @return character vector of hexadecimal rainbow colors
#' @export
getRainbowColors <- function(n) {
  n <- as.integer(n)
  if (n < 1L) .gstop("'n' colors wanted must be at least 1\n")
  rcols <- rev(grDevices::rainbow(100L, start = 0.1, end = 0.9))

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
#' @description Returns a number of distinct colors based on the RGB scale
#' @param n number of colors wanted
#' @return character vector of hexadecimal distinct colors
#' @export
getDistinctColors <- function(n) {
  package_check('RColorBrewer')
  if(n < 1) .gstop("'n' colors wanted must be at least 1\n")

  qual_col_pals <- RColorBrewer::brewer.pal.info[RColorBrewer::brewer.pal.info$category == 'qual',]
  col_vector <- unique(unlist(mapply(RColorBrewer::brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals))));

  if(n > length(col_vector)) {

    # get all possible colors
    all_colors = grDevices::colors()
    all_colors_no_grey = grep(x = all_colors, pattern = 'grey|gray', value = T, invert = T)
    grey_colors = grep(x = all_colors, pattern = 'grey', value = T, invert = F)
    admitted_grey_colors = grey_colors[seq(1, 110, 10)]
    broad_colors = c(all_colors_no_grey, admitted_grey_colors)

    set.seed(1234)
    on.exit(set.seed(Sys.time()))
    # if too many colors requested, warn about recycling
    if(n > length(broad_colors)) {
      warning('\n not enough unique colors in R, maximum = 444 \n')
      col_vector = sample(x = broad_colors, size = n, replace = TRUE)
    } else {
      col_vector = sample(x = broad_colors, size = n, replace = FALSE)
    }

  } else {

    xxx <- grDevices::col2rgb(col_vector);
    dist_mat <- as.matrix(stats::dist(t(xxx)));
    diag(dist_mat) <- 1e10;
    while (length(col_vector) > n) {
      minv <- apply(dist_mat,1,function(x)min(x));
      idx <- which(minv==min(minv))[1];
      dist_mat <- dist_mat[-idx, -idx];
      col_vector <- col_vector[-idx]
    }

  }
  return(col_vector)
}
