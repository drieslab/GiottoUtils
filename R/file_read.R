#' @name file_extension
#' @title Get file extension
#' @description
#' Get the file extension from a provided filepath
#' @param file character. Filepath
#' @returns character
#' @examples
#' file_extension("my_file.txt")
#'
#' @export
file_extension <- function(file) {
    ex <- strsplit(basename(file), split = ".", fixed = TRUE)[[1L]]
    return(ex[-1])
}


#' @title Generate file manifest list from a directory
#' @name dir_manifest
#' @description Create a `list` of full filepaths (character) that are named
#' by with the respective `[basename()]`. Allows easy `$` exploration and
#' indexing of items.\cr
#' All params are directly passed to `[list.files()]` except
#' for `full.names`. `[list.files()]` also normally returns both actual files
#' and directories when `recursive = FALSE`, but this function specifically
#' tests if items are existing files and not directories with
#' `file_test(op = -f)` and fully obeys that flag in all cases.
#' @param path a character vector of full path names; the default corresponds
#' to the working directory, `[getwd()]`. Tilde expansion (see [path.expand])
#' and [`normalizePath()`] are performed. Missing values will be ignored.
#' Elements with a marked encoding  will be converted to the native encoding
#' (and if that fails, considered non-existent).
#' @param pattern an optional regular expression. Only file names which match
#' the regular expression will be returned.
#' @param all.files a logical value. If `FALSE`, only the names of visible
#' files are returned (following Unix-style visibility, that is files whose
#' name does not start with a dot). If `TRUE`, all file names will be returned.
#' @param recursive logical. Should the listing recurse into directories?
#' @param ignore.case logical. Should pattern-matching be case-insensitive?
#' @param include.dirs logical. Should subdirectory names be included in
#' recursive listings?
#' @param no.. logical. Should both `"."` and `".."` be excluded also from
#' non-recursive listings?
#' @param as.list logical. Should output be a list or a named character vector
#' @examples
#' dir_manifest()
#' @returns full and normalized filepaths named by the file basename as either
#' a list (default) or if `as.list = FALSE`, a character vector.
#' @export
dir_manifest <- function(
        path = ".",
        pattern = NULL,
        all.files = FALSE,
        recursive = FALSE,
        ignore.case = FALSE,
        include.dirs = FALSE,
        no.. = FALSE,
        as.list = TRUE) {
    a <- get_args_list(keep = c(
        "path", "pattern", "all.files", "recursive", "ignore.case",
        "include.dirs", "no.."
    ))
    a$full.names <- TRUE
    fullpaths <- do.call("list.files", args = a)
    fullpaths <- normalizePath(fullpaths)
    if (include.dirs == FALSE) {
        is_file <- file_test(op = "-f", x = fullpaths)
        fullpaths <- fullpaths[is_file]
    }
    names(fullpaths) <- basename(fullpaths)
    if (as.list) fullpaths <- as.list(fullpaths)
    return(fullpaths)
}


#' @title Fread specific rows based on column matches
#' @name read_colmatch
#' @aliases fread_colmatch
#' @description
#' Deprecated. Do not use
#' @param file path to file to load
#' @param col name of col to match from
#' @param sep grep term to match as column delimiters within the file
#' @param values_to_match values in \code{col} to match given as a vector
#' @param drop Vector of column names or numbers to drop, keep the rest.
#' @param verbose be verbose
#' @param ... additional parameters to pass to [arrow::open_delim_dataset()]
#' @keywords internal
#' @returns A tibble
#' @examples
#' f <- file.path(tempdir(), "my_file.csv")
#' x <- data.frame(a = c("a", "b", "c"), b = 1:3, c = 5:7)
#' write.csv(x, f)
#' read_colmatch(f, col = "a", values_to_match = c("a", "c"))
#' read_colmatch(f, col = "a", values_to_match = c("a", "c"), drop = c(1, 4))
#' read_colmatch(f, 
#'     col = "a", values_to_match = c("a", "c"), drop = c("V1", "b")
#' )
#' unlink(f)
#'
#' @export
read_colmatch <- function(file,
    col,
    sep = NULL,
    values_to_match,
    drop = NULL,
    verbose = FALSE,
    ...) {
    # check dependencies
    package_check("dplyr")
    .arrow_codec_check(file)

    file <- normalizePath(file)
    # get colnames

    if (is.null(sep)) {
        filename <- basename(file)
        if (grepl(pattern = ".csv", x = filename)) {
            sep <- ","
        } else if (grepl(pattern = ".tsv", x = filename)) {
            sep <- "\t"
        } else {
            stop("read_colmatch: sep param cannot be guessed", call. = FALSE)
        }
    }

    a <- arrow::open_delim_dataset(file,
        schema = .arrow_infer_schema(file),
        skip = 1L,
        delim = sep,
        ...
    )

    # check for presence of column to match values against
    cols <- names(a)
    if (!col %in% cols) {
        sprintf(
            "Column %s not found in file. Available columns: '%s'",
            col, paste(cols, collapse = "', '")
        ) %>%
            stop(call. = FALSE)
    }

    # perform filter
    dt <- dplyr::filter(a, !!dplyr::ensym(col) %in% !!values_to_match) %>%
        dplyr::collect() %>%
        data.table::setDT()
    
    if (is.character(drop)) {
        drop <- which(colnames(dt) %in% drop)
    }
    if (is.numeric(drop)) {
        all_idx <- seq_len(ncol(dt))
        dt <- dt[, all_idx[!all_idx %in% drop], with = FALSE]
    }
    return(dt)
}

#' @describeIn read_colmatch deprecated.
fread_colmatch <- function(...) {
    deprecate_soft("0.2.4", "fread_colmatch()", "read_colmatch()")
    read_colmatch(...)
}




# internals

# codecs that can be discerned from filepaths:
## gz, bz2
# codecs to directly check for:
## zstd, lz4, snappy
.arrow_codec_check <- function(file = NULL, codec = NULL) {
    # check codec(s) needed
    if (!is.null(file)) {
        extension <- file_extension(file)
        if (any(c("gz", "gzip", "tgz") %in% extension)) {
            codec <- c(codec, "gzip")
        }
        if (any(c("bz2", "bzip2", "tbz2") %in% extension)) {
            codec <- c(codec, "bz2")
        }
        # If checked path and no codecs noted, pass.
        if (length(codec) == 0L) {
            return(invisible(TRUE))
        }
    }

    if (length(codec) == 0L) {
        ".arrow_codec_check: either `file` or `codec` must be provided" %>%
            stop(call. = FALSE)
    }

    # check install and arrow capabilities
    has_arrow <- requireNamespace("arrow", quietly = TRUE)
    if (has_arrow) {
        caps <- arrow::arrow_info()$capabilities
        zstd <- caps[c("zstd")]
        gzip <- caps[c("gzip")]
        bz2 <- caps[c("bz2")]
        lz4 <- caps[c("lz4")]
        snappy <- caps[c("snappy")]
    }

    # determine if (re)install needed
    codecs_okay <- TRUE
    if ("zstd" %in% codec && !zstd) codecs_okay <- FALSE
    if ("gzip" %in% codec && !gzip) codecs_okay <- FALSE
    if ("bz2" %in% codec && !bz2) codecs_okay <- FALSE
    if ("lz4" %in% codec && !lz4) codecs_okay <- FALSE
    if ("snappy" %in% codec && !snappy) codecs_okay <- FALSE

    # general message for commonly seen codecs
    if (!has_arrow || !codecs_okay) {
        'Needed arrow compression codec(s) not installed. Please run:

        Sys.setenv(ARROW_WITH_ZSTD = "ON")
        Sys.setenv(ARROW_WITH_GZ2 = "ON")
        Sys.setenv(ARROW_WITH_BZ2 = "ON")
        Sys.setenv(ARROW_WITH_LZ4 = "ON")
        Sys.setenv(ARROW_WITH_SNAPPY = "ON")
        install.packages("arrow",
            repos = c("https://apache.r-universe.dev"),
            type = "source"
        )' %>%
            wrap_txt() %>%
            stop(call. = FALSE)
    }
    invisible(TRUE)
}

# Use data.table to get a sample and infer schema
.arrow_infer_schema <- function(file, n_rows = 10) {
    lines <- readLines(file, n = n_rows)
    # Parse with fread as string input
    sample_dt <- data.table::fread(paste(lines, collapse = "\n"))

    # Map data.table/R types to Arrow types
    type_mapping <- list(
        "integer" = arrow::int32(),
        "double" = arrow::float64(),
        "raw" = arrow::binary(),
        "character" = arrow::string(),
        "logical" = arrow::boolean(),
        "Date" = arrow::date32(),
        "POSIXct" = arrow::timestamp("us")
    )

    # Create schema
    fields <- lapply(sample_dt, function(col) {
        col_type <- type_mapping[[typeof(col)]] %null% arrow::string()
        col_type
    })

    # If there were no column names, generate them
    if (all(grepl("^V[0-9]+$", names(sample_dt)))) {
        names(fields) <- paste0("V", seq_along(fields))
    } else {
        names(fields) <- names(sample_dt)
    }

    arrow::schema(!!!fields)
}
