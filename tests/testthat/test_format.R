test_that("Angle conversions", {
    rads <- pi
    degs <- 180L

    expect_equal(degrees(rads), degs)
    expect_equal(radians(degs), rads)
})

test_that("Time conversions", {
    test_time1 <- 10
    test_time2 <- 1e3
    test_time3 <- 1e4
    test_time4 <- 5

    out1 <- time_format(test_time1)
    out2 <- time_format(test_time2)
    out3 <- time_format(test_time3)
    out4 <- time_format(test_time4)

    checkmate::expect_character(out1)
    checkmate::expect_character(out2)
    checkmate::expect_character(out3)

    expect_identical(out1, "10.0s")
    expect_identical(out2, "00:16:40")
    expect_identical(out3, "02:46:40")
    expect_identical(out4, "5.000s")
})

test_that("wrap_txt", {
    ipsum <- "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
    out_30 <- "Lorem ipsum dolor sit amet,\n consectetur adipiscing elit,\n sed do eiusmod tempor\n incididunt ut labore et\n dolore magna aliqua. Ut enim\n ad minim veniam, quis\n nostrud exercitation ullamco\n laboris nisi ut aliquip ex\n ea commodo consequat. Duis\n aute irure dolor in\n reprehenderit in voluptate\n velit esse cillum dolore eu\n fugiat nulla pariatur.\n Excepteur sint occaecat\n cupidatat non proident, sunt\n in culpa qui officia\n deserunt mollit anim id est\n laborum."
    expect_identical(
        wrap_txt(ipsum,
            strWidth = 30L
        ),
        out_30
    )
    expect_no_condition(wrap_txt(ipsum, errWidth = TRUE))
    expect_identical(wrap_txt("a", "b", sep = "Y", errWidth = TRUE), "aYb")
})




test_that("str_vector", {
    x <- letters[seq(5)]
    out1 <- str_vector(x)
    expect_identical(out1, "'a', 'b', 'c', 'd', 'e'")

    y <- seq(6)
    out2 <- str_vector(y)
    expect_identical(out2, "'1', '2', '3', '4', '5', '6'")
})





# color print ####

test_that("color_tag is the expected list", {
    out <- color_tag()
    checkmate::expect_list(out, types = "character", names = "named", len = 7L)
})

test_that("use_color_text settings", {
    opt1 <- getOption("giotto.color_show", default = NULL)
    options("giotto.color_show" = FALSE)
    expect_false(use_color_text())

    opt2 <- getOption("cli.num_colors", default = NULL)
    options("giotto.color_show" = TRUE)
    options("cli.num_colors" = 2L)
    expect_message(use_color_text(), "Color text not supported on this system.")

    options("cli.num_colors" = 10L)
    expect_true(use_color_text())
    options("giotto.color_show" = NULL)
    expect_true(use_color_text())

    on.exit({
        options("giotto.color_show" = opt1)
        options("cli.num_colors" = opt2)
    })
})

# Test for ansi_colors function
test_that("ansi_color settings", {
    # get defaults
    opt1 <- getOption("giotto.num_colors", default = NULL)
    opt2 <- getOption("cli.num_colors", default = NULL)

    options("cli.num_colors" = NULL) # needed to bypass otherwise value is 1 when
    # running from testthat

    options("giotto.num_colors" = 256)
    expect_equal(ansi_colors(), 256)
    options("giotto.num_colors" = NULL)

    options("cli.num_colors" = 128)
    expect_equal(ansi_colors(), 128)

    on.exit({
        options("giotto.num_colors" = opt1)
        options("cli.num_colors" = opt2)
    })
})

# Test for ansi_colors function
test_that("ansi_colors returns an integer value", {
    num_colors <- ansi_colors()
    checkmate::expect_integer(num_colors)
    expect_true(num_colors >= 1)
})

# Test for is_emacs_with_color function
test_that("is_emacs_with_color returns a boolean value", {
    result <- is_emacs_with_color()
    checkmate::expect_logical(result)
})

# Test for emacs_version function
test_that("emacs_version returns a numeric version or NA", {
    version <- emacs_version()
    checkmate::expect_numeric(version)
    expect_true(is.numeric(version) || is.na(version))
})

# Test for a valid version string
test_that("emacs_version returns a numeric version for a valid version string", {
    old_ENV <- Sys.getenv("INSIDE_EMACS")
    on.exit(Sys.setenv(INSIDE_EMACS = old_ENV))
    Sys.setenv("INSIDE_EMACS" = 1)
    version <- emacs_version()
    checkmate::expect_numeric(version)
    expect_true(!is.na(version))
})

# Test for an invalid version string
test_that("emacs_version returns NA for an invalid version string", {
    old_ENV <- Sys.getenv("INSIDE_EMACS")
    on.exit(Sys.setenv(INSIDE_EMACS = old_ENV))

    Sys.setenv(INSIDE_EMACS = "not_a_version_string")
    version <- emacs_version()

    checkmate::expect_integer(version)
    expect_true(is.na(version))
})

# Test for an empty version string
test_that("emacs_version returns NA for an empty version string", {
    old_ENV <- Sys.getenv("INSIDE_EMACS")
    on.exit(Sys.setenv(INSIDE_EMACS = old_ENV))

    Sys.setenv(INSIDE_EMACS = "")
    version <- emacs_version()

    checkmate::expect_integer(version)
    expect_true(is.na(version))
})


test_that("further color decision making works", {
    g_opt <- getOption("giotto.num_colors", default = NULL)
    cli_opt <- getOption("cli.num_colors", default = NULL)
    cray_on <- getOption("crayon.enabled", NULL)
    cray_num <- getOption("crayon.colors", NULL)
    sys_color <- Sys.getenv("NO_COLOR", NA_character_)
    r_color <- Sys.getenv("R_CLI_NUM_COLORS", "")
    plat <- .Platform$GUI
    env <- Sys.getenv("RSTUDIO")
    knit <- getOption("knitr.in.progress")

    options("crayon.enabled" = FALSE)
    options("crayon.colors" = 10)

    # cli and giotto settings
    options("cli.num_colors" = NULL) # needed to bypass otherwise value is 1 when
    # running from testthat
    options("giotto.num_colors" = NULL)

    Sys.setenv("R_CLI_NUM_COLORS" = 2)
    expect_equal(ansi_colors(), 2L)
    Sys.setenv("R_CLI_NUM_COLORS" = "")

    # crayon compatibility
    expect_equal(ansi_colors(), 1L)
    options("crayon.enabled" = TRUE)
    expect_equal(ansi_colors(), 10L)

    options("crayon.colors" = NULL)
    expect_equal(ansi_colors(), 8L)

    options("crayon.enabled" = NULL)
    options("crayon.colors" = NULL)

    Sys.setenv("NO_COLOR" = TRUE)
    expect_equal(ansi_colors(), 1L)
    Sys.setenv("NO_COLOR" = NA_character_)

    # options("knitr.in.progress" = TRUE)
    # expect_equal(ansi_colors(), 1L)
    # options("knitr.in.progress" = FALSE)

    # .Platform$GUI = "AQUA"
    # expect_equal(ansi_colors(), 1L)
    # .Platform$GUI = "RStudio"

    # Sys.setenv("RSTUDIO" = "1")
    # expect_equal(ansi_colors(), 8L)
    # Sys.setenv("RSTUDIO" = env)

    on.exit({
        options("giotto.num_colors" = g_opt)
        options("cli.num_colors" = cli_opt)
        options("crayon.enabled" = cray_on)
        options("crayon.colors" = cray_num)
        Sys.setenv("NO_COLOR" = sys_color)
        .Platform$GUI <- plat
        Sys.setenv("RSTUDIO" = env)
        Sys.setenv("R_CLI_NUM_COLORS" = r_color)
        options("knitr.in.progress" = knit)
    })
})
