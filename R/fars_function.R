#' This is a assignment to Coursera Building R Packages week2.

#' fars_function
#'
#' The functions will be using data from the US National Highway Traffic Safety
#' Administration's Fatality Analysis Reporting System, which is a
#' nationwide census providing the American public yearly data regarding fatal
#' injuries suffered in motor vehicle traffic crashes.
#'
#' @param filename A character string with the filename to read.
#'
#' @return This function returns a data frame from csv file, or an error with
#'         wrong filename.
#'
#' @import readr
#' @import dplyr
#'
#' @examples
#' fars_read(filename = 'accident_2013.csv.bz2')
#'
#' @export

fars_read <- function(filename) {
    if(!file.exists(filename))
        stop("file '", filename, "' does not exist")
    data <- suppressMessages({
        readr::read_csv(filename, progress = FALSE)
    })
    dplyr::tbl_df(data)
}

#' make_filename
#'
#' This function make csv filenames related to the argument \code{year}.
#' This function doesn't check the file available or not.
#'
#' @param year An integer, a numeric or string of the year.
#'
#' @return this function returns a string that is the proper FARS data
#'    filename for the given year.
#'
#' @examples
#' make_filename(year = '2013')
#'
#' @export
make_filename <- function(year) {
    year <- as.integer(year)
    sprintf("accident_%d.csv.bz2", year)
}

#' fars_read_years
#'
#' This function takes a vector of years and produces a list of tibbles,
#' where each tibble is that year's FARS file year and MONTH observations.
#'
#' @param years Vector of years' FARS files to open.
#'
#' @return A data.frame including entries in data by month, or NULL if the
#'  \code{year} is not valid.
#'
#' @import dplyr
#' @import magrittr
#'
#' @examples
#' fars_read_years(years = c(2014, 2015))
#'
#' @export
fars_read_years <- function(years) {
    lapply(years, function(year) {
        file <- make_filename(year)
        tryCatch({
            dat <- fars_read(file)
            dplyr::mutate(dat, year = year) %>%
                dplyr::select(MONTH, year)
        }, error = function(e) {
            warning("invalid year: ", year)
            return(NULL)
        })
    })
}

#' fars_summarize_years
#'
#' This function summarizes yearly accidents data, by month.
#'
#' @param years Vector of years' FARS files to open.
#'
#' @return A data.frame with number of accidents by years summarized by month.
#'
#' @import magrittr
#' @import tidyr
#' @import dplyr
#'
#' @examples
#' fars_summarize_years(years = c(2014, 2015))
#'
#' @export
fars_summarize_years <- function(years) {
    dat_list <- fars_read_years(years)
    dplyr::bind_rows(dat_list) %>%
        dplyr::group_by(year, MONTH) %>%
        dplyr::summarize(n = n()) %>%
        tidyr::spread(year, n)
}

#' fars_map_state
#'
#' This function provide a plot with a state map including the accidents
#' location by year.
#'
#' @param year An integer, a numeric or string of the year.
#'
#' @param state.num An Integer with the State Code.
#'
#' @return None.
#'
#' @import dplyr
#' @import maps
#' @import graphics
#'
#'
#' @examples
#' \dontrun{
#' fars_map_state(35, 2013)
#' }
#'
#' @export
fars_map_state <- function(state.num, year) {
    filename <- make_filename(year)
    data <- fars_read(filename)
    state.num <- as.integer(state.num)

    if(!(state.num %in% unique(data$STATE)))
        stop("invalid STATE number: ", state.num)
    data.sub <- dplyr::filter(data, STATE == state.num)
    if(nrow(data.sub) == 0L) {
        message("no accidents to plot")
        return(invisible(NULL))
    }
    is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
    is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
    with(data.sub, {
        maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                  xlim = range(LONGITUD, na.rm = TRUE))
        graphics::points(LONGITUD, LATITUDE, pch = 46)
    })
}

