#use esquisse's function
get_data <- function(data = NULL, name = NULL) {

  if (!is.null(data)) {
    if (is.character(data)) {
      guitable_data <- try({
        dat <- get(x = data, envir = globalenv())
        dat
        # if (inherits(dat, what = "sf")) {
        #   dat
        # } else {
        #   as.data.frame(dat)
        # }
      }, silent = TRUE)
      guitable_data_name <- data
      if ("try-error" %in% class(guitable_data)) {
        warning(paste0("'", data, "' not found"), call. = FALSE)
        guitable_data <- NULL
        guitable_data_name <- ""
      }
    } else if (inherits(x = data, what = "data.frame")) {
      guitable_data <- try({
        data
        # if (inherits(data, what = "sf")) {
        #   data
        # } else {
        #   as.data.frame(data)
        # }
      }, silent = TRUE)
      if ("try-error" %in% class(guitable_data)) {
        warning(paste0("'", data, "' not found"), call. = FALSE)
        guitable_data <- NULL
        guitable_data_name <- ""
      } else {
        if (!is.null(name)) {
          guitable_data_name <- as.character(name)
        } else {
          guitable_data_name <- deparse(substitute(data))
        }
      }

    } else {
      guitable_data <- NULL
      guitable_data_name <- ""
    }
  }

  list(guitable_data = guitable_data, guitable_data_name = guitable_data_name)
}
