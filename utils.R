library(rlog)

#' Run a function caching the result on the file system.
#'
#' The function will be called with the appended arguments. The [`id`] argument
#' will be used to identify the cache file on the file system and in log
#' messages.
run_cached <- function(id, func, ...) {
    if (!dir.exists("cache")) {
        dir.create("cache")
    }

    cache_file <- paste("cache/", id, ".rds", sep = "")

    if (file.exists(cache_file)) {
        log_info(sprintf("Loading %s from cache", id))

        # If the cache file exists, we restore the data from it.
        readRDS(cache_file)
    } else {
        # If the cache file doesn't exist, we have to do the computation.
        data <- func(...)

        # The results are cached for the next run.
        saveRDS(data, cache_file)

        data
    }
}
