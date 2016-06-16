download_index <- function(year, quarter) {
    idx_url <- paste0("ftp://ftp.sec.gov/edgar/full-index/",
                      year, "/QTR", quarter, "/master.idx")
    destdir <- paste0("data/", year)
    destfile <- paste0(destdir, "/q", quarter, "_master.idx")
    if (!dir.exists(destdir))
        dir.create(destdir, recursive = TRUE)
    if (download.file(idx_url, destfile) != 0)
        stop("There was a problem while downloading")
    destfile
}
