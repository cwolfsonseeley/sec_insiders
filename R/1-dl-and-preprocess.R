library(dplyr)
library(magrittr)
library(stringr)
library(xml2)
library(tidyr)
source("R/is_corp.R")
source("R/functions-download-index.R")

# scripts will download all (potentially matching) forms for a single quarter, 
# specify which year/quarter:
sec_year <- 2016
sec_qtr <- 4

# download a master index file
# idx_filename will store the name of the file that's been downloaded
idx_filename <- download_index(sec_year, sec_qtr)

destination <- paste0(dirname(idx_filename), "/q", sec_qtr, "/")
if (!dir.exists(destination)) dir.create(destination, recursive = TRUE)

# read in the index file
# should receive a single warning, relating to the fact that the file has,
# inexplicably, a row of dashes after the header but before hte data
master <- readr::read_delim(idx_filename, 
                            skip = 9, 
                            delim = "|",
                            col_names = TRUE,
                            escape_backslash = FALSE)

# so we remove the line of dashes
master <- master[-1,]

# going to run the list of filer names from the master index against
# the known names of people in CADS to look for potential matches,
# so only download filings that may end up being matches
cads_names <- getcdw::get_cdw("select distinct entity_id, first_name, last_name, middle_name from cdw.d_bio_name_mv
where entity_id in (select entity_id from cdw.d_entity_mv where person_or_org = 'P' and record_status_code = 'A')")

# prep for matching by converting to lowercase
cads_names %<>%
    mutate_each(funs = funs(tolower), first_name:middle_name)


# make a list of all files to download, by
# looking for filings by people whose names match cads names
# looking only for forms 3, 4, and 5.
master %>%
    filter(`Form Type` %in% c('3', '4', '5')) %>% 
    filter(!is_corp(`Company Name`)) %>%
    mutate(name = `Company Name`, name2 = tolower(name)) %>%
    mutate(last1 = str_extract(name2, "^[^\\s]+\\s+") %>% str_trim,
           special = last1 %in% c("van", "de"),
           last_name = ifelse(special,
                              str_extract(name2, "^[^\\s]+\\s+[^\\s]+") %>% str_trim,
                              last1),
           first_name = ifelse(special, 
                               str_match(name2, "^[^\\s]+\\s+[^\\s]+\\s+([^\\s]+)")[,2] %>% str_trim,
                               str_extract(name2, "\\s+[^\\s]+") %>% str_trim),
           middle_name = ifelse(special,
                                str_extract(name2, "^[^\\s]+\\s+[^\\s]+\\s+[^\\s]+\\s+[^\\s]+") %>% 
                                    str_extract("[^\\s]+$") %>% str_trim,
                                str_extract(name2, "^[^\\s]+\\s+[^\\s]+\\s+([^\\s]+)") %>% 
                                    str_extract("[^\\s]+$") %>% str_trim)) %>%
    mutate(middle_initial = str_sub(middle_name, 1, 1)) %>%
    inner_join(cads_names, by = c("first_name" = "first_name",
                                  "last_name"  = "last_name")) %>%
    mutate(sec_mi = str_trim(middle_initial), cads_mi = str_trim(str_sub(middle_name.y, 1, 1))) %>%
    filter(nchar(sec_mi) == 0 | nchar(cads_mi) == 0 | is.na(sec_mi) | is.na(cads_mi) |
               sec_mi == cads_mi) %>% 
    mutate(file_location = paste("https://www.sec.gov/Archives/", Filename, sep = "")) %>%
    # group_by(CIK) %>% 
    # mutate(recent = max(`Date Filed`)) %>% 
    # ungroup %>% 
    # filter(`Date Filed` == recent) %>%
    select(file_location) %>%
    distinct -> dl_list

# this sets up a data frame with the necessary info for downloading all 
# files (ie, download file url, and destination path)
dl_list %<>%
    mutate(dir = dirname(file_location),
           dir = str_extract(dir, "[^/]+$"),
           filename = basename(file_location)) %>%
    transmute(file_location = file_location,
              filename = paste(dir, filename, sep = "-"),
              filename = paste(destination, filename, sep = ""))

# include small delays between file downloads to avoid bombarding the server
# with all of these (thousands of) requests
# note that this part of the script is best run overnight
downloader <- function(file, destfile) {
    if (file.exists(destfile)) return()
    download.file(file, destfile, quiet = TRUE)
    Sys.sleep(.2)
}

# download all files to the proper directory
# note definition of downloader function above -- if interrupted, 
# you can just re-start without worrying about duplicating effort
Map(downloader, dl_list$file_location, destfile = dl_list$filename)

# these files define functions that parse the sec filings
# the main one is process_filing, and then others are helpers
source("r/parsers_reporting_owner.R")
source("r/parsers_mail_address.R")
source("r/parsers_main.R")

list.files(destination, full.names = TRUE) %>% 
    lapply(process_filing) -> processed_filings

processed_filings <- bind_rows(processed_filings)
processed_filings %<>% tbl_df
processed_filings %>% select(cik, name) %>% distinct

# add most recent close prices by stock ticker,
# will be used to estimate total $$ value of holdings from filings
library(quantmod)
prices <- processed_filings %>% 
    select(ticker) %>% 
    filter(!is.na(ticker)) %>% 
    mutate(ticker = str_match(ticker, "^(.+:\\s*)?([A-Za-z]+)")[,3]) %>%
    distinct %>%
    "$"(ticker) %>%
    paste(collapse = ";") %>%
    getQuote

prices$symbol <- rownames(prices)
