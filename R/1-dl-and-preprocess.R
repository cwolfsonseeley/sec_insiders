## prelim: check any research-verified ciks that are not in the rdata database,
## update sec_cik_dict and/or sec_blacklist as necessary
getcdw::get_cdw("select 
entity_id, 
to_number(other_id) as cik 
from cdw.d_ids_base 
where ids_type_code = 'SEC'
and not regexp_like(other_id, '[^0-9]')
minus
(select
    entity_id,
    cik
    from rdata.sec_cik_dict
    minus
    select
    entity_id,
    cik
    from rdata.sec_blacklist)")

# update sec_cik_dict:
data_frame(entity_id = 261021, cik = 1184736) %>% 
    ROracle::dbWriteTable(
        conn = getcdw::connect("URELUAT_DEVEL"), name = "SEC_CIK_DICT", 
        value = ., 
        schema = 'RDATA',
        overwrite = FALSE, append = TRUE)
ROracle::dbCommit(getcdw::connect("URELUAT_DEVEL"))

# update sec_blacklist
data_frame(entity_id = 452155, cik = 1580690) %>% 
    ROracle::dbWriteTable(
        conn = getcdw::connect("URELUAT_DEVEL"), name = "SEC_BLACKLIST", 
        value = ., 
        schema = 'RDATA',
        overwrite = FALSE, append = TRUE)
ROracle::dbCommit(getcdw::connect("URELUAT_DEVEL"))

# just in case
getcdw::reset_cdw()

library(dplyr)
library(magrittr)
library(stringr)
library(xml2)
library(tidyr)
source("R/is_corp.R")
source("R/functions-download-index.R")

# scripts will download all (potentially matching) forms for a single quarter, 
# specify which year/quarter:
sec_year <- 2018
sec_qtr <- 2

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
cads_names <- getcdw::get_cdw("select distinct entity_id, 
lower(first_name) as first_name, 
lower(last_name) as last_name, 
lower(middle_name) as middle_name 
from cdw.d_bio_name_mv
where entity_id in (select entity_id from cdw.d_entity_mv where person_or_org = 'P' and record_status_code = 'A')")

# prep for matching by converting to lowercase
# cads_names %<>%
#     mutate_each(funs = funs(tolower), first_name:middle_name)

# research has begun storing CIK when it is verified it's the right person,
# should use this data when attempting to match (since they are known to be correct)
verified_cik <- getcdw::get_cdw("
select
  entity_id as entity_id_v,
  cik as cads_cik
from rdata.sec_cik_dict
minus
select
  entity_id as entity_id_v,
  cik as cads_cik
from rdata.sec_blacklist")
    
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

## add on the known cik's in case any were missed:
dl_list_extra <- master %>% 
    filter(`Form Type` %in% c('3', '4', '5')) %>% 
    filter(CIK %in% verified_cik$cads_cik) %>%
    mutate(file_location = paste("https://www.sec.gov/Archives/", Filename, sep = "")) %>%
    select(file_location) %>% distinct

dl_list <- dplyr::union(dl_list, dl_list_extra)

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
# also: using .5 second delay b/c sec.gov does not specify what the limit
# should be! https://www.sec.gov/privacy.htm#security says:
## QUOTE ##
# If a single IP Address exceeds a threshold request per second rate, further 
# requests from that IP Address will be limited for a brief period.)
## ENDQUOTE ##
# but what is the threshold? it doesn't say. their API limit is 2/second, though.
# so here i'll use the same limit and cross my fingers
downloader <- function(file, destfile) {
    if (file.exists(destfile)) return()
    download.file(file, destfile, quiet = TRUE)
    Sys.sleep(1)
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
