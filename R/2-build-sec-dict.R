# create a reference directory if there isn't already one
if (!dir.exists("reference")) dir.create("reference")

# this isn't used in the script, but is useful to have 
# (and may help to build smarter parsers at some point)
# technical specs for the edgar xml format
techspec_url <- "https://www.sec.gov/info/edgar/ownershipxmltechspec-v5-1.zip"
techspec_dest <- paste("reference", basename(techspec_url), sep = "/")
if (!file.exists(techspec_dest)) 
    download.file(techspec_url, destfile = techspec_dest)

######
# now build name frequency table for sec filing data
# using a cik dictinary for full set of filer names

# download a dictionary of cik - company name. 
#cik_url <- "https://www.sec.gov/edgar/NYU/cik.coleft.c"
cik_url <- "https://www.sec.gov/Archives/edgar/cik-lookup-data.txt"

# will overwrite the dictionary if it already exists
download.file(cik_url, "reference/cik_dictionary.c")

# just want to make a one-column tbl_df and being lazy,
# checked with "grep ~ cik_dictionary.c" to make sure ~
# never appears, so can use ~ as a delimiter --> one-column.
cik_dict <- readr::read_delim("reference/cik_dictionary.c",
                              delim = "~",
                              col_names = FALSE,
                              col_types = 'c',
                              escape_backslash = FALSE)

# this actually splits out the CIK from the name
cik_dict %<>%
    transmute(cik = str_match(X1, "(.+?):([0-9]+):")[,3],
              name = str_match(X1, "(.+?):([0-9]+):")[,2])

# now try to remove corporate/company names and leave just human 
# names (as much as possible), and then split out first/middle/last,
# and then leave a data frame that maps cik to first, middle, last names
# this is where the script could really be improved -- 
#    - better logic to remove corp names
#    - better logic to split out components of person's name
cik_dict %<>%
    filter(!is_corp(name)) %>%
    mutate(name2 = tolower(name)) %>%
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
    select(cik, name, first_name, middle_name, middle_initial, last_name) %>% 
    distinct

# now build frequency tables for first/last name
cik_dict %>%
    select(cik, first_name) %>%
    distinct %>%
    group_by(first_name) %>% summarise(sec_first_count = n_distinct(cik)) %>%
    mutate(n_sec_first = sum(sec_first_count)) -> sec_first

cik_dict %>%
    select(cik, last_name) %>%
    distinct %>%
    group_by(last_name) %>% summarise(sec_last_count = n_distinct(cik)) %>%
    mutate(n_sec_last = sum(sec_last_count)) -> sec_last