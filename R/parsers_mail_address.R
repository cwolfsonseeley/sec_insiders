get_mail_address <- function(el) {
    
    from <- str_detect(el, "<SEC-HEADER>") %>% which
    to <- str_detect(el, "</SEC-HEADER>") %>% which
    header <- el[from:to]
    
    header <- header[str_length(str_trim(header)) > 0]
    records <- sum(str_detect(header, "REPORTING-OWNER:"))
    
    cik <- character(records)
    name <- character(records)
    street1 <- character(records)
    street2 <- character(records)
    city <- character(records)
    state <- character(records)
    zip <- character(records)
    
    counter <- 0L
    for (line in header) {
        if (str_trim(line) == "REPORTING-OWNER:") counter <- counter + 1L
        
        if (str_detect(line, "CENTRAL INDEX KEY:")) cik[counter] <- str_extract(line, "[0-9]+$")
        if (str_detect(line, "COMPANY CONFORMED NAME:")) name[counter] <- str_extract(line, "[^\\t]+$")
        if (str_detect(line, "STREET 1:")) street1[counter] <- str_extract(line, "[^\\t]+$")
        if (str_detect(line, "STREET 2:")) street2[counter] <- str_extract(line, "[^\\t]+$")
        if (str_detect(line, "CITY:")) city[counter] <- str_extract(line, "[^\\t]+$")
        if (str_detect(line, "STATE:")) state[counter] <- str_extract(line, "[^\\t]+$")
        if (str_detect(line, "ZIP:")) zip[counter] <- str_extract(line, "[^\\t]+$")
    }
    list(cik = cik, name = name, street1 = street1, street2 = street2, city = city,
         state = state, zip = zip)
}