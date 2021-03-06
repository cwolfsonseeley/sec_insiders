parse_filing <- function(filing) {
    range <- which(stringr::str_detect(filing, "</?XML>"))
    from <- min(range)
    to <- max(range)
    fil <- paste(filing[(from+1):(to-1)], collapse = " ")
    xml2::read_xml(fil)
}

parse_feature <- function(doc, feature) {
    x <- xml_find_all(doc, feature)
    res <- xml_text(x)
    if (length(res) == 0) return("")
    res
}

summarize_transactions <- function(filing) {
    di <- xml_find_all(filing, "//nonDerivativeTable//directOrIndirectOwnership/value") %>%
        xml_text
    shares <- xml_find_all(filing, "//nonDerivativeTable//sharesOwnedFollowingTransaction/value") %>%
        xml_text %>% as.numeric
    xml_find_all(filing, "//nonDerivativeTable//sharesOwnedFollowingTransaction/value") %>%
        xml_text %>% as.numeric
    
    direct <- max(shares[di == "D"], na.rm = TRUE)
    indirect <- sum(shares[di == "I"], na.rm = TRUE)
    list(direct = direct, indirect = indirect)
    
}

read_filing <- function(platext) {
    filing <- parse_filing(platext)
    
    report_date <- parse_feature(filing, ".//periodOfReport")
    company_name <- parse_feature(filing, ".//issuerName")
    ticker <- parse_feature(filing, ".//issuerTradingSymbol")
    reporting_owner <- parse_reporting_owner(filing)
    
    mailing_address <- get_mail_address(platext)
    
    mcik <- mailing_address$cik
    mname <- mailing_address$name
    mstreet1 <- mailing_address$street1
    mstreet2 <- mailing_address$street2
    mcity <- mailing_address$city
    mstate <- mailing_address$state
    mzip <- mailing_address$zip
    
    share_data <- summarize_transactions(filing)
    direct_shares <- share_data$direct
    indirect_shares <- share_data$indirect
    
    
    res <- data.frame(report_date, reporting_owner,
                      company_name, ticker, direct_shares, indirect_shares,
                      stringsAsFactors = FALSE)
    
    bind_rows(res, data.frame(report_date, cik = mcik, name = mname,
                              title = "", 
                              street1 = mstreet1, street2 = mstreet2,
                              city = mcity, state = mstate, zip = mzip, 
                              director = "", officer = "", ten_percenter = "",
                              company_name = company_name, ticker = ticker, 
                              direct_shares = direct_shares, indirect_shares = indirect_shares,
                              stringsAsFactors = FALSE))
}

process_filing <- function(filename) {
    x <- readLines(filename)
    read_filing(x)
}
