parse_reporting_owner <- function(filing) {
    reporters <- xml_find_all(filing, ".//reportingOwner")
    
    parse_each <- function(filing) {
        cik <- parse_feature(filing, ".//rptOwnerCik")
        name <- parse_feature(filing, ".//rptOwnerName")
        title <- parse_feature(filing, ".//officerTitle")
        street1 <- parse_feature(filing, ".//rptOwnerStreet1")
        street2 <- parse_feature(filing, ".//rptOwnerStreet2")
        city <- parse_feature(filing, ".//rptOwnerCity")
        state <- parse_feature(filing, ".//rptOwnerState")
        zip <- parse_feature(filing, ".//rptOwnerZipCode")
        director <- parse_feature(filing, ".//isDirector")
        officer <- parse_feature(filing, ".//isOfficer")
        ten_percenter <- parse_feature(filing, ".//isTenPercentOwner")
        dplyr::data_frame(cik, name, title, street1, street2,
                          city, state, zip, director, officer, ten_percenter)
    }
    
    res <- lapply(reporters, parse_each)
    dplyr::bind_rows(res)
}
