rm(list = ls()[ls() != "matches"])

library(xml2)
library(tidyverse)

parse_filing <- function(filing) {
    range <- which(stringr::str_detect(filing, "</?XML>"))
    from <- min(range)
    to <- max(range)
    fil <- paste(filing[(from+1):(to-1)], collapse = " ")
    xml2::read_xml(fil)
}

form4_issuer <- function(xml) {
    is <- xml_find_all(xml, "/ownershipDocument/issuer")
    
    sections <- list(
        issuer_name = ".//issuerName",
        issuer_cik = ".//issuerCik",
        ticker = ".//issuerTradingSymbol"
    )
    
    getsec <- function(section) {
        gs <- function(x) {
            val <- xml_text(xml_find_all(x, section))
            if (length(val) == 0) return("")
            val
        }
        purrr::map_chr(is, gs)
    }
    
    dplyr::as_data_frame(purrr::map(sections, getsec))
}


form4_header <- function(xml) {
    ro <- xml_find_all(xml, "/ownershipDocument/reportingOwner")
    sections <- 
        list(
            reporting_owner_cik = ".//reportingOwnerId/rptOwnerCik",
            reporting_owner = ".//reportingOwnerId/rptOwnerName",
            is_director = ".//reportingOwnerRelationship/isDirector",
            is_officer = ".//reportingOwnerRelationship/isOfficer",
            is_ten_percenter = ".//reportingOwnerRelationship/isTenPercentOwner",
            is_other=  ".//reportingOwnerRelationship/isOther",
            officer_title = ".//reportingOwnerRelationship/officerTitle",
            other_text = ".//reportingOwnerRelationship/otherText"
        )
    
    getsec <- function(section) {
        gs <- function(x) {
            val <- xml_text(xml_find_all(x, section))
            if (length(val) == 0) return("")
            val
        }
        purrr::map_chr(ro, gs)
    }
    
    dplyr::as_data_frame(purrr::map(sections, getsec))
}

form4_non_deriv <- function(xml) {
    nd <- xml_find_all(xml, "/ownershipDocument/nonDerivativeTable")
    nd <- xml_find_all(nd, "./nonDerivativeTransaction|./nonDerivativeHolding")
    
    sections <- 
        list(
            security_title = ".//securityTitle/value",
            transaction_date = ".//transactionDate/value",
            deemed_execution_date = ".//deemedExecutionDate/value",
            transaction_code = ".//transactionCoding/transactionCode",
            equity_swap = ".//transactionCoding/equitySwapInvolved",
            acquired_disposed = "./transactionAmounts/transactionAcquiredDisposedCode/value",
            shares = "./transactionAmounts/transactionShares/value",
            price_share = "./transactionAmounts/transactionPricePerShare/value",
            post_transaction_shares = "./postTransactionAmounts/sharesOwnedFollowingTransaction/value",
            post_transaction_value = "./postTransactionAmounts/valueOwnedFollowingTransaction",
            direct_indirect = "./ownershipNature/directOrIndirectOwnership/value",
            ownership_nature = "./ownershipNature/natureOfOwnership/value"
        )
    
    getsec <- function(section) {
        gs <- function(x) {
            val <- xml_text(xml_find_all(x, section))
            if (length(val) == 0) return("")
            val
        }
        purrr::map_chr(nd, gs)
    }
    
    dplyr::as_data_frame(purrr::map(sections, getsec))
}

form4_deriv <- function(xml) {
    deriv <- xml_find_all(xml, "/ownershipDocument/derivativeTable")
    deriv <- xml_find_all(deriv, "./derivativeTransaction|./derivativeHolding")
    
    sections <- 
        list(
            security_title = ".//securityTitle/value",
            conv_ex_price = "./conversionOrExercisePrice/value",
            transaction_date = ".//transactionDate/value",
            deemed_execution_date = ".//deemedExecutionDate/value",
            transaction_code = ".//transactionCoding/transactionCode",
            equity_swap = ".//transactionCoding/equitySwapInvolved",
            acquired_disposed = "./transactionAmounts/transactionAcquiredDisposedCode/value",
            shares = "./transactionAmounts/transactionShares/value",
            transaction_total_value = "./transactionAmounts/transactionTotalValue/value",
            exercise_date = "./exerciseDate/value",
            expiration_date = "./expirationDate",
            underlying_security = "./underlyingSecurity/underlyingSecurityTitle/value",
            underlying_security_shares = "./underlyingSecurity/underlyingSecurityShares",
            underlying_security_value = "./underlyingSecurity/underlyingSecurityValue/value",
            price_share = "./transactionAmounts/transactionPricePerShare/value",
            post_transaction_shares = "./postTransactionAmounts/sharesOwnedFollowingTransaction/value",
            post_transaction_value = "./postTransactionAmounts/valueOwnedFollowingTransaction",
            direct_indirect = "./ownershipNature/directOrIndirectOwnership/value",
            ownership_nature = "./ownershipNature/natureOfOwnership/value"
        )
    
    getsec <- function(section) {
        gs <- function(x) {
            val <- xml_text(xml_find_all(x, section))
            if (length(val) == 0) return("")
            val
        }
        purrr::map_chr(deriv, gs)
    }
    
    dplyr::as_data_frame(purrr::map(sections, getsec))    
}

form4_footnotes <- function(xml) {
    fn <- xml_children( xml_find_all(xml, "//footnotes") )
    ids <- fn %>% xml_attr("id")
    text <- fn %>% xml_text
    if (length(ids) != length(text)) stop("footnote id and text mistmatched")
    data_frame(footnote_id = ids, footnote = text)
}

form4_filing_date <- function(txt) {
    dt_line <- str_which(txt, "^<SEC-DOCUMENT>.+[0-9]+$")
    str_extract(txt[dt_line], "[0-9]+$")
}

sec <- data_frame(file = list.files("data/2018/q2/", 
                                    full.names = TRUE,
                                    recursive = TRUE)) %>% 
    filter(str_detect(file, "\\.txt$"))

sec <- sec %>% 
    mutate(info = basename(file), 
           cik = str_extract(info, "^[0-9]+\\-") %>% str_replace_all("[^0-9]", ""), 
           accession = str_replace_all(info, "^[0-9]+\\-", "") %>% str_replace_all("\\.txt$", "")) %>% 
    select(file, cik, accession) %>% 
    mutate(cik = as.integer(cik)) %>% 
    inner_join(matches %>% mutate(cik = as.integer(cik)), by = "cik")

sec_footnotes <- sec %>% 
    mutate(text = map(file, readLines),
           xml = map(text, parse_filing),
           footnotes = map(xml, form4_footnotes))

sec_footnotes <- sec_footnotes %>% 
    select(accession, footnotes) %>% unnest

sec_footnotes <- sec_footnotes %>% distinct

# check:
sec_footnotes %>% 
    group_by(accession, footnote_id) %>% 
    filter(n() > 1) 

sec <- sec %>% 
    mutate(
        text = map(file, readLines),
        xml = map(text, parse_filing),
        filing_date = map_chr(text, form4_filing_date),
        header = map(xml, form4_header),
        issuer = map(xml, form4_issuer),
        nonderiv = map(xml, form4_non_deriv),
        deriv = map(xml, form4_deriv))

sec_main <- sec %>% 
    select(cik, accession, filing_date:deriv)

sec_hdr <- sec_main %>% select(accession, filing_date, header) %>% 
    unnest %>% 
    mutate(cik = as.integer(reporting_owner_cik)) %>% 
    select(cik, accession, filing_date, reporting_owner:other_text) %>% 
    distinct

# check: should be 0 rows
sec_hdr %>% group_by(cik, accession) %>% filter(n() > 1)

# format:
sec_hdr <- sec_hdr %>% 
    mutate_at(vars(starts_with("is")), ~case_when(
        . == "1" ~ 1L,
        . == "0" ~ 0L,
        . == "true" ~ 1L,
        . == "false" ~ 0L,
        . == "" ~ 0L
    )) %>% 
    mutate(filing_date = lubridate::ymd(filing_date)) %>% 
    mutate_at(vars(accession, reporting_owner, 
                   officer_title, other_text), 
              str_trim)

sec_issuer <- sec %>% select(accession, issuer) %>% unnest %>% distinct
sec_issuer <- sec_issuer %>% 
    mutate(ticker = str_to_upper(ticker),
           ticker = str_replace_all(ticker, '\\[|\\]|\\(|\\)|\\"', "")) %>% 
    mutate(ticker_neat = case_when(
        ticker %in% c("NONE", "N/A", "NA") ~ NA_character_,
        str_detect(ticker, "\\:") ~ str_extract(ticker, "[A-Z]+$"),
        str_detect(ticker, "\\.|/|,|\\s") ~ str_extract(ticker, "^[A-Z]+"),
        TRUE ~ ticker
    )) 

sec_issuer <- sec_issuer %>% 
    mutate_all(str_trim) %>% 
    mutate(issuer_cik = as.integer(issuer_cik))
# check: should be 0 rows
sec_issuer %>% group_by(accession) %>% filter(n() > 1)

sec_nonderiv <- sec_main %>% select(accession, nonderiv) %>% unnest %>% 
    mutate_all(str_trim) %>% 
    mutate(transaction_date = str_sub(transaction_date, 1, 10)) %>% 
    mutate(
        transaction_date = lubridate::ymd(transaction_date),
        deemed_execution_date = lubridate::ymd(deemed_execution_date),
        shares = as.numeric(shares),
        price_share = as.numeric(price_share),
        post_transaction_shares = as.numeric(post_transaction_shares),
        post_transaction_value = as.numeric(post_transaction_value)
    ) %>% 
    group_by(accession) %>% 
    mutate(xsequence = seq_along(accession)) %>%
    ungroup %>% 
    select(accession, xsequence, everything())

sec_deriv <- sec_main %>% select(accession, deriv) %>% unnest %>% 
    mutate_all(str_trim) %>% 
    mutate_at(vars(ends_with("_date")), ~str_sub(., 1, 10) %>% lubridate::ymd()) %>% 
    mutate(
        conv_ex_price = as.numeric(conv_ex_price),
        shares = as.numeric(shares),
        transaction_total_value = as.numeric(transaction_total_value),
        underlying_security_shares = as.numeric(underlying_security_shares),
        underlying_security_value = as.numeric(underlying_security_value),
        price_share = as.numeric(price_share),
        post_transaction_shares = as.numeric(post_transaction_shares),
        post_transaction_value = as.numeric(post_transaction_value)
    ) %>% 
    group_by(accession) %>% 
    mutate(xsequence = seq_along(accession)) %>%
    ungroup %>% 
    select(accession, xsequence, everything())

### sec_hdr should include issuer info:
## (first make sure that no accession no.s in issuer are missing from hdr)
sec_hdr <- sec_hdr %>% inner_join(sec_issuer, by = "accession")


Sys.setenv(TZ = "DB_TZ")
Sys.setenv(ORA_SDTZ = "DB_TZ")
cdw <- getcdw::connect("URELUAT_DEVEL")
getcdw::get_cdw("delete from rdata.sec_hdr_stage", dsn = "URELUAT_DEVEL")
ROracle::dbCommit(cdw)

res <- ROracle::dbWriteTable(
    cdw, "SEC_HDR_STAGE", sec_hdr, 
    schema = 'RDATA',
    overwrite = FALSE, append = TRUE)
ROracle::dbCommit(cdw)

getcdw::get_cdw("
                insert into rdata.sec_hdr
                select * from sec_hdr_stage
                ", dsn = "URELUAT_DEVEL")
ROracle::dbCommit(cdw)

#######

sec_nonderiv <- sec_nonderiv %>% 
    mutate_at(vars(transaction_code, acquired_disposed, direct_indirect),
              ~ifelse(. == "", NA_character_, .)) %>% 
    mutate(equity_swap = case_when(
        equity_swap == "1" ~ 1L,
        equity_swap == "0" ~ 0L,
        equity_swap == "true" ~ 1L,
        equity_swap == "false" ~ 0L,
        equity_swap == "" ~ 0L
    ))

getcdw::get_cdw("delete from rdata.sec_nonderiv_stage", dsn = "URELUAT_DEVEL")
ROracle::dbCommit(cdw)
res <- ROracle::dbWriteTable(
    cdw, "SEC_NONDERIV_STAGE", sec_nonderiv, 
    schema = 'RDATA',
    overwrite = FALSE, append = TRUE)
ROracle::dbCommit(cdw)

getcdw::get_cdw("
                insert into rdata.sec_nonderiv
                select * from sec_nonderiv_stage
                ", dsn = "URELUAT_DEVEL")
ROracle::dbCommit(cdw)

######


sec_deriv <- sec_deriv %>% 
    mutate_at(vars(transaction_code, acquired_disposed, direct_indirect),
              ~ifelse(. == "", NA_character_, .)) %>% 
    mutate(equity_swap = case_when(
        equity_swap == "1" ~ 1L,
        equity_swap == "0" ~ 0L,
        equity_swap == "true" ~ 1L,
        equity_swap == "false" ~ 0L,
        equity_swap == "" ~ 0L
    ))

getcdw::get_cdw("delete from rdata.sec_deriv_stage", dsn = "URELUAT_DEVEL")
ROracle::dbCommit(cdw)

res <- ROracle::dbWriteTable(
    cdw, "SEC_DERIV_STAGE", sec_deriv, 
    schema = 'RDATA',
    overwrite = FALSE, append = TRUE)
ROracle::dbCommit(cdw)

getcdw::get_cdw("
                insert into rdata.sec_deriv
                select * from sec_deriv_stage
                ", dsn = "URELUAT_DEVEL")
ROracle::dbCommit(cdw)

## also add unverified cik dictionary
cik_entity_dictionary <- getcdw::get_cdw("select * from rdata.sec_cik_dict minus select * from rdata.sec_blacklist")
cik_entity_dictionary %>% group_by(entity_id) %>% filter(n() > 1) %>% arrange(entity_id)

cik_blacklist <- getcdw::get_cdw("select * from rdata.sec_blacklist")

new_dict_entries <- dplyr::setdiff(
    matches %>% 
        mutate(cik = as.integer(cik)) %>% 
        select(entity_id, cik),
    cik_entity_dictionary) %>% 
    anti_join(cik_blacklist, by = c("entity_id", "cik"))

res <- ROracle::dbWriteTable(
    cdw, "SEC_CIK_DICT", new_dict_entries, 
    schema = 'RDATA',
    overwrite = FALSE, append = TRUE)
ROracle::dbCommit(cdw)

#############

getcdw::get_cdw("delete from rdata.sec_footnote_stage", dsn = "URELUAT_DEVEL")
res <- ROracle::dbWriteTable(
    cdw, "SEC_FOOTNOTE_STAGE", sec_footnotes, 
    schema = 'RDATA',
    overwrite = FALSE, append = TRUE)
ROracle::dbCommit(cdw)

getcdw::get_cdw("
                insert into rdata.sec_footnote
                select * from rdata.sec_footnote_stage
                ", dsn = "URELUAT_DEVEL")
ROracle::dbCommit(cdw)
