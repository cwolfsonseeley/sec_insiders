name_candidates <- processed_filings %>%
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
    select(cik, first_name, last_name, middle_initial) %>% distinct %>%
    inner_join(cads_names, by = c("first_name", "last_name")) %>%
    mutate(sec_mi = middle_initial, cads_mi = str_sub(middle_name, 1, 1)) %>%
    filter(nchar(sec_mi) == 0 | nchar(cads_mi) == 0 | is.na(sec_mi) | is.na(cads_mi) |
               sec_mi == cads_mi) %>% 
    select(cik, entity_id, first_name, last_name) %>% distinct %>%
    inner_join(first_wt, by = "first_name") %>%
    inner_join(last_wt, by = "last_name") %>%
    transmute(cik, entity_id, name_weight = first_weight + last_weight)

name_candidates %>%
    inner_join(processed_filings, by = "cik") %>%
    select(cik, entity_id, name_weight, 
           street1, street2, city, state, zip,
           title, company_name) %>% 
    distinct -> cands

cads_addresses <- getcdw::get_cdw("
                                  select distinct
                                  entity_id,
                                  care_of,
                                  company_name_1,
                                  street1, street2,
                                  city,
                                  state_code,
                                  zipcode5
                                  from 
                                  cdw.d_bio_address_mv
                                  where 
                                  contact_type_desc = 'ADDRESS'
                                  and entity_id in (
                                  select entity_id from cdw.d_entity_mv where person_or_org = 'P' and record_status_code = 'A')
                                  ")

cads_employers <- getcdw::get_cdw("
select distinct
  emp.entity_id,
  emp.job_title,
  nm.report_name as cads_employer_name 
  from CDW.d_bio_employment_mv emp
  inner join cdw.d_bio_name_mv nm on emp.employer_entity_id = nm.entity_id
  where emp.entity_id in (
    select entity_id 
    from cdw.d_entity_mv 
    where  
    person_or_org = 'P' 
    and record_status_code = 'A')
 ")

# flip employer names to lower case (address etc handled below)
cads_employers %<>% 
    mutate(job_title = str_trim(str_to_lower(job_title)),
           cads_employer_name = str_trim(str_to_lower(cads_employer_name)))

# now find possible matches by comparing various combinations of fields 
# this part is sloppy and ad hoc and could really be improved. 
library(stringdist)
cands %>% 
    left_join(cads_addresses, by = "entity_id") %>%
    mutate(sec_add = paste(tolower(street1.x), tolower(street2.x), sep = " ") %>% str_trim,
           cad_add = paste(tolower(care_of), tolower(company_name_1), 
                           tolower(street1.y), tolower(street2.y), sep = " ") %>% str_trim,
           title = str_trim(str_to_lower(title)), 
           company_name = str_trim(str_to_lower(company_name))) %>%
    left_join(cads_employers, by = "entity_id") %>%
    mutate(sec_job = paste(title, company_name),
           cads_job = ifelse(title == "", 
                             cads_employer_name, 
                             paste(job_title, cads_employer_name))) %>%
    mutate(state_match = state == state_code,
           zip_match = zip == zipcode5,
           street_dist = stringdist(sec_add, cad_add, 
                                    method = "cosine", 
                                    q = 5, nthread = 1),
           company_dist = stringdist(sec_job, 
                                     cads_job, 
                                     method = "cosine",
                                     q = 4,
                                     nthread = 1),
           cdist2 = stringdist(company_name, cads_employer_name, 
                               method = "cosine", q = 3, nthread = 1),
           company_dist = pmin(company_dist, cdist2)) %>% 
    select(cik, entity_id, name_weight, 
           state_match, zip_match, street_dist, company_dist) %>% 
    group_by(cik, entity_id) %>% 
    mutate(min_address = min(street_dist, na.rm = TRUE),
           min_company = min(company_dist, na.rm = TRUE),
           max_state_match = max(state_match),
           max_zip_match = max(zip_match)) %>%
    ungroup %>%
    filter(street_dist == min_address | company_dist == min_company |
               zip_match) %>% 
    filter(name_weight > 27 | (min_address <= .65 & max_zip_match) |
               (company_dist <= .65 & max_state_match)) %>% 
    #group_by(cik) %>% mutate(minstreet = min(street_dist)) %>% ungroup %>%
    #filter(street_dist == minstreet) %>% 
    select(cik, entity_id) %>% 
    distinct -> matches

# cik's that are already verified (are in CADS) should be part of matches, and 
# supercede any entity_id candidates already in matches

matches$verified <- FALSE
verified_cik <- verified_cik %>%
    rename(cik = cads_cik, 
           entity_id = entity_id_v) %>% 
    mutate(cik = str_pad(cik, width = 10, pad = "0"),
           verified = TRUE)

matches <- bind_rows(matches, verified_cik) %>%
    group_by(cik) %>%
    filter(verified == max(verified)) %>%
    select(-verified) %>% ungroup

# sanity check:
dplyr::setdiff(verified_cik[, c("entity_id", "cik"), drop = FALSE], matches)

# append stock prices using the ticker-price from 1-dl-and-preprocess.R
# also just keep the most recent filing for each filer-company pair,
# in case multiple filings on same date, break ties by most shares reported. 
# total_value is just no. of shares * share price
library(lubridate)
matches %>%
    inner_join(processed_filings, by = "cik") %>%
    mutate(report_date = str_sub(report_date, 1, 10),
           report_date = as.Date(ymd(report_date)),
           direct_shares = pmax(direct_shares, 0),
           indirect_shares = pmax(indirect_shares, 0)) %>%
    transmute(cik, entity_id, company_name, report_date,
              director = as.numeric(director),
              officer = as.numeric(officer),
              ten_percenter = as.numeric(ten_percenter),
              ticker, direct_shares, indirect_shares,
              total_shares = direct_shares + indirect_shares) %>%
    mutate(ticker = str_match(ticker, "^(.+:\\s*)?([A-Za-z]+)")[,3]) %>%
    filter(!is.na(direct_shares) | !is.na(indirect_shares)) %>%
    group_by(cik, entity_id, ticker) %>%
    mutate(recent = max(report_date)) %>%
    ungroup %>% filter(report_date == recent) %>%
    group_by(cik, entity_id, ticker) %>%
    mutate(maxshares = max(total_shares)) %>% 
    ungroup %>% filter(total_shares == maxshares) %>%
    left_join(prices, by = c("ticker" = "symbol")) %>%
    select(cik:total_shares, price = Last) %>%
    mutate(price = as.numeric(price),
           total_value = price * total_shares) %>%
    distinct -> matched_financials

## need to add employer/sic
context <- getcdw::get_cdw("
                           select 
                           ent.entity_id, 
                           ent.report_name,
                           p.capacity_rating_desc, 
                           (select max(weight) from cdw.d_bio_demographic_profile_mv where dp_rating_type_code = 'CAP' and entity_id = ent.entity_id) as implied_capacity,
                           record_types,
                           degree_major_year,
                           emp_job_title, 
                           (select report_name from cdw.d_entity_mv where entity_id = ent.employer_entity_id) as employer,
                           fld_of_work_desc,
                           sic_code_desc, 
                           fld_of_spec_1_desc,
                           (select total_raised_amt from cdw.sf_entity_summary_mv where entity_id = ent.entity_id) as total_giving
                           from cdw.d_entity_mv ent
                           left outer join cdw.sf_entity_based_prspct_smry_mv p
                           on ent.entity_id = p.entity_id
                           where ent.person_or_org = 'P' and ent.record_status_code = 'A'
                           ")

matched_financials %>%
    group_by(cik, entity_id, ticker, report_date) %>% 
    summarise_all(funs(max(., na.rm = TRUE))) %>%
    ungroup %>%
    inner_join(context, by = "entity_id") %>%
    write.csv("details.csv", row.names = FALSE)

# matched_financials %>%
#     group_by(cik, entity_id) %>%
#     summarise(total_value = sum(total_value, na.rm = TRUE)) %>%
#     ungroup %>%
#     group_by(cik) %>% mutate(count = n_distinct(entity_id)) %>% ungroup %>%
#     inner_join(context, by = "entity_id") %>%
#     write.csv("entity_summary.csv", row.names = FALSE)