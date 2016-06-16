# craeate cads name frequency tables
cads_frequency_first <- cads_names %>%
    select(entity_id, first_name) %>%
    distinct %>%
    group_by(first_name) %>%
    summarise(cads_first = n_distinct(entity_id)) %>%
    mutate(n_cads_first = sum(cads_first))

cads_frequency_last <- cads_names %>%
    select(entity_id, last_name) %>%
    distinct %>%
    group_by(last_name) %>%
    summarise(cads_last = n_distinct(entity_id)) %>%
    mutate(n_cads_last = sum(cads_last))

# now use sec and cads frequencies to create weights for a match on first/last
# name. for example, "john smith" match would not create a ton of confidence 
# that two records correspond to same person, but "zorro zamboni" would
first_wt <- sec_first %>%
    inner_join(cads_frequency_first, by = "first_name") %>%
    mutate(n_sec_first = as.numeric(n_sec_first), 
           n_cads_first = as.numeric(n_cads_first)) %>%
    mutate(h = ifelse(sec_first_count > 1 | cads_first > 1,
                      pmin(sec_first_count, cads_first),
                      .6)) %>%
    mutate(nab = sum(h),
           m_first = .98 * h / sum(h),
           u_first = .98 * (sec_first_count * cads_first - h) / (n_sec_first * n_cads_first - sum(h))) %>%
    mutate(weight = log2(m_first / u_first)) %>%
    select(first_name, first_weight = weight)


last_wt <- sec_last %>%
    inner_join(cads_frequency_last, by = "last_name") %>%
    replace_na(list(cads_last = 0, n_cads_last = 0,
                    sec_last_count = 0, n_sec_last = 0)) %>%
    mutate(h = ifelse(sec_last_count > 1 | cads_last > 1,
                      pmin(sec_last_count, cads_last),
                      .6)) %>%
    mutate(m_last = h / sum(h),
           u_last = .98 * (sec_last_count * cads_last - h) / (n_sec_last * n_cads_last - sum(h))) %>%
    mutate(wt = log2(m_last / u_last)) %>%
    select(last_name, 
           last_weight = wt)