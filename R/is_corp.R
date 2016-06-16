# function to identify/remove corporations and leave only individuals
is_corp <- function(name) {
    str_detect(name, regex("(\\bCORP\\b)|(\\bINTERNATIONAL\\b)|(\\bLABORATORIES\\b)", ignore_case = TRUE)) |
        str_detect(name, regex("/|&|(\\bCO\\b)", ignore_case = TRUE)) |
        str_detect(name, regex("\\bINC\\b", ignore_case = TRUE)) |
        str_detect(name, regex("LTD", ignore_case = TRUE)) |
        str_detect(name, regex("(\\bL(\\.?)L(\\.?)C(\\.?)\\b)|(\\bPLC\\b)|(\\bLP\\b)", ignore_case = TRUE)) |
        str_detect(name, regex("(\\bPARTNERS\\b)|(\\bFUND\\b)", ignore_case = TRUE)) |
        str_detect(name, "^[0-9]") |
        str_detect(name, regex("partnership", ignore_case = TRUE)) |
        str_detect(name, regex("venture", ignore_case = TRUE)) |
        str_detect(name, regex("\\baccount\\b", ignore_case = TRUE)) |
        str_detect(name, regex("\\btrust\\b", ignore_case = TRUE)) |
        str_detect(name, regex("\\bL(\\.?)P(\\.?)\\b", ignore_case = TRUE)) |
        str_detect(name, regex("\\bfunds\\b", ignore_case = TRUE)) | 
        str_detect(name, regex("\\bfoods\\b", ignore_case = TRUE)) |
        str_detect(name, regex("\\binvestment(s?)\\b", ignore_case = TRUE)) |
        str_detect(name, regex("mortgage", ignore_case = TRUE)) |
        str_detect(name, regex("corporation", ignore_case = TRUE)) |
        str_detect(name, regex("\\bsecurit", ignore_case = TRUE)) |
        str_detect(name, regex("\\bholdings\\b", ignore_case = TRUE)) |
        str_detect(name, regex("\\bportfolio", ignore_case = TRUE)) |
        str_detect(name, regex("^AB\\b", ignore_case = TRUE)) |
        str_detect(name, regex("^FT\\b", ignore_case = TRUE)) |
        str_detect(name, regex("\\binvesco\\b", ignore_case = TRUE)) |
        str_detect(name, regex("^[^\\s]+$", ignore_case = TRUE)) |
        str_detect(name, regex("\\bcapital\\b", ignore_case = TRUE)) |
        str_detect(name, regex("\\bamerican\\b", ignore_case = TRUE)) |
        str_detect(name, regex("unit trust", ignore_case = TRUE)) |
        str_detect(name, regex("\\bseries\\b", ignore_case = TRUE)) |
        str_detect(name, regex("municipal", ignore_case = TRUE)) |
        str_detect(name, regex("taxable", ignore_case = TRUE)) |
        str_detect(name, regex("\\bincome\\b", ignore_case = TRUE)) |
        str_detect(name, regex("\\basset(s?)\\b", ignore_case = TRUE)) |
        str_detect(name, regex("management", ignore_case = TRUE))
}
