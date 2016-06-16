pulls selected sec form 3/4/5 filings from edgar, parses data into a table, and attempts to match the data to entity ids in CADS. 

the parsers are ad-hoc and could be much improved, but work for this narrow application. see `parsers_mail_address.R` and `parsers_main.R`. 

the match logic is also ad-hoc, and could be improved. it happens in `4-match-to-cads.R`, and is currently based on exact first/last name match plus approximate match on company or address information. extra weight is given to matches on uncommon names. matches for all but the most uncommon of names depend on having accurate and up-to-date employment information in cADS.

all data comes from the sec, and is downloaded into the `data/` directory (created if it doesn't exist). documentation is built and placed in the `docs/` directory (created if it doesn't exist) during `2-build-sec-dict.R`. 
