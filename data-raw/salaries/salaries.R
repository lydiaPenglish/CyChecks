# Creates salaries.csv

salaries <- CyChecks2::cyf_getsals(token = "$$app_token=GJISvIEj4Jg2KmwRkV3oCGJrj")

readr::write_csv(salaries, path = "salaries.csv")
