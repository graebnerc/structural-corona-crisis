countries <- list()
countries[["Core"]] <- countrycode(c("Austria", "Belgium", "Denmark", 
                                              "Finland", "Germany", "Sweden"), 
                                            "country.name", "iso3c")
countries[["Catchup"]] <- countrycode(c("Bulgaria", "Romania", 
                                                 "Czech Republic", "Estonia", 
                                                 "Latvia", "Lithuania", 
                                                 "Hungary", "Poland", "Slovenia", 
                                                 "Slovakia", "Croatia"), 
                                               "country.name", "iso3c")
countries[["Finance"]] <- countrycode(c("Luxembourg", "Netherlands", 
                                                 "Malta", "Ireland"), 
                                               "country.name", "iso3c")
countries[["Periphery"]] <- countrycode(c("Cyprus", "France", "Greece", 
                                                   "Italy", "Portugal", "Spain"), 
                                                 "country.name", "iso3c")
countries_all <- unlist(countries)
