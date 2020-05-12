countries <- list()
countries[["north"]] <- countrycode::countrycode(
  c("Austria", "Belgium", "Finland", "Germany", "Denmark", "Sweden"),
  "country.name", "iso3c")
countries[["south"]] <- countrycode::countrycode(
  c("Greece", "Italy", "Portugal",  "Spain"),
  "country.name", "iso3c")
countries[["finance"]] <- countrycode(c("Luxembourg", "Netherlands", 
                                        "Malta", "Ireland"), 
                                      "country.name", "iso3c")
countries[["france"]] <- countrycode::countrycode(
  c("France"), "country.name", "iso3c")
countries_all <- unlist(countries)



