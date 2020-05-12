countries <- list()
countries[["north"]] <- countrycode::countrycode(
  c("Austria", "Belgium", "Finland", "Germany", "Netherlands"),
  "country.name", "iso3c")
countries[["south"]] <- countrycode::countrycode(
  c("Greece", "Italy", "Portugal",  "Spain"),
  "country.name", "iso3c")
countries[["france"]] <- countrycode::countrycode(
  c("France"), "country.name", "iso3c")
countries_all <- unlist(countries)