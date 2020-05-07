library(fst)
library(here)
library(data.table)
library(countrycode)
library(tidyverse)
export_data_source <- "HARV"
update_data <- FALSE

# Get export data from Harvard=================================================
# http://atlas.cid.harvard.edu/downloads
if (export_data_source=="HARV"){
  export_data_file_name <- here("data/hrvd_complexity_atlas.fst")
  if (update_data){
    web_link <- "https://intl-atlas-downloads.s3.amazonaws.com/country_sitcproduct4digit_year.csv.zip"
    export_data_raw <- fread(cmd = paste0("curl ", web_link, " | funzip"),
                             colClasses = c(rep("double", 11), 
                                            rep("character", 4)), 
                             select = c("year", "export_value", 
                                        "location_code", "sitc_product_code"))
    export_data_raw <- export_data_raw[location_code%in%countrycode(
      countries_considered, "iso2c", "iso3c")]
    fst::write.fst(x = export_data_raw, 
                   path = export_data_file_name, compress = 100)
  } else{
    export_data_raw <- fst::read.fst(export_data_file_name, 
                                     as.data.table = T)
  }
}
export_data_raw[, year:=as.double(year)
                ][, export_value:=as.double(export_value)
                  ][, total_exports:=sum(export_value, na.rm = T), 
                    .(location_code, year)]

# Oil shares of total exports==================================================

# For SITC codes see: 
# https://unstats.un.org/unsd/tradekb/Knowledgebase/50262/Search-SITC-code-description
oil_codes <- c("33", "34") 
# 33	Petroleum, petroleum products and related materials 
# 34	Gas, natural and manufactured
oil_exports <- copy(export_data_raw)
oil_exports[, sitc_red:=substr(sitc_product_code, 1, 2)
            ][sitc_red%in%oil_codes, 
              oil_export:=sum(export_value, na.rm = T), 
              .(year, location_code)
              ][, oil_exports_share:=oil_export/total_exports]
oil_exports <- unique(oil_exports[!is.na(oil_exports_share)
                                  ][, .(year, location_code, oil_exports_share)])
head(oil_exports)

# Test with dplyr -------------------------------------------------------------
test_oil_exports <- copy(export_data_raw)
test_oil_exports <- test_oil_exports %>%
  dplyr::filter(location_code=="AUT" & year==1989) %>%
  dplyr::mutate(sitc2=substr(sitc_product_code, 1, 2)) %>%
  dplyr::filter(sitc2 %in% oil_codes) %>%
  dplyr::mutate(oil_exp_share=sum(export_value, na.rm = T)/total_exports)

# Coal and metal share of total exports========================================

# For SITC codes see: 
# https://unstats.un.org/unsd/tradekb/Knowledgebase/50262/Search-SITC-code-description
coal_and_metal_codes_2 <- c("32", "35", "28", "68", "97")
# 32	Coal, coke and briquettes
# 35	Electric current
# 28	Metalliferous ores and metal scrap
# 68	Non-ferrous metals
# 97	Gold, non-monetary (excluding gold ores and concentrates)

coal_and_metal_codes_4 <- c("5224", "5231", "5232", "5233")
# 5224	Metallic oxides of zinc, iron, lead, chromium etc
# 5231	Metallic salts and peroxysalts of inorganic acids
# 5232	Metallic salts and peroxysalts of inorganic acids
# 5233	Salts of metallic acids; compounds of precious metals

# Eher nicht:
# 69	Manufactures of metals, nes
# 691	Structures and parts, nes, of iron, steel or aluminium
coal_metal_shares <- copy(export_data_raw)
coal_metal_shares[, sitc2:=substr(sitc_product_code, 1, 2)
                  ][, sitc4:=sitc_product_code
                    ][, coal_metal:=ifelse(
                      sitc2 %in% coal_and_metal_codes_2 | 
                        sitc4 %in% coal_and_metal_codes_4, 
                      TRUE, FALSE)]
coal_metal_shares[coal_metal==TRUE,
                  coal_metal_exports:=sum(export_value, na.rm = T), 
                  .(year, location_code)
                  ][ , coal_metal_export_share:=coal_metal_exports/total_exports]
coal_metal_shares <- unique(coal_metal_shares[!is.na(coal_metal_export_share), 
                                              .(year, location_code, coal_metal_export_share)])
head(coal_metal_shares)

# Test with dplyr -------------------------------------------------------------
test_coal_metal_shares <- copy(export_data_raw)
test_coal_metal_shares <- test_coal_metal_shares %>%
  dplyr::filter(location_code=="AUT" & year==1988) %>%
  dplyr::mutate(sitc2=substr(sitc_product_code, 1, 2),
                sitc4=substr(sitc_product_code, 1, 4)) %>%
  dplyr::filter(sitc2 %in% coal_and_metal_codes_2 | sitc4 %in% coal_and_metal_codes_4) %>%
  dplyr::mutate(coal_exp_share=sum(export_value, na.rm = T)/total_exports)

# Share of primary exports=====================================================

primary_goods_codes_1 <- c("0", "1", "2", "4")
primary_goods_codes_2 <- c(primary_goods_codes_1, "3")
# In jedem Fall:
# 0	Food and live animals chiefly for food
# 2	Crude materials, inedible, except fuels
# 1	Beverages and tobacco
# 4	Animal and vegetable oils, fats and waxes
# Unklar:
# 3	Mineral fuels, lubricants and related materials
# TODO das stimmt noch nicht: nicht die subsets genommen, und gibt summe>100
primary_exports_data <- copy(export_data_raw)
primary_exports_data[, sitc_main:=substr(sitc_product_code, 1, 1)
                     ][sitc_main%in%primary_goods_codes_1, 
                       export_primary_1:=sum(export_value, na.rm = T), 
                       .(year, location_code)
                       ][sitc_main%in%primary_goods_codes_2, 
                         export_primary_2:=sum(export_value, na.rm = T), 
                         .(year, location_code)
                         ][, primary_exports_share_1:=export_primary_1/total_exports
                           ][, primary_exports_share_2:=export_primary_2/total_exports]
primary_exports_data <-  unique(
  primary_exports_data[!is.na(primary_exports_share_1) & !is.na(primary_exports_share_2), 
                       .(year, location_code, 
                         primary_exports_share_1, primary_exports_share_2)]
)

# Test with dplyr -------------------------------------------------------------
head(primary_exports_data)
test_primary_goods_shares <- copy(export_data_raw)
test_primary_goods_shares <- test_primary_goods_shares %>%
  dplyr::filter(location_code=="AUT" & year==1988) %>%
  dplyr::mutate(sitc1=substr(sitc_product_code, 1, 1)) %>%
  dplyr::filter(sitc1 %in% primary_goods_codes_2) %>%
  dplyr::mutate(primary_exp_share=sum(export_value, na.rm = T)/total_exports)

# Merge data ------------------------------------------------------------------
head(coal_metal_shares)
head(oil_exports)
head(primary_exports_data)

resource_data <- coal_metal_shares[oil_exports, on = .(year, location_code)] 
resource_data <- resource_data[primary_exports_data, on = .(year, location_code)] 
fwrite(x = resource_data, file = here("data/resource_data.csv"))