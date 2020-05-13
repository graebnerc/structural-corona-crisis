rm(list=ls())
library(countrycode)
library(data.table)
library(tidyverse)
library(ggrepel)
library(ggpubr)
library(here)
library(reldist)
library(latex2exp)
library(scales)
if (!require("icaeDesign")){
  devtools::install_github("graebnerc/icaeDesign")
}
source(here("R/setup_country_classification_North_South.R"))

download_bond_data <- F
download_eurostat_debt <- F
macro_data <- fread(here("data/macro_data_fig1-2_4_NEW.csv"))

start_year <- 2000
end_year <- 2020

plots_title_size <- 8
plots_axis_title_size <- 7
plots_axis_ticks_size <- 7

x_axis_breaks <- c(2000, 2005, 2007, 2010, 2015, 2020)

# Figure 1: Diverging development trajectories=================================
# 1.1. Deviation from mean income----------------------------------------------
dev_mean_data <- data.table::copy(macro_data)
dev_mean_plot <- dev_mean_data[!is.na(population) & !is.na(gdp_real_pc_ppp), 
                               .(year, iso3c, gdp_real_pc_ppp, population,
                  cluster=ifelse(iso3c %in% countries[["north"]], 
                                 "Northern euro area", ifelse(
                                   iso3c %in% countries[["south"]], 
                                        "Southern euro area", ifelse(
                                          iso3c %in% countries[["france"]], 
                                               "France", NA))))
                  ][!is.na(cluster) & year>=start_year & year <= end_year] %>%
  group_by(year, cluster) %>%
  summarise(gdp_real_pc_ppp_sd=sd(gdp_real_pc_ppp),
            gdp_real_pc_ppp=weighted.mean(gdp_real_pc_ppp, w = population)
            ) %>% 
  ungroup() %>%
  group_by(year) %>%
  dplyr::mutate(mean.GDP_pc_PPP=mean(gdp_real_pc_ppp, na.rm = T)) %>%
  ungroup() %>%
  dplyr::mutate(dev.mean.GDP_pc_PPP = (gdp_real_pc_ppp - mean.GDP_pc_PPP)/1000
                ) %>%
  ggplot(., 
         aes(x=year, y=dev.mean.GDP_pc_PPP, color=cluster)) +
  geom_line() + geom_point() +
  ggtitle("Deviation of GDP p.c. from EA-average (population-weighted)") + 
  ylab("Deviation of GDP p.c. (PPP)\n from euro area average\n in thousand EUR") +
  scale_y_continuous(labels = scales::number_format(suffix = "k")) +
  theme_icae() +
  scale_x_continuous(
    breaks=x_axis_breaks, 
    expand = expand_scale(
      mult = c(0, 0), add = c(0, 0.5)
    )
  ) +
  scale_color_icae(palette = "mixed") + 
  theme(legend.text=element_text(size=7)) +
  theme(axis.title = element_text(color="black", size=plots_axis_title_size),
        plot.title = element_text(color="black", size=plots_title_size),
        axis.text = element_text(color="black", size=plots_axis_ticks_size))
dev_mean_plot

# 1.2 Unemployment rate--------------------------------------------------------
start_year <- 2000
end_year <- 2020
x_axis_breaks <- c(2000, 2005, 2007, 2010, 2015, 2020)
unemp_plot <- macro_data %>%
  dplyr::select(dplyr::one_of(
    "year", "iso3c", "unemp_rate", "population")
  ) %>%
  dplyr::rename(population=population) %>%
  dplyr::mutate(is.north=ifelse(iso3c %in% countries[["north"]],
                               "Northern euro area", ifelse(
                                 iso3c %in% countries[["south"]], 
                                      "Southern euro area", NA))
  ) %>%
  dplyr::mutate(is.north=ifelse(iso3c=="FRA", "France", is.north)
  ) %>%
  dplyr::filter(!is.na(is.north)
  ) %>%
  dplyr::mutate(is.north=as.factor(is.north)
  ) %>%
  dplyr::filter(year>=start_year & year <= end_year) %>%
  dplyr::group_by(year, is.north) %>%
  dplyr::mutate(population_group=sum(population)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(pop_rel_group=population / population_group) %>%
  dplyr::group_by(year, is.north) %>%
  dplyr::mutate(test_pop=sum(pop_rel_group)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(year, is.north) %>%
  dplyr::summarise(
    unemp_rate_mean=weighted.mean(unemp_rate, 
                                  pop_rel_group),
    unemp_rate_sd=sd(unemp_rate*pop_rel_group)
  ) %>%
  dplyr::ungroup() %>%
  ggplot(., aes(x=year, y=unemp_rate_mean, color=is.north)) +
  geom_point() + 
  geom_line() +
  ylab("in % of active population") +
  scale_color_icae(palette = "mixed", 
                                  aesthetics=c("color", "fill")
                                  ) +
  ggtitle("Unemployment rates in euro area North/South (population-weighted)") + 
  scale_x_continuous(
    breaks=x_axis_breaks, 
    expand = expand_scale(
      mult = c(0, 0), add = c(0, 0.5)
      )
    ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 1)
    ) +
  theme_icae() + 
  labs(x = "Source: AMECO (Spring 2020 forecast), own calculations") +
  theme(legend.text=element_text(size=7)) +
  theme(axis.title = element_text(color="black", size=plots_axis_title_size),
        plot.title = element_text(color="black", size=plots_title_size),
        axis.text = element_text(color="black", size=plots_axis_ticks_size))
unemp_plot 

# 1.2 NAWRU--------------------------------------------------------
start_year <- 2000
end_year <- 2020
x_axis_breaks <- c(2000, 2005, 2007, 2010, 2015, 2020)
NAWRU_plot <- macro_data %>%
  dplyr::select(dplyr::one_of(
    "year", "iso3c", "NAWRU", "population")
  ) %>%
  dplyr::rename(population=population) %>%
  dplyr::mutate(is.north=ifelse(iso3c %in% countries[["north"]],
                                "Northern euro area", ifelse(
                                  iso3c %in% countries[["south"]], 
                                  "Southern euro area", NA))
  ) %>%
  dplyr::mutate(is.north=ifelse(iso3c=="FRA", "France", is.north)
  ) %>%
  dplyr::filter(!is.na(is.north)
  ) %>%
  dplyr::mutate(is.north=as.factor(is.north)
  ) %>%
  dplyr::filter(year>=start_year & year <= end_year) %>%
  dplyr::group_by(year, is.north) %>%
  dplyr::mutate(population_group=sum(population)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(pop_rel_group=population / population_group) %>%
  dplyr::group_by(year, is.north) %>%
  dplyr::mutate(test_pop=sum(pop_rel_group)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(year, is.north) %>%
  dplyr::summarise(
    NAWRU_mean=weighted.mean(NAWRU, 
                                  pop_rel_group),
    NAWRU_sd=sd(NAWRU*pop_rel_group)
  ) %>%
  dplyr::ungroup() %>%
  ggplot(., aes(x=year, y=NAWRU_mean, color=is.north)) +
  geom_point() + 
  geom_line() +
  ylab("in % of active population") +
  scale_color_icae(palette = "mixed", 
                   aesthetics=c("color", "fill")
  ) +
  ggtitle("NAWRU in euro area North/South (population-weighted)") + 
  scale_x_continuous(
    breaks=x_axis_breaks, 
    expand = expand_scale(
      mult = c(0, 0), add = c(0, 0.5)
    )
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 1)
  ) +
  theme_icae() + 
  labs(x = "Source: AMECO (Spring 2020 forecast), own calculations") +
  theme(axis.title = element_text(color="black", size=plots_axis_title_size),
        plot.title = element_text(color="black", size=plots_title_size),
        axis.text = element_text(color="black", size=plots_axis_ticks_size))
NAWRU_plot

# 1.2 Public debt to GDP--------------------------------------------------------
start_year <- 2000
end_year <- 2020
x_axis_breaks <- c(2000, 2005, 2007, 2010, 2015, 2020)
publicdebt_plot <- macro_data %>%
  dplyr::select(dplyr::one_of(
    "year", "iso3c", "publicdebt", "population")
  ) %>%
  dplyr::rename(population=population) %>%
  dplyr::mutate(is.north=ifelse(iso3c %in% countries[["north"]],
                                "Northern euro area", ifelse(
                                  iso3c %in% countries[["south"]], 
                                  "Southern euro area", NA))
  ) %>%
  dplyr::mutate(is.north=ifelse(iso3c=="FRA", "France", is.north)
  ) %>%
  dplyr::filter(!is.na(is.north)
  ) %>%
  dplyr::mutate(is.north=as.factor(is.north)
  ) %>%
  dplyr::filter(year>=start_year & year <= end_year) %>%
  dplyr::group_by(year, is.north) %>%
  dplyr::mutate(population_group=sum(population)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(pop_rel_group=population / population_group) %>%
  dplyr::group_by(year, is.north) %>%
  dplyr::mutate(test_pop=sum(pop_rel_group)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(year, is.north) %>%
  dplyr::summarise(
    publicdebt_mean=weighted.mean(publicdebt, 
                             pop_rel_group),
    publicdebt_sd=sd(publicdebt*pop_rel_group)
  ) %>%
  dplyr::ungroup() %>%
  ggplot(., aes(x=year, y=publicdebt_mean, color=is.north)) +
  geom_point() + 
  geom_line() +
  ylab("in % of GDP") +
  scale_color_icae(palette = "mixed", 
                   aesthetics=c("color", "fill")
  ) +
  ggtitle("Public debt to GDP in euro area North/South (population-weighted)") + 
  scale_x_continuous(
    breaks=x_axis_breaks, 
    expand = expand_scale(
      mult = c(0, 0), add = c(0, 0.5)
    )
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 1)
  ) +
  theme_icae() + 
  theme(legend.text=element_text(size=7)) +
  labs(x = "Source: AMECO (Spring 2020 forecast), own calculations") +
  theme(axis.title = element_text(color="black", size=plots_axis_title_size),
        plot.title = element_text(color="black", size=plots_title_size),
        axis.text = element_text(color="black", size=plots_axis_ticks_size))
publicdebt_plot

# 1.2 GDP growth rate--------------------------------------------------------
start_year <- 2000
end_year <- 2020
x_axis_breaks <- c(2000, 2005, 2007, 2010, 2015, 2020)
GDPgrowth_plot <- macro_data %>%
  dplyr::select(dplyr::one_of(
    "year", "iso3c", "GDPgrowth", "population")
  ) %>%
  dplyr::rename(population=population) %>%
  dplyr::mutate(is.north=ifelse(iso3c %in% countries[["north"]],
                                "Northern euro area", ifelse(
                                  iso3c %in% countries[["south"]], 
                                  "Southern euro area", NA))
  ) %>%
  dplyr::mutate(is.north=ifelse(iso3c=="FRA", "France", is.north)
  ) %>%
  dplyr::filter(!is.na(is.north)
  ) %>%
  dplyr::mutate(is.north=as.factor(is.north)
  ) %>%
  dplyr::filter(year>=start_year & year <= end_year) %>%
  dplyr::group_by(year, is.north) %>%
  dplyr::mutate(population_group=sum(population)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(pop_rel_group=population / population_group) %>%
  dplyr::group_by(year, is.north) %>%
  dplyr::mutate(test_pop=sum(pop_rel_group)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(year, is.north) %>%
  dplyr::summarise(
    GDPgrowth_mean=weighted.mean(GDPgrowth, 
                                 pop_rel_group),
    GDPgrowth_sd=sd(GDPgrowth*pop_rel_group)
  ) %>%
  dplyr::ungroup() %>%
  ggplot(., aes(x=year, y=GDPgrowth_mean, color=is.north)) +
  geom_point() + 
  geom_line() +
  ylab("annual change in %") +
  scale_color_icae(palette = "mixed", 
                   aesthetics=c("color", "fill")
  ) +
  ggtitle("Real GDP growth rates in euro area North/South (population-weighted)") + 
  scale_x_continuous(
    breaks=x_axis_breaks, 
    expand = expand_scale(
      mult = c(0, 0), add = c(0, 0.5)
    )
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 1)
  ) +
  theme_icae() + 
  labs(x = "Source: AMECO (Spring 2020 forecast), own calculations") +
  theme(axis.title = element_text(color="black", size=plots_axis_title_size),
        plot.title = element_text(color="black", size=plots_title_size),
        axis.text = element_text(color="black", size=plots_axis_ticks_size))
GDPgrowth_plot 

# 1.2 Fiscal balance--------------------------------------------------------
x_axis_breaks <- c(2000, 2005, 2007, 2010, 2015, 2020)
fiscalbalance_plot <- macro_data %>%
  dplyr::select(dplyr::one_of(
    "year", "iso3c", "fiscalbalance", "population")
  ) %>%
  dplyr::rename(population=population) %>%
  dplyr::mutate(is.north=ifelse(iso3c %in% countries[["north"]],
                                "Northern euro area", ifelse(
                                  iso3c %in% countries[["south"]], 
                                  "Southern euro area", NA))
  ) %>%
  dplyr::mutate(is.north=ifelse(iso3c=="FRA", "France", is.north)
  ) %>%
  dplyr::filter(!is.na(is.north)
  ) %>%
  dplyr::mutate(is.north=as.factor(is.north)
  ) %>%
  dplyr::filter(year>=start_year & year <= end_year) %>%
  dplyr::group_by(year, is.north) %>%
  dplyr::mutate(population_group=sum(population)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(pop_rel_group=population / population_group) %>%
  dplyr::group_by(year, is.north) %>%
  dplyr::mutate(test_pop=sum(pop_rel_group)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(year, is.north) %>%
  dplyr::summarise(
    fiscalbalance_mean=weighted.mean(fiscalbalance, 
                                     pop_rel_group),
    fiscalbalance_sd=sd(fiscalbalance*pop_rel_group)
  ) %>%
  dplyr::ungroup() %>%
  ggplot(., aes(x=year, y=fiscalbalance_mean, color=is.north)) +
  geom_point() + 
  geom_line() +
  ylab("in % of GDP") +
  scale_color_icae(palette = "mixed", 
                   aesthetics=c("color", "fill")
  ) +
  ggtitle("Fiscal balances in euro area North/South (population-weighted)") + 
  theme(legend.text=element_text(size=7)) +
  scale_x_continuous(
    breaks=x_axis_breaks, 
    expand = expand_scale(
      mult = c(0, 0), add = c(0, 0.5)
    )
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 1)
  ) +
  theme_icae() + 
  theme(legend.text=element_text(size=7)) +
  labs(x = "Source: AMECO (Spring 2020 forecast), own calculations") +
  theme(axis.title = element_text(color="black", size=plots_axis_title_size),
        plot.title = element_text(color="black", size=plots_title_size),
        axis.text = element_text(color="black", size=plots_axis_ticks_size))
fiscalbalance_plot 

# 1.2 Primary fiscal balance--------------------------------------------------------
x_axis_breaks <- c(2000, 2005, 2007, 2010, 2015, 2020)
primaryfiscalbalance_plot <- macro_data %>%
  dplyr::select(dplyr::one_of(
    "year", "iso3c", "primaryfiscalbalance", "population")
  ) %>%
  dplyr::rename(population=population) %>%
  dplyr::mutate(is.north=ifelse(iso3c %in% countries[["north"]],
                                "Northern euro area", ifelse(
                                  iso3c %in% countries[["south"]], 
                                  "Southern euro area", NA))
  ) %>%
  dplyr::mutate(is.north=ifelse(iso3c=="FRA", "France", is.north)
  ) %>%
  dplyr::filter(!is.na(is.north)
  ) %>%
  dplyr::mutate(is.north=as.factor(is.north)
  ) %>%
  dplyr::filter(year>=start_year & year <= end_year) %>%
  dplyr::group_by(year, is.north) %>%
  dplyr::mutate(population_group=sum(population)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(pop_rel_group=population / population_group) %>%
  dplyr::group_by(year, is.north) %>%
  dplyr::mutate(test_pop=sum(pop_rel_group)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(year, is.north) %>%
  dplyr::summarise(
    primaryfiscalbalance_mean=weighted.mean(primaryfiscalbalance, 
                                     pop_rel_group),
    primaryfiscalbalance_sd=sd(primaryfiscalbalance*pop_rel_group)
  ) %>%
  dplyr::ungroup() %>%
  ggplot(., aes(x=year, y=primaryfiscalbalance_mean, color=is.north)) +
  geom_point() + 
  geom_line() +
  ylab("in % of GDP") +
  scale_color_icae(palette = "mixed", 
                   aesthetics=c("color", "fill")
  ) +
  ggtitle("Fiscal balances in euro area North/South (population-weighted)") + 
  scale_x_continuous(
    breaks=x_axis_breaks, 
    expand = expand_scale(
      mult = c(0, 0), add = c(0, 0.5)
    )
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 1)
  ) +
  theme_icae() + 
  labs(x = "Source: AMECO (Spring 2020 forecast), own calculations") +
  theme(axis.title = element_text(color="black", size=plots_axis_title_size),
        plot.title = element_text(color="black", size=plots_title_size),
        axis.text = element_text(color="black", size=plots_axis_ticks_size))
primaryfiscalbalance_plot 

# 1.2 Potential output--------------------------------------------------------
#
start_year <- 2000
end_year <- 2020

x_axis_breaks <- c(2000, 2005, 2007, 2010, 2015, 2020)
potentialoutput_plot <- macro_data %>%
  dplyr::select(dplyr::one_of(
    "year", "iso3c", "potentialoutput", "population")
  ) %>%
  dplyr::rename(population=population) %>%
  dplyr::mutate(is.north=ifelse(iso3c %in% countries[["north"]],
                                "Northern euro area", ifelse(
                                  iso3c %in% countries[["south"]], 
                                  "Southern euro area", NA))
  ) %>%
  dplyr::mutate(is.north=ifelse(iso3c=="FRA", "France", is.north)
  ) %>%
  dplyr::filter(!is.na(is.north)
  ) %>%
  dplyr::mutate(is.north=as.factor(is.north)
  ) %>%
  dplyr::filter(year>=start_year & year <= end_year) %>%
  dplyr::group_by(year, is.north) %>%
  dplyr::mutate(population_group=sum(population)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(pop_rel_group=population / population_group) %>%
  dplyr::group_by(year, is.north) %>%
  dplyr::mutate(test_pop=sum(pop_rel_group)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(year, is.north) %>%
  dplyr::summarise(
    potentialoutput_mean=weighted.mean(potentialoutput, 
                                     pop_rel_group),
    potentialoutput_sd=sd(potentialoutput*pop_rel_group)
  ) %>%
  dplyr::ungroup() %>%
  ggplot(., aes(x=year, y=potentialoutput_mean, color=is.north)) +
  geom_point() + 
  geom_line() +
  ylab("2007=100") +
  scale_color_icae(palette = "mixed", 
                   aesthetics=c("color", "fill")
  ) +
  ggtitle("Potential output (2007=100) in euro area North/South (population-weighted)") + 
  scale_x_continuous(
    breaks=x_axis_breaks, 
    expand = expand_scale(
      mult = c(0, 0), add = c(0, 0.5)
    )
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 1)
  ) +
  theme_icae() + 
  labs(x = "Source: AMECO (Spring 2020 forecast), own calculations") +
  theme(axis.title = element_text(color="black", size=plots_axis_title_size),
        plot.title = element_text(color="black", size=plots_title_size),
        axis.text = element_text(color="black", size=plots_axis_ticks_size))
potentialoutput_plot 

# 1.2 Output gap--------------------------------------------------------
#
start_year <- 2000
end_year <- 2020

x_axis_breaks <- c(2000, 2005, 2007, 2010, 2015, 2020)
outputgap_plot <- macro_data %>%
  dplyr::select(dplyr::one_of(
    "year", "iso3c", "outputgap", "population")
  ) %>%
  dplyr::rename(population=population) %>%
  dplyr::mutate(is.north=ifelse(iso3c %in% countries[["north"]],
                                "Northern euro area", ifelse(
                                  iso3c %in% countries[["south"]], 
                                  "Southern euro area", NA))
  ) %>%
  dplyr::mutate(is.north=ifelse(iso3c=="FRA", "France", is.north)
  ) %>%
  dplyr::filter(!is.na(is.north)
  ) %>%
  dplyr::mutate(is.north=as.factor(is.north)
  ) %>%
  dplyr::filter(year>=start_year & year <= end_year) %>%
  dplyr::group_by(year, is.north) %>%
  dplyr::mutate(population_group=sum(population)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(pop_rel_group=population / population_group) %>%
  dplyr::group_by(year, is.north) %>%
  dplyr::mutate(test_pop=sum(pop_rel_group)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(year, is.north) %>%
  dplyr::summarise(
    outputgap_mean=weighted.mean(outputgap, 
                                       pop_rel_group),
    outputgap_sd=sd(outputgap*pop_rel_group)
  ) %>%
  dplyr::ungroup() %>%
  ggplot(., aes(x=year, y=outputgap_mean, color=is.north)) +
  geom_point() + 
  geom_line() +
  ylab("in % of potential output") +
  scale_color_icae(palette = "mixed", 
                   aesthetics=c("color", "fill")
  ) +
  ggtitle("Output gap in euro area North/South (population-weighted)") + 
  scale_x_continuous(
    breaks=x_axis_breaks, 
    expand = expand_scale(
      mult = c(0, 0), add = c(0, 0.5)
    )
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 1)
  ) +
  theme_icae() + 
  labs(x = "Source: AMECO (Spring 2020 forecast), own calculations") +
  theme(axis.title = element_text(color="black", size=plots_axis_title_size),
        plot.title = element_text(color="black", size=plots_title_size),
        axis.text = element_text(color="black", size=plots_axis_ticks_size))
outputgap_plot

#
start_year <- 2017
end_year <- 2020

# 1.2 GDP change (2019=100--------------------------------------------------------
x_axis_breaks <- c(2018, 2019, 2020, 2021)
GDP2019_plot <- macro_data %>%
  dplyr::select(dplyr::one_of(
    "year", "iso3c", "GDP2019", "population")
  ) %>%
  dplyr::mutate(is.north=ifelse(iso3c %in% countries[["north"]],
                                "Northern euro area", ifelse(
                                  iso3c %in% countries[["south"]], 
                                  "Southern euro area", NA))
  ) %>%
  dplyr::mutate(is.north=ifelse(iso3c=="FRA", "France", is.north)
  ) %>%
  dplyr::filter(!is.na(is.north)
  ) %>%
  dplyr::mutate(is.north=as.factor(is.north)
  ) %>%
  dplyr::filter(year>=start_year & year <= end_year) %>%
  dplyr::group_by(year, is.north) %>%
  dplyr::mutate(population_group=sum(population)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(pop_rel_group=population / population_group) %>%
  dplyr::group_by(year, is.north) %>%
  dplyr::mutate(test_pop=sum(pop_rel_group)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(year, is.north) %>%
  dplyr::summarise(
    GDP2019_mean=weighted.mean(GDP2019, 
                                  pop_rel_group),
    GDP2019_sd=sd(GDP2019*pop_rel_group)
  ) %>%
  dplyr::ungroup() %>%
  ggplot(., aes(x=year, y=GDP2019_mean, color=is.north)) +
  geom_point() + 
  geom_line() +
  ylab("2019=100 (in %)") +
  scale_color_icae(palette = "mixed", 
                   aesthetics=c("color", "fill")
  ) +
  ggtitle("Real GDP (2019=100) in euro area North/South (population-weighted)") + 
  scale_x_continuous(
    breaks=x_axis_breaks, 
    expand = expand_scale(
      mult = c(0, 0), add = c(0, 0.5)
    )
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 1)
  ) +
  theme_icae() + 
  labs(x = "Source: AMECO (Spring 2020 forecast), own calculations") +
  theme(axis.title = element_text(color="black", size=plots_axis_title_size),
        plot.title = element_text(color="black", size=plots_title_size),
        axis.text = element_text(color="black", size=plots_axis_ticks_size))
GDP2019_plot 

# 1.2 Domestic demand (2019=100--------------------------------------------------------

plots_title_size <- 10
plots_axis_title_size <- 8
plots_axis_ticks_size <- 8
x_axis_breaks <- c(2017, 2018, 2019, 2020)
domesticdemand_plot <- macro_data %>%
  dplyr::select(dplyr::one_of(
    "year", "iso3c", "domesticdemand", "population")
  ) %>%
  dplyr::mutate(is.north=ifelse(iso3c %in% countries[["north"]],
                                "Northern euro area", ifelse(
                                  iso3c %in% countries[["south"]], 
                                  "Southern euro area", NA))
  ) %>%
  dplyr::mutate(is.north=ifelse(iso3c=="FRA", "France", is.north)
  ) %>%
  dplyr::filter(!is.na(is.north)
  ) %>%
  dplyr::mutate(is.north=as.factor(is.north)
  ) %>%
  dplyr::filter(year>=start_year & year <= end_year) %>%
  dplyr::group_by(year, is.north) %>%
  dplyr::mutate(population_group=sum(population)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(pop_rel_group=population / population_group) %>%
  dplyr::group_by(year, is.north) %>%
  dplyr::mutate(test_pop=sum(pop_rel_group)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(year, is.north) %>%
  dplyr::summarise(
    domesticdemand_mean=weighted.mean(domesticdemand, 
                               pop_rel_group),
    domesticdemand_sd=sd(domesticdemand*pop_rel_group)
  ) %>%
  dplyr::ungroup() %>%
  ggplot(., aes(x=year, y=domesticdemand_mean, color=is.north)) +
  geom_point() + 
  geom_line() +
  ylab("2019=100 (in %)") +
  scale_color_icae(palette = "mixed", 
                   aesthetics=c("color", "fill")
  ) +
  ggtitle("Domestic demand") + 
  scale_x_continuous(
    breaks=x_axis_breaks, 
    expand = expand_scale(
      mult = c(0, 0), add = c(0, 0.5)
    )
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 1)
  ) +
  theme_icae() + 
  coord_cartesian(ylim = c(80, 100)) +
  labs(x = "Source: AMECO (Spring 2020 forecast), own calculations") +
  theme(axis.title = element_text(color="black", size=plots_axis_title_size),
        plot.title = element_text(color="black", size=plots_title_size),
        axis.text = element_text(color="black", size=plots_axis_ticks_size))
domesticdemand_plot 

# 1.2 Exports of goods and services (2019=100--------------------------------------------------------
x_axis_breaks <- c(2017, 2018, 2019, 2020)
exportsgoodsandservices_plot <- macro_data %>%
  dplyr::select(dplyr::one_of(
    "year", "iso3c", "exportsgoodsandservices", "population")
  ) %>%
  dplyr::mutate(is.north=ifelse(iso3c %in% countries[["north"]],
                                "Northern euro area", ifelse(
                                  iso3c %in% countries[["south"]], 
                                  "Southern euro area", NA))
  ) %>%
  dplyr::mutate(is.north=ifelse(iso3c=="FRA", "France", is.north)
  ) %>%
  dplyr::filter(!is.na(is.north)
  ) %>%
  dplyr::mutate(is.north=as.factor(is.north)
  ) %>%
  dplyr::filter(year>=start_year & year <= end_year) %>%
  dplyr::group_by(year, is.north) %>%
  dplyr::mutate(population_group=sum(population)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(pop_rel_group=population / population_group) %>%
  dplyr::group_by(year, is.north) %>%
  dplyr::mutate(test_pop=sum(pop_rel_group)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(year, is.north) %>%
  dplyr::summarise(
    exportsgoodsandservices_mean=weighted.mean(exportsgoodsandservices, 
                                      pop_rel_group),
    exportsgoodsandservices_sd=sd(exportsgoodsandservices*pop_rel_group)
  ) %>%
  dplyr::ungroup() %>%
  ggplot(., aes(x=year, y=exportsgoodsandservices_mean, color=is.north)) +
  geom_point() + 
  geom_line() +
  ylab("2019=100 (in %)") +
  scale_color_icae(palette = "mixed", 
                   aesthetics=c("color", "fill")
  ) +
  ggtitle("Exports of goods and services") + 
  scale_x_continuous(
    breaks=x_axis_breaks, 
    expand = expand_scale(
      mult = c(0, 0), add = c(0, 0.5)
    )
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 1)
  ) +
  theme_icae() + 
  coord_cartesian(ylim = c(80, 100)) +
  labs(x = "Source: AMECO (Spring 2020 forecast), own calculations") +
  theme(axis.title = element_text(color="black", size=plots_axis_title_size),
        plot.title = element_text(color="black", size=plots_title_size),
        axis.text = element_text(color="black", size=plots_axis_ticks_size))
exportsgoodsandservices_plot 

# 1.2 Exports of goods (2019=100--------------------------------------------------------
plots_title_size <- 10
plots_axis_title_size <- 9
plots_axis_ticks_size <- 9

x_axis_breaks <- c(2017, 2018, 2019, 2020)
exportsgoods_plot <- macro_data %>%
  dplyr::select(dplyr::one_of(
    "year", "iso3c", "exportsgoods", "population")
  ) %>%
  dplyr::mutate(is.north=ifelse(iso3c %in% countries[["north"]],
                                "Northern euro area", ifelse(
                                  iso3c %in% countries[["south"]], 
                                  "Southern euro area", NA))
  ) %>%
  dplyr::mutate(is.north=ifelse(iso3c=="FRA", "France", is.north)
  ) %>%
  dplyr::filter(!is.na(is.north)
  ) %>%
  dplyr::mutate(is.north=as.factor(is.north)
  ) %>%
  dplyr::filter(year>=start_year & year <= end_year) %>%
  dplyr::group_by(year, is.north) %>%
  dplyr::mutate(population_group=sum(population)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(pop_rel_group=population / population_group) %>%
  dplyr::group_by(year, is.north) %>%
  dplyr::mutate(test_pop=sum(pop_rel_group)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(year, is.north) %>%
  dplyr::summarise(
    exportsgoods_mean=weighted.mean(exportsgoods, 
                                      pop_rel_group),
    exportsgoods_sd=sd(exportsgoods*pop_rel_group)
  ) %>%
  dplyr::ungroup() %>%
  ggplot(., aes(x=year, y=exportsgoods_mean, color=is.north)) +
  geom_point() + 
  geom_line() +
  ylab("2019=100 (in %)") +
  coord_cartesian(ylim = c(70, 100)) +
  scale_color_icae(palette = "mixed", 
                   aesthetics=c("color", "fill")
  ) +
  ggtitle("Exports of goods") + 
  scale_x_continuous(
    breaks=x_axis_breaks, 
    expand = expand_scale(
      mult = c(0, 0), add = c(0, 0.5)
    )
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 1)
  ) +
  theme_icae() + 
  labs(x = "Source: AMECO (Spring 2020 forecast), own calculations") +
  theme(axis.title = element_text(color="black", size=plots_axis_title_size),
        plot.title = element_text(color="black", size=plots_title_size),
        axis.text = element_text(color="black", size=plots_axis_ticks_size))
exportsgoods_plot 

# 1.2 Exports of services (2019=100--------------------------------------------------------
x_axis_breaks <- c(2017, 2018, 2019, 2020)
exportsservices_plot <- macro_data %>%
  dplyr::select(dplyr::one_of(
    "year", "iso3c", "exportsservices", "population")
  ) %>%
  dplyr::mutate(is.north=ifelse(iso3c %in% countries[["north"]],
                                "Northern euro area", ifelse(
                                  iso3c %in% countries[["south"]], 
                                  "Southern euro area", NA))
  ) %>%
  dplyr::mutate(is.north=ifelse(iso3c=="FRA", "France", is.north)
  ) %>%
  dplyr::filter(!is.na(is.north)
  ) %>%
  dplyr::mutate(is.north=as.factor(is.north)
  ) %>%
  dplyr::filter(year>=start_year & year <= end_year) %>%
  dplyr::group_by(year, is.north) %>%
  dplyr::mutate(population_group=sum(population)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(pop_rel_group=population / population_group) %>%
  dplyr::group_by(year, is.north) %>%
  dplyr::mutate(test_pop=sum(pop_rel_group)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(year, is.north) %>%
  dplyr::summarise(
    exportsservices_mean=weighted.mean(exportsservices, 
                                      pop_rel_group),
    exportsservices_sd=sd(exportsservices*pop_rel_group)
  ) %>%
  dplyr::ungroup() %>%
  ggplot(., aes(x=year, y=exportsservices_mean, color=is.north)) +
  geom_point() + 
  geom_line() +
  ylab("2019=100 (in %)") +
  scale_color_icae(palette = "mixed", 
                   aesthetics=c("color", "fill")
  ) +
  ggtitle("Exports of services") + 
  scale_x_continuous(
    breaks=x_axis_breaks, 
    expand = expand_scale(
      mult = c(0, 0), add = c(0, 0.5)
    )
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 1)
  ) +
  theme_icae() + 
  coord_cartesian(ylim = c(70, 100)) +
  labs(x = "Source: AMECO (Spring 2020 forecast), own calculations") +
  theme(axis.title = element_text(color="black", size=plots_axis_title_size),
        plot.title = element_text(color="black", size=plots_title_size),
        axis.text = element_text(color="black", size=plots_axis_ticks_size))
exportsservices_plot 

ggsave(plot = GDP2019_plot, 
       filename = here("output/GDP2019.pdf"),
       width = 10, height = 4)

ggsave(plot = unemp_plot, 
       filename = here("output/unemp.pdf"),
       width = 10, height = 4)

ggsave(plot = GDPgrowth_plot, 
       filename = here("output/GDPgrowth.pdf"),
       width = 10, height = 4)

ggsave(plot = fiscalbalance_plot, 
       filename = here("output/fiscalbalance.pdf"),
       width = 10, height = 4)

ggsave(plot = publicdebt_plot, 
       filename = here("output/publicdebt.pdf"),
       width = 10, height = 4)

ggsave(plot = domesticdemand_plot, 
       filename = here("output/domesticdemand.pdf"),
       width = 10, height = 4)

ggsave(plot = exportsgoodsandservices_plot, 
       filename = here("output/exportsgoodsandservices.pdf"),
       width = 10, height = 4)

ggsave(plot = exportsgoods_plot, 
       filename = here("output/exportsgoods.pdf"),
       width = 10, height = 4)

ggsave(plot = exportsservices_plot, 
       filename = here("output/exportsservices.pdf"),
       width = 10, height = 4)

library(gridExtra)
grid1 <- grid.arrange(domesticdemand_plot, exportsgoodsandservices_plot, ncol=2, nrow=1)

ggsave(plot = grid1, 
       filename = here("output/grid-domesticdemand-and-exports.pdf"),
       width = 10, height = 4)

grid2 <- grid.arrange(dev_mean_plot, unemp_plot, fiscalbalance_plot, publicdebt_plot, ncol=2, nrow=2)

ggsave(plot = grid2, 
       filename = here("output/grid-income-unem-fiscal.pdf"),
       width = 10, height = 4)

grid3 <- grid.arrange(exportsgoods_plot, exportsservices_plot, ncol=2, nrow=1)

ggsave(plot = grid3, 
       filename = here("output/grid-exports-goods-vs-services.pdf"),
       width = 10, height = 4)
