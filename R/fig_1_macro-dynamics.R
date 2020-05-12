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

classification <- "finance"
download_bond_data <- F
download_eurostat_debt <- F

if (classification=="north_south"){
  source(here("R/setup_country_classification_North_South.R"))
  
  macro_data <- fread(here("data/macro_data_fig1-2_4_NEW.csv")) %>%
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
    )
} else if (classification=="jee"){
  source(here("R/setup_country_classification_JEE.R"))
  
  macro_data <- fread(here("data/macro_data_fig1-2_4_NEW.csv")) %>%
    dplyr::mutate(is.north=ifelse(iso3c %in% countries[["Core"]],
                                  "Core euro area", ifelse(
                                    iso3c %in% countries[["Catchup"]], 
                                    "Catchup countries", ifelse(
                                      iso3c %in% countries[["Finance"]], 
                                      "Finance hubs", ifelse(
                                        iso3c %in% countries[["Periphery"]], 
                                        "Southern periphery countries", NA
                                      )
                                    )))
    )  %>%
    dplyr::filter(!is.na(is.north)
    ) %>%
    dplyr::mutate(is.north=as.factor(is.north)
    )
} else if (classification=="finance"){
  source(here("R/setup_country_classification_finance.R"))
  
  macro_data <- fread(here("data/macro_data_fig1-2_4_NEW.csv")) %>%
    dplyr::mutate(is.north=ifelse(iso3c %in% countries[["north"]],
                                  "Northern euro area", ifelse(
                                    iso3c %in% countries[["south"]], 
                                    "Southern euro area", ifelse(
                                      iso3c %in% countries[["finance"]], 
                                      "Financial hubs", NA)))
    ) %>%
    dplyr::mutate(is.north=ifelse(iso3c=="FRA", "France", is.north)
    ) %>%
    dplyr::filter(!is.na(is.north)
    ) %>%
    dplyr::mutate(is.north=as.factor(is.north)
    )
}

start_year <- 2000
end_year <- 2020

plots_title_size <- 8
plots_axis_title_size <- 7
plots_axis_ticks_size <- 7

x_axis_breaks <- c(2000, 2005, 2007, 2010, 2015, 2020)

# 1.1. Deviation from mean income----------------------------------------------
dev_mean_plot <- macro_data %>%
  dplyr::filter(!is.na(population), !is.na(gdp_real_pc_ppp), 
                year>=start_year, 
                year <= end_year) %>%
  group_by(year, is.north) %>%
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
         aes(x=year, y=dev.mean.GDP_pc_PPP, color=is.north)) +
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
    "year", "iso3c", "unemp_rate", "population", "is.north")
  ) %>%
  dplyr::rename(population=population) %>%
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
  ylab("% of active population") +
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
  theme(legend.text=element_text(size=7)) +
  theme(axis.title = element_text(color="black", size=plots_axis_title_size),
        plot.title = element_text(color="black", size=plots_title_size),
        axis.text = element_text(color="black", size=plots_axis_ticks_size))
unemp_plot 


# 1.3 Fiscal balance--------------------------------------------------------
x_axis_breaks <- c(2000, 2005, 2007, 2010, 2015, 2020)
fiscalbalance_plot <- macro_data %>%
  dplyr::select(dplyr::one_of(
    "year", "iso3c", "fiscalbalance", "population", "is.north")
  ) %>%
  dplyr::rename(population=population) %>%
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
  ylab("% of GDP") +
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
  theme(axis.title = element_text(color="black", size=plots_axis_title_size),
        plot.title = element_text(color="black", size=plots_title_size),
        axis.text = element_text(color="black", size=plots_axis_ticks_size))
fiscalbalance_plot 

# 1.4 Public debt to GDP--------------------------------------------------------
start_year <- 2000
end_year <- 2020
x_axis_breaks <- c(2000, 2005, 2007, 2010, 2015, 2020)
publicdebt_plot <- macro_data %>%
  dplyr::select(dplyr::one_of(
    "year", "iso3c", "publicdebt", "population", "is.north")
  ) %>%
  dplyr::rename(population=population) %>%
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
  ylab("% of GDP") +
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
  theme(axis.title = element_text(color="black", size=plots_axis_title_size),
        plot.title = element_text(color="black", size=plots_title_size),
        axis.text = element_text(color="black", size=plots_axis_ticks_size))
publicdebt_plot

# Full figure------------------------------------------------------------------
grid2 <- ggarrange(dev_mean_plot, unemp_plot, 
                   fiscalbalance_plot, publicdebt_plot, 
                   ncol=2, nrow=2, common.legend = T,
                   legend = "bottom", 
                   labels = paste0(LETTERS[1:4], ")"))

grid2 <- annotate_figure(
  grid2,
  top = text_grob("Macroeconomic polarization in the EMU", 
                  color = "black",  size = 14),
  bottom = text_grob("Source: AMECO (Spring 2020 forecast), own calculations.",
                     color = "black", hjust = 1, x = 1, 
                     face = "italic", size = 8))

ggsave(plot = grid2, 
       filename = paste0(here("output/fig_1_macro-dynamics_"), 
                         classification, ".pdf"),
       width = 11, height = 5)
