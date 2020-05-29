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

classification <- "north_south"
start_year <- 2017
end_year <- 2020

fig_height <- 3
fig_width <- 11
plots_title_size <- 12
plots_axis_title_size <- 11
plots_axis_ticks_size <- 10
strip_text_size <- 10
x_axis_breaks <- c(2017, 2018, 2019, 2020)



if (classification=="north_south"){
  source(here("R/setup_country_classification_North_South.R"))
  macro_data <- fread(here("data/macro_data.csv")) %>%
    dplyr::mutate(is.north=ifelse(iso3c %in% countries[["north"]],
                                  "Northern Eurozone countries", ifelse(
                                    iso3c %in% countries[["south"]], 
                                    "Southern Eurozone countries", NA))
    ) %>%
    dplyr::mutate(is.north=ifelse(iso3c=="FRA", "France", is.north)
    ) 
} else if (classification=="jee"){
  source(here("R/setup_country_classification_JEE.R"))
  
  macro_data <- fread(here("data/macro_data.csv")) %>%
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
    )  
} else if (classification=="finance"){
  source(here("R/setup_country_classification_finance.R"))
  
  macro_data <- fread(here("data/macro_data.csv")) %>%
    dplyr::mutate(is.north=ifelse(iso3c %in% countries[["north"]],
                                  "Northern euro area", ifelse(
                                    iso3c %in% countries[["south"]], 
                                    "Southern euro area", ifelse(
                                      iso3c %in% countries[["finance"]], 
                                      "Financial hubs", NA)))
    ) %>%
    dplyr::mutate(is.north=ifelse(iso3c=="FRA", "France", is.north)
    ) 
}


macro_data <- macro_data %>%
  dplyr::filter(!is.na(is.north), year>=start_year, year<=end_year
  ) %>%
  dplyr::mutate(is.north=as.factor(is.north)
  ) %>%
  dplyr::select(dplyr::one_of(
    "year", "iso3c", "domesticdemand", "exportsgoodsandservices", 
    "population", "is.north", "exportsgoods", "exportsservices")
  ) %>%
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
    exportsgoods_mean=weighted.mean(exportsgoods, 
                                    pop_rel_group),
    exportsgoodsandservices_mean=weighted.mean(exportsgoodsandservices, 
                                               pop_rel_group),
    exportsservices_mean=weighted.mean(exportsservices, 
                                       pop_rel_group)
  ) %>%
  dplyr::ungroup() 


# Figure 2: Domestic demand and exports----------------------------------------

exports_domestic_plot_data <- macro_data %>%
  select(one_of("is.north", "year", "domesticdemand_mean", 
                "exportsgoodsandservices_mean")) %>%
  rename(Exports=exportsgoodsandservices_mean,
         `Domestic demand`=domesticdemand_mean) %>%
  pivot_longer(cols = c("Exports", "Domestic demand"), 
               names_to = "variable", values_to = "exports_mean")

exports_domestic_plot <- ggplot(
  exports_domestic_plot_data, aes(x=year, y=exports_mean, color=is.north)
  ) +
  geom_point() + 
  geom_line() +
  ylab("2019=100 (in %)") +
  scale_color_icae(palette = "mixed", 
                   aesthetics=c("color", "fill")
  ) +
  ggtitle("Domestic demand and exports") + 
  scale_x_continuous(
    breaks=x_axis_breaks, 
    expand = expand_scale(
      mult = c(0, 0), add = c(0.5, 0.5)
    )
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 1)
  ) +
  theme_icae() + 
  coord_cartesian(ylim = c(80, 100)) +
    facet_wrap(~variable) +
  labs(caption = "Source: AMECO (Spring 2020 forecast), own calculations") +
  theme(axis.title = element_text(color="black", size=plots_axis_title_size),
        plot.title = element_text(color="black", size=plots_title_size),
        axis.text = element_text(color="black", size=plots_axis_ticks_size), 
        axis.title.x = element_blank(), 
        strip.text = element_text(color="black", size=strip_text_size))
exports_domestic_plot

ggsave(plot = exports_domestic_plot, 
       filename = paste0(here("output/fig_4_exp-domestic_"), classification, ".pdf"),
       width = fig_width, height = fig_height)

# Figure 3: Exports of goods and services--------------------------------------

goods_services_plot_data <- macro_data %>%
  select(one_of("is.north", "year", "exportsservices_mean", 
                "exportsgoods_mean")) %>%
  rename(`Exports of goods`=exportsgoods_mean,
         `Exports of services`=exportsservices_mean) %>%
  pivot_longer(cols = c("Exports of goods", "Exports of services"), 
               names_to = "variable", values_to = "exports_mean")

goods_services_plot <- ggplot(
  goods_services_plot_data, aes(x=year, y=exports_mean, color=is.north)
) +
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
      mult = c(0, 0), add = c(0.5, 0.5)
    )
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 1)
  ) +
  theme_icae() + 
  coord_cartesian(ylim = c(70, 100)) +
  facet_wrap(~variable) +
  labs(caption = "Source: AMECO (Spring 2020 forecast), own calculations") +
  theme(axis.title = element_text(color="black", size=plots_axis_title_size),
        plot.title = element_text(color="black", size=plots_title_size),
        axis.text = element_text(color="black", size=plots_axis_ticks_size), 
        axis.title.x = element_blank(),
        strip.text = element_text(color="black", size=strip_text_size))
goods_services_plot

ggsave(plot = goods_services_plot, 
       filename = paste0(here("output/fig_5_exp-goods-services_"), 
                         classification, ".pdf"),
       width = fig_width, height = fig_height)
