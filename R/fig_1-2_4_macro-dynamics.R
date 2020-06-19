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
# "north_south" for the main paper, "jee" for the appendix
start_year <- 2000
end_year <- 2020

plots_title_size <- 11
plots_axis_title_size <- 8
plots_axis_ticks_size <- 7
legend_font_size <- 11

x_axis_breaks <- c(2000, 2005, 2007, 2010, 2015, 2020)

macro_data_file <- here("data/macro_data.csv")

if (classification=="north_south"){
  source(here("R/setup_country_classification_North_South.R"))
  
  title_base <- " in Eurozone North/South (population-weighted)"
  fig_1_title <- "Macroeconomic polarization in the EMU"
  
  macro_data <- fread(macro_data_file) %>%
    dplyr::mutate(is.north=ifelse(iso3c %in% countries[["north"]],
                                  "Northern Eurozone countries", ifelse(
                                    iso3c %in% countries[["south"]], 
                                    "Southern Eurozone countries", NA))
    ) %>%
    dplyr::mutate(is.north=ifelse(iso3c=="FRA", "France", is.north)
    ) %>%
    dplyr::filter(!is.na(is.north)
    ) %>%
    dplyr::mutate(is.north=as.factor(is.north)
    )
  
  macro_data_short <- subset(macro_data, 
                             year %in% as.character(1999:2016))
  
  eci_income_data <- data.table::copy(macro_data_short) %>%
    select(iso3c, year, ECI, gdp_real_pc_ppp) %>%
    mutate(cluster=ifelse(iso3c %in% countries[["north"]], 
                          "Northern Eurozone countries", ifelse(
                            iso3c %in% countries[["south"]],
                            "Southern Eurozone countries", ifelse(
                              iso3c %in% countries[["france"]], 
                              "France", NA)))
    ) %>%
    dplyr::filter(!is.na(cluster), !is.na(gdp_real_pc_ppp)) %>%
    group_by(iso3c, cluster) %>%
    summarise_all(mean, na.rm=T)%>%
    ungroup() 
  
} else if (classification=="jee"){
  source(here("R/setup_country_classification_JEE.R"))
  
  title_base <- " in Europe (population-weighted)"
  fig_1_title <- "Macroeconomic polarization in Europe"
  
  macro_data <- fread(macro_data_file) %>%
    dplyr::mutate(is.north=ifelse(iso3c %in% countries[["Core"]],
                                  "Core countries", ifelse(
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
  
  macro_data_short <- subset(macro_data, year %in% as.character(1999:2016))
  
  eci_income_data <- data.table::copy(macro_data_short) %>%
    select(iso3c, year, ECI, gdp_real_pc_ppp) %>%
    dplyr::mutate(cluster=ifelse(iso3c %in% countries[["Core"]],
                                  "Core Eurozone", ifelse(
                                    iso3c %in% countries[["Catchup"]], 
                                    "Catchup countries", ifelse(
                                      iso3c %in% countries[["Finance"]], 
                                      "Finance hubs", ifelse(
                                        iso3c %in% countries[["Periphery"]], 
                                        "Southern periphery countries", NA
                                      )
                                    )))
    ) %>%
    dplyr::filter(!is.na(cluster), !is.na(gdp_real_pc_ppp), iso3c!="LUX") %>%
    group_by(iso3c, cluster) %>%
    summarise_all(mean, na.rm=T)%>%
    ungroup() 
  
} 

# Figure 1---------------------------------------------------------------------

# 1.1 GDP growth--------------------------------------------------------

GDPgrowth_plot <- macro_data %>%
  dplyr::select(dplyr::one_of(
    "year", "iso3c", "GDPgrowth", "population", "is.north")
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
  ggtitle(paste0("GDP growth", title_base)) + 
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
  theme(legend.text=element_text(size=legend_font_size)) +
  theme(axis.title = element_text(color="black", size=plots_axis_title_size),
        axis.title.x = element_blank(),
        plot.title = element_text(color="black", size=plots_title_size),
        axis.text = element_text(color="black", size=plots_axis_ticks_size))
GDPgrowth_plot 

# 1.2 Unemployment rate--------------------------------------------------------

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
  ggtitle(paste0("Unemployment rates" , title_base) ) + 
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
  theme(legend.text=element_text(size=legend_font_size)) +
  theme(axis.title = element_text(color="black", size=plots_axis_title_size),
        axis.title.x = element_blank(),
        plot.title = element_text(color="black", size=plots_title_size),
        axis.text = element_text(color="black", size=plots_axis_ticks_size))
unemp_plot 

# 1.3 Fiscal balance--------------------------------------------------------

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
  ggtitle(paste0("Fiscal balances", title_base)) + 
  theme(legend.text=element_text(size=legend_font_size)) +
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
  theme(legend.text=element_text(size=legend_font_size)) +
  theme(axis.title = element_text(color="black", size=plots_axis_title_size),
        axis.title.x = element_blank(),
        plot.title = element_text(color="black", size=plots_title_size),
        axis.text = element_text(color="black", size=plots_axis_ticks_size))
fiscalbalance_plot 

# 1.4 Public debt to GDP--------------------------------------------------------

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
  ggtitle(paste0("Public debt to GDP", title_base)) + 
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
  theme(legend.text=element_text(size=legend_font_size)) +
  theme(axis.title = element_text(color="black", size=plots_axis_title_size),
        axis.title.x = element_blank(),
        plot.title = element_text(color="black", size=plots_title_size),
        axis.text = element_text(color="black", size=plots_axis_ticks_size))
publicdebt_plot

# Full figure 1----------------------------------------------------------------
grid2 <- ggarrange(GDPgrowth_plot, unemp_plot, 
                   fiscalbalance_plot, publicdebt_plot, 
                   ncol=2, nrow=2, common.legend = T,
                   legend = "bottom", 
                   labels = paste0(LETTERS[1:4], ")"))

grid2 <- annotate_figure(
  grid2,
  top = text_grob(fig_1_title, 
                  color = "black",  size = 14),
  bottom = text_grob("Source: AMECO (Spring 2020 forecast), own calculations.",
                     color = "black", hjust = 1, x = 1, 
                     face = "italic", size = 8))

ggsave(plot = grid2, 
       filename = paste0(here("output/fig_1_macro-dynamics_"), 
                         classification, ".pdf"),
       width = 11, height = 5)

# Figure 2: Tax revenues and government spending-------------------------------
# 2.1 Tax revenues-------------------------------------------------------------
start_year <- 2017
end_year <- 2020
fig_height <- 3
fig_width <- 11
plots_title_size <- 9
plots_axis_title_size <- 9
plots_axis_ticks_size <- 8
strip_text_size <- 11
x_axis_breaks <- c(2017, 2018, 2019, 2020)


taxrevenues_plot <- macro_data %>%
  dplyr::select(dplyr::one_of(
    "year", "iso3c", "taxrevenues", "population", "is.north")
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
    taxrevenues_mean=weighted.mean(taxrevenues, 
                                   pop_rel_group),
    taxrevenues_sd=sd(taxrevenues*pop_rel_group)
  ) %>%
  dplyr::ungroup() %>%
  ggplot(., aes(x=year, y=taxrevenues_mean, color=is.north)) +
  geom_point() + 
  geom_line() +
  ylab("2019=100") +
  scale_color_icae(palette = "mixed", 
                   aesthetics=c("color", "fill")
  ) +
  ggtitle(paste0("Tax revenues", title_base)) + 
  theme(legend.text=element_text(size=legend_font_size)) +
  scale_x_continuous(
    breaks=x_axis_breaks, 
    expand = expand_scale(
      mult = c(0, 0), add = c(0.5, 0.5)
    )
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 1),
    expand = expand_scale(mult = c(0, 0), add = c(2, 0))
  ) +
  theme_icae() + 
  coord_cartesian(ylim = c(90, 115)) +
  theme(legend.text=element_text(size=legend_font_size)) +
  theme(axis.title = element_text(color="black", size=plots_axis_title_size),
        axis.title.x = element_blank(),
        plot.title = element_text(color="black", size=plots_title_size),
        axis.text = element_text(color="black", size=plots_axis_ticks_size), 
        strip.text.x = element_text(color="black", size=strip_text_size))
taxrevenues_plot 

# 2.2 Government spending--------------------------------------------------------

governmentspending_plot <- macro_data %>%
  dplyr::select(dplyr::one_of(
    "year", "iso3c", "governmentspending", "population", "is.north")
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
    governmentspending_mean=weighted.mean(governmentspending, 
                                          pop_rel_group),
    governmentspending_sd=sd(governmentspending*pop_rel_group)
  ) %>%
  dplyr::ungroup() %>%
  ggplot(., aes(x=year, y=governmentspending_mean, color=is.north)) +
  geom_point() + 
  geom_line() +
  ylab("2019=100") +
  scale_color_icae(palette = "mixed", 
                   aesthetics=c("color", "fill")
  ) +
  ggtitle(paste0("Government spending", title_base)) + 
  theme(legend.text=element_text(size=legend_font_size)) +
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
  coord_cartesian(ylim = c(90, 115)) +
  theme(legend.text=element_text(size=legend_font_size)) +
  theme(axis.title = element_text(color="black", size=plots_axis_title_size),
        axis.title.x = element_blank(),
        plot.title = element_text(color="black", size=plots_title_size),
        axis.text = element_text(color="black", size=plots_axis_ticks_size),
        strip.text.x = element_text(color="black", size=strip_text_size))
governmentspending_plot 

# Full figure 2----------------------------------------------------------------

fig_2 <- ggarrange(taxrevenues_plot, governmentspending_plot,
                   ncol=2, nrow=1, common.legend = T,
                   legend = "bottom", 
                   labels = paste0(LETTERS[1:2], ")"))

fig_height <- 3
fig_width <- 11
ggsave(plot = fig_2, 
       filename = paste0(here("output/fig_2_revenues_spending_"), 
                         classification, ".pdf"),
       width = fig_width, height = fig_height)

# Figure 4: GDP deviation and ECI correlation----------------------------------

# 4.1. Deviation from mean income----------------------------------------------
start_year <- 2000
end_year <- 2020
x_axis_breaks <- c(2000, 2005, 2007, 2010, 2015, 2020)
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
  ggtitle("Deviation of GDP p.c. from EZ-average (population-weighted)") + 
  ylab("Deviation of GDP p.c. (PPP)\n from Eurozone average") +
  scale_y_continuous(labels = scales::number_format(suffix = "k")) +
  theme_icae() +
  scale_x_continuous(
    breaks=x_axis_breaks, 
    expand = expand_scale(
      mult = c(0, 0), add = c(0, 0.5)
    )
  ) +
  scale_color_icae(palette = "mixed") + 
  theme(legend.text=element_text(size=legend_font_size-1)) +
  theme(axis.title.y = element_text(color="black", size=plots_axis_title_size), 
        axis.title.x = element_blank(),
        plot.title = element_text(color="black", size=plots_title_size),
        axis.text = element_text(color="black", size=plots_axis_ticks_size))
dev_mean_plot

# 4.2. ECI and GDP per capita levels-------------------------------------------
l_reg <- lm(ECI ~ gdp_real_pc_ppp, data = eci_income_data)
r_sq <- round(summary(l_reg)$r.squared, 2)
reg_eq <- as.character(as.expression(
  paste0("$GDP = \\beta_0 + \\beta_1 ECI,$ \\hspace{1cm} $R^2=", r_sq, "$")
))
reg_eq

if (classification=="jee"){
  y_notation <- 10500
  x_notation <- 1.8
} else if (classification=="north_south"){
  y_notation <- 20000
  x_notation <- 1.8
}

eci_income <- ggplot(eci_income_data, 
                     aes(x=ECI, 
                         y=gdp_real_pc_ppp, "k", 
                         color=cluster)) +
  geom_point(
    aes(x=ECI, 
        y=gdp_real_pc_ppp, 
        color=cluster), 
    show.legend = T) +
  geom_smooth(
    aes(x=ECI, 
        y=gdp_real_pc_ppp), 
    method = "lm", 
    color="#330080") +
  geom_label_repel(
    mapping = aes(
      x=ECI, 
      y=gdp_real_pc_ppp, 
      label=iso3c, 
      color=cluster), 
    show.legend = F
  ) +
  theme_icae() + 
  scale_color_icae(palette = "mixed") +
  xlab("Economic complexity index (ECI)") + 
  ylab("Average GDP p.c. (PPP)") +
  scale_x_continuous(expand = c(0, 0.02)) +
  scale_y_continuous(labels = scales::number_format(scale = 1/1000, suffix = "k")) +
  ggtitle("Complexity and per-capita income (average 1999-2016)") +
  annotate("text", x=x_notation, y=y_notation, 
           label=TeX(
             paste0("$GDP = \\beta_0 + \\beta_1 ECI,$  ", " $R^2=", r_sq, "$"), 
             output = "character"), parse=TRUE) +
  theme(legend.text=element_text(size=legend_font_size-1)) +
  theme(axis.title = element_text(color="black", size=plots_axis_title_size),
        plot.title = element_text(color="black", size=plots_title_size),
        axis.text = element_text(color="black", size=plots_axis_ticks_size))
eci_income

# Full figure------------------------------------------------------------------

fig_4 <- ggarrange(dev_mean_plot, eci_income,
                   ncol=1, nrow=2, common.legend = T,
                   legend = "bottom", 
                   labels = paste0(LETTERS[1:2], ")"))

fig_4 <- annotate_figure(
  fig_4,
  bottom = text_grob(
    "Source: AMECO (Spring 2020), The Atlas of Economic Complexity; authors' calculations.",
    color = "black", hjust = 1, x = 1, face = "italic", size = 8))

ggsave(plot = fig_4, 
       filename = paste0(here("output/fig_4_divergence-eci_"), 
                         classification, ".pdf"),
       width = 6, height = 5)
