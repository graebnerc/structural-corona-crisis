rm(list=ls())
library(data.table)
library(tidyverse)
library(here)
library(scales)
if (!require("icaeDesign")){
  devtools::install_github("graebnerc/icaeDesign")
}
tiff_res <- 600 # resolution for tiff figures
plots_title_size <- 12
plots_axis_title_size <- 11
plots_axis_ticks_size <- 10
plot_title <- "Discretionary 2020 fiscal measures adopted in response to coronavirus\n (in % of 2019 GDP)"

plot_fiscalresponse <- fread(here("data/Fiscal_responses.csv")) %>%
  tidyr::pivot_longer(cols = -V1, names_to = "country", values_to = "value") %>%
  dplyr::mutate(
    V1=factor(V1, levels=c(
      "Other liquidity provisions / guarantees", "Deferral",
      "Immediate fiscal impulse"))) %>%
  ggplot(
    data=., 
    aes(x=reorder(country, -value), y=value, 
        fill=V1, color=V1)
  ) +
  geom_bar(stat="identity") +
  scale_y_continuous(
    name = "% of GDP", 
    expand = expansion(mult = c(0, 0), add = c(0, 3)), 
    labels = scales::percent_format(scale = 1)) +
  labs(title = plot_title, caption = "Data source: Anderson et al. (2020, last update on June 4th 2020); own calculations.") +
  scale_fill_icae(palette = "mixed", aesthetics=c("fill", "color")) +
  theme_icae() + 
  theme(axis.title.y = element_text(color="black", size=plots_axis_title_size),
        axis.title.x = element_blank(),
        plot.title = element_text(color="black", size=plots_title_size),
        axis.text = element_text(color="black", size=plots_axis_ticks_size),
        legend.title = element_blank(), legend.position = "bottom")
plot_fiscalresponse

ggsave(plot = plot_fiscalresponse, 
       filename = here("output/fig_3_fiscalresponse.pdf"),
       width = 7, height = 5)

ggsave(plot = plot_fiscalresponse, 
       filename = here("output/fig_3_fiscalresponse.tiff"),
       dpi = tiff_res, compression="lzw", type="cairo",
       width = 7, height = 5)
