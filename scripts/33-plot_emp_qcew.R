
#### combined dataset ESD and PSRC ####

ks_esd_qcew_naics_2_plt <- 
  read_csv(here("data", "wrangled", "ks_esd_qcew_naics_2_plt.csv")) %>% 
  mutate(across(c(area_label, naics_custom, naics_custom_label), as_factor))

ks_psrc_qcew_naics_2_plt <- 
  read_csv(here("data", "wrangled", "ks_psrc_qcew_naics_2_plt.csv")) %>% 
  mutate(across(c(area_label, naics_custom, naics_custom_label), as_factor))

ks_qcew_naics_2_tmp <- 
  bind_rows(ESD = ks_esd_qcew_naics_2_plt,
            PSRC = ks_psrc_qcew_naics_2_plt,
            .id = "data_source") 

ks_qcew_naics_2_plt <- 
  bind_rows(ks_qcew_naics_2_tmp,
            ks_qcew_naics_2_tmp %>% 
              filter(naics_custom %in% c("42, 44-45", "51", "54-56")) %>% 
              group_by(data_source, area_label, measure, yr) %>% 
              summarise(lvl = sum(lvl)) %>% 
              arrange(data_source, area_label, measure, yr) %>% 
              group_by(data_source, area_label, measure) %>%
              mutate(naics_custom = "42, 44-45, 51, 54-56",
                     naics_custom_label = "NAICS with main PET contributors",
                     gyy = lvl / lag(lvl) - 1,
                     dyy = lvl - lag(lvl),
                     hover_label = str_c(area_label, "<br>",
                                         yr, ": ",
                                         label_number(accuracy = 0.1, scale = 1e-3, suffix = "k")(lvl), ", ",
                                         label_percent(accuracy = 0.1)(gyy))) %>% 
              ungroup()) %>% 
  arrange(area_label, naics_custom) %>% 
  mutate(naics_custom_label = recode(naics_custom_label,
                                     "Trade" = "Retail and Wholesale Trade") %>% 
                              as_factor())

ks_qcew_naics_2_plt %>% 
  write_csv(here("data", "wrangled", "ks_qcew_naics_2_plt.csv")) 

# time series plots
naics_custom_to_plot <- c("TTL", "23", "31-33", "42, 44-45", "51", "52-53", "54-56", "61-62", "71-72", "GOV")

g_combined_facet_by_naics <- 
  ggplot() +
    aes(x = yr, y  = gyy, col = area_label, fill = area_label, linetype = data_source, text = hover_label, group = str_c(data_source, area_label)) +
    geom_hline(yintercept = 0, col = "gray50") +
    scale_y_continuous(labels = label_percent()) +
    scale_linetype_manual(values = c(2, 1)) +
    labs(x = NULL, y = NULL, col = NULL, fill = NULL, linetype = NULL,
         title = "Employment growth for selected NAICS sectors, March 2011 to March 2021") +
    facet_wrap(~naics_custom_label, scales = "free_x")

g_bar_combined_facet_by_naics <- 
  g_ombined_facet_by_naics +
  geom_col(position = position_dodge2(width = 0.1))

g_bar_combined_facet_by_naics %+% 
  {ks_qcew_naics_2_plt %>%
      filter(measure == "Mar Employment") %>% 
      filter(naics_custom %in% c("TTL", "42, 44-45, 51, 54-56")) %>% 
      filter(area_label != "Snohomish County") %>% 
      filter(yr >= 2005)}
ggplotly(tooltip = "text")

g_line_combined_facet_by_naics <- 
  g_combined_facet_by_naics +
  geom_line(size = 1)

g_line_combined_facet_by_naics %+% 
  {ks_qcew_naics_2_plt %>%
      filter(measure == "Mar Employment") %>% 
      filter(naics_custom %in% naics_custom_to_plot) %>% 
      filter(area_label != "Snohomish County") %>% 
      filter(yr >= 2005)}
ggplotly()

g_line_ombined_facet_by_naics %+% 
  {ks_qcew_naics_2_plt %>%
      filter(measure == "Mar Employment") %>% 
      filter(naics_custom %in% c("TTL", "42, 44-45, 51, 54-56", "42, 44-45", "51", "54-56", "71-72")) %>% 
      filter(area_label != "Snohomish County") %>% 
      filter(yr >= 2005)}
ggplotly(tooltip = "text")

g_line_ombined_facet_by_naics %+% 
  {ks_qcew_naics_2_plt %>%
      filter(measure == "Mar Employment") %>% 
      # filter(naics_custom %in% c("42, 44-45, 51, 54-56", "42, 44-45", "51")) %>%
      filter(naics_custom %in% c("42, 44-45", "51")) %>% 
      filter(area_label != "Snohomish County") %>% 
      filter(yr >= 2011)} +
  scale_x_continuous(breaks = 2011:2021) 
  # scale_y_continuous(limits = c(-0.05, 0.2), labels = label_percent()) 
ggplotly(tooltip = "text")

g_line_ombined_facet_by_naics %+% 
  {ks_qcew_naics_2_plt %>%
      filter(measure == "Mar Employment") %>% 
      filter(naics_custom %in% c("TTL", "42, 44-45, 51, 54-56")) %>% 
      filter(area_label != "Snohomish County") %>% 
      filter(yr >= 2011)}
ggplotly(tooltip = "text")
