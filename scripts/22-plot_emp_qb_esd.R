
#### load ESD employment data ####

date_from <- ymd("2020-10-01")
date_to   <- ymd("2022-06-01")

# read ESD historical payroll employment data by county from xlsx files
esd_qb_combined_raw <- 
  crossing(vintage = seq.Date(date_from, date_to, by = "months") %>% label_date(format = "%Y%m")(),
           seas = c("NSA", "SA")) %>%
  mutate(data_raw = map2(vintage, seas, ~read_csv(here("data", "wrangled", str_c("ks_waqb_m_", ..2, "_", ..1, ".csv")),
                                                  col_types = cols(.default = col_double(),
                                                                   yearm = col_character())) %>%
                                           set_names((names(.) %>% 
                                                        str_remove("_NSA"))))) 

esd_qb_combined_plt <- 
  esd_qb_combined_raw %>% 
  unnest(data_raw) %>% 
  pivot_longer(-c(vintage, seas, yearm), names_to = "var_code") %>%
  mutate(seas_label = recode(seas,
                             "SA"  = "Seasonally Adjusted",
                             "NSA" = "Not Seasonally Adjusted"),
                             yearm = as.yearmon(yearm),
         vintage_yearm = as.yearmon(vintage, format = "%Y%m"),
         release = (vintage_yearm + 1/12) %>% as.character() %>% as_factor(),
         benchmark = vintage_yearm %>% as.yearqtr() %>% as.yearmon() - 4/12,
         benchmark_label = benchmark %>% as.character() %>% as_factor(),
         month_in_benchmark_group = round(12 * (vintage_yearm - (benchmark + 3/12))) %>% as.integer() %>% as.factor(),
         hover_label = str_c(var_code, "<br>",
                             yearm, "<br>",
                             label_number(accuracy = 0.1, big.mark = ",")(value)))

last_esd_date <- esd_qb_combined_plt %>% pull(yearm) %>% max() %>% label_date(format = "%Y%m")()

var_code_selected <- c("KS_N", "KS_NCON", "KS_NMFG", "KS_NTRD", "KS_NTWU", 
                       "KS_NINF", "KS_NFIN", "KS_NPBS", "KS_NLHS", "KS_NGOV")

esd_qb_combined_plt %>% 
  filter(var_code %in% var_code_selected) %>% 
  write_csv(here("data", "wrangled", str_c("ks_esd_qb_combined_plt_", last_esd_date ,".csv")))

#### plot ESD employment data ####

esd_qb_combined_plt %>%
  filter(var_code == "KS_N",
         yearm >= "Jan 2019") %>%
  ggplot(aes(x = yearm, y = value, col = benchmark_label, alpha = month_in_benchmark_group)) +
    geom_line(size = 1) +
    scale_alpha_manual(values = c(0.4, 0.7, 1.0)) +
    labs(x = NULL, y = NULL,
         title = "Total Nonfarm Employment by ESD Release") +
    facet_wrap(~seas_label)
ggplotly()


n_colors <- 8

pal_esd_benchmarks_gghue <- gg_color_hue(n_colors)
pal_esd_benchmarks_pmw <- pnw_palette("Bay", n_colors) %>% rev()
pal_esd_benchmarks_base <- c("darkgreen", "blue", "cyan3", "orange", "red", "purple", "brown", "yellow")

pal_esd <- pal_esd_benchmarks_base

plot_esd_emp <- function(.var_code, .label) {
  esd_qb_selected_plt <- 
    esd_qb_combined_plt %>%
    filter(var_code %in% .var_code,
           yearm >= "Jan 2019") 
  
  area_label <- 
    case_when(str_sub(.var_code, 1, 2) == "KS" ~ "Seattle MD",
              str_sub(.var_code, 1, 2) == "WA" ~ "Washington State")

  g_esd <- 
    esd_qb_selected_plt %>%
    ggplot() +
      aes(x = yearm, y = value, col = release, alpha = release, text = hover_label, group = release) +
      geom_line(size = 1) +
      scale_color_manual(values = rep(pal_esd, each = 3)) +
      scale_alpha_manual(values = rep(c(0.4, 0.7, 1.0), times = n_colors)) +
      scale_y_continuous(labels = label_number(big.mark = ",")) +
      labs(x = NULL, y = NULL, col = NULL, alpha = NULL) +
      labs(x = NULL, y = NULL,
           title = str_c(area_label, " Employment by ESD Release - ", .label)) +
      guides(color = guide_legend(nrow = 3, byrow = FALSE))
  
  g_esd + facet_wrap(~seas_label, ncol = 2)
}
  
plot_esd_emp("WA_N", "Total Nonfarm")

plot_esd_emp("KS_N", "Total Nonfarm")
plot_esd_emp("KS_NGDS", "Goods")
plot_esd_emp("KS_NSRV", "Services")

plot_esd_emp("KS_NCON", "Construction")
plot_esd_emp("KS_NMFG", "Manufacturing")
plot_esd_emp("KS_NAER", "Manufacturing - Aerospace")
plot_esd_emp("KS_NINF", "Information")
plot_esd_emp("KS_NFIN", "Financial Activities")
plot_esd_emp("KS_NPBS", "Professional and business services")
plot_esd_emp("KS_NLHS", "Leisure and hospitality")
plot_esd_emp("KS_NOSRV", "Other Services")
plot_esd_emp("KS_NGOV", "Government")

ggplotly(tooltip = "text")
