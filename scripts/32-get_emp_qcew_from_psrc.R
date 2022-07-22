psrc_qcew_files <- 
  tibble(file_path = dir_ls(path_qcew_psrc, regex = "cityemp.*\\.xlsx$"),
         file_name = path_file(file_path),
         yr = parse_number(file_name),
         sheets = map(file_path, xlsx_sheet_names))

psrc_qcew_files %>% 
  select(file_name, sheets) %>% 
  unnest_longer(sheets, values_to = "sheet_org") %>% 
  count(sheet_org)

psrc_qcew_raw <- 
  psrc_qcew_files %>% 
  unnest_longer(sheets, values_to = "sheet_org") %>% 
  mutate(dataset = recode(sheet_org,
                          "City (2-digit NAICS)" = "NAICS 2-digit",
                          "City (2-Digit NAICS)" = "NAICS 2-digit",
                          "City(2-digit_NAICS)"  = "NAICS 2-digit",
                          "City(2-digNAICS)"     = "NAICS 2-digit",
                          "City (Major Sector)"  = "Major Sector",
                          "City(Major_Sector)"   = "Major Sector")) %>% 
  filter(dataset %in% c("NAICS 2-digit", "Major Sector")) %>% 
  mutate(data_raw = map2(file_path, sheet_org, ~xlsx_cells(.x, .y)))

tidy_psrc_qcew_data <- function(.data) {
  .data %>% 
    filter(row %in% c(3:93, 96:100)) %>% 
    behead(direction = "left", name = "county") %>% 
    behead(direction = "left", name = "city") %>% 
    behead(direction = "up", name = "naics_sector") %>% 
    mutate(naics_sector = recode(naics_sector,
                                 "33†" = "33",
                                 "Ed" = "Education",
                                 "Gv" = "Government")) %>% 
    transmute(row, col, 
              county = str_trim(county), 
              city = str_trim(city), 
              naics_sector, 
              lvl = numeric, 
              suppressed = (data_type == "character" & character == "*"))
}

psrc_qcew_tmp <- 
  psrc_qcew_raw %>% 
  select(dataset, yr, data_raw) %>% 
  arrange(dataset, yr) %>% 
  mutate(data_cln = map(data_raw, tidy_psrc_qcew_data))

psrc_qcew_tmp %>% 
  filter(dataset == "NAICS 2-digit") %>% 
  filter(yr == 2013) %>% 
  pluck("data_cln", 1) %>% 
  filter(city == "Seattle")

ks_psrc_qcew_tmp <- 
  psrc_qcew_tmp %>% 
  filter(dataset == "NAICS 2-digit") %>% 
  select(yr, data_cln) %>% 
  unnest(data_cln) %>% 
  filter(county %in% c("King", "Snohomish"),
         city %in% c("Bellevue", "Seattle", "Total"))

ks_psrc_qcew_tmp %>% 
  count(naics_sector) %>% 
  print(n = 50)

# check which NAICS are suppressed
ks_psrc_qcew_tmp %>% 
  filter(suppressed) %>% 
  arrange(naics_sector, county, city, yr) %>% 
  print(n = 50)

# NAICS crosswalk
naics_crosswalk_psrc <- 
  read_csv(str_c(path_data_root, "naics_crosswalk_psrc.csv")) %>% 
  mutate(across(c(naics_custom, naics_custom_label), as_factor))

ks_psrc_qcew_naics_2_tmp <- 
  ks_psrc_qcew_tmp %>% 
  mutate(area = if_else(city == "Total", county, city)) %>% 
  transmute(area_label = if_else(city == "Total", str_c(county, " County"), city),
            naics_2 = naics_sector,
            measure = "Mar Employment",
            yr, lvl) %>% 
  arrange(area_label, naics_2, measure, yr) %>% 
  left_join(naics_crosswalk_psrc, by = "naics_2") 

ks_psrc_qcew_naics_2_cln <- 
  bind_rows(ks_psrc_qcew_naics_2_tmp,
            ks_psrc_qcew_naics_2_tmp %>% 
              filter(area_label %in% c("King County", "Snohomish County")) %>% 
              mutate(area_label = "Seattle MD")) %>% 
  group_by(area_label, naics_custom, naics_custom_label, measure, yr) %>% 
  summarise(lvl = sum(lvl)) %>% 
  ungroup() %>% 
  mutate(area_label = factor(area_label, levels = c("Bellevue", "Seattle", "King County", "Snohomish County", "Seattle MD"))) 

# calculate implied employment for Trade in Seattle in 2021, since 42,44-45 is the only suppressed sector that year
lvl_implied_trade_emp_2021 <- 
  ks_psrc_qcew_naics_2_cln %>% 
  filter(area_label == "Seattle" & yr == 2021) %>% 
  select(naics_custom, lvl) %>% 
  summarise(lvl_trade = lvl[naics_custom == "TTL"] - sum(lvl[naics_custom != "TTL"], na.rm = TRUE)) %>% 
  pull(lvl_trade)
  
ks_psrc_qcew_naics_2_plt <- 
  ks_psrc_qcew_naics_2_cln %>% 
  mutate(lvl = if_else(area_label == "Seattle" & yr == 2021 & naics_custom == "42, 44-45",
                       lvl_implied_trade_emp_2021, lvl)) %>%
  arrange(area_label, naics_custom, measure, yr) %>% 
  group_by(area_label, naics_custom,measure) %>% 
  mutate(gyy = lvl / lag(lvl) - 1,
         dyy = lvl - lag(lvl),
         hover_label = str_c(area_label, "<br>",
                             yr, ": ",
                             label_number(accuracy = 0.1, scale = 1e-3, suffix = "k")(lvl), ", ",
                             label_percent(accuracy = 0.1)(gyy))) %>% 
  ungroup() %>% 
  arrange(area_label, naics_custom, measure, yr) 

ks_psrc_qcew_naics_2_plt %>% 
  write_csv(here("data", "wrangled", "ks_psrc_qcew_naics_2_plt.csv"))

# Contributions to total employment change between March 2020 and March 2021
ks_psrc_qcew_naics_2_plt %>%
  filter(naics_custom %in% naics_custom_to_plot,
         area_label != "Snohomish County",
         yr %in% 2020:2021) %>% 
  group_by(area_label, yr) %>% 
  mutate(shr = lvl / lvl[naics_custom == "TTL"]) %>% 
  group_by(area_label, naics_custom) %>% 
  mutate(ctr = gyy * shr[yr == 2020]) %>% 
  ungroup() %>% 
  filter(yr == 2021) %>%
  mutate(naics_custom_label = fct_rev(naics_custom_label)) %>% 
  ggplot(aes(x = naics_custom_label, y = ctr, fill = area_label)) +
    geom_col(position = position_dodge2(width = 0.1)) +
    geom_hline(yintercept = 0, col = "gray50") +
    scale_y_continuous(labels = label_percent(), sec.axis = dup_axis()) +
    scale_fill_manual(values = pnw_palette("Bay", 6)[c(4, 6, 1, 3)]) +
    labs(x = NULL, y = NULL, fill = NULL,
         title = "Contributions to total employment change between March 2020 and March 2021",
         caption = "Source: Puget Sound Regional Council, covered employment estimates") +
    coord_flip()
ggplotly()

# time series plots
naics_custom_to_plot <- c("TTL", "23", "31-33", "42, 44-45", "51", "52-53", "54-56", "61-62", "71-72", "GOV")

g_facet_by_naics <- 
  ggplot() +
    aes(x = yr, y  = gyy, col = area_label, text = hover_label, group = area_label) +
    geom_hline(yintercept = 0, col = "gray50") +
    geom_line(size = 1) +
    scale_y_continuous(labels = label_percent()) +
    labs(x = NULL, y = NULL, col = NULL,
         title = "Employment by NAICS, PSRC covered employment estimates") +
         # caption = "Source: Puget Sound Regional Council, covered employment estimates") +
    facet_wrap(~naics_custom_label, scales = "free_x") 

g_facet_by_naics %+% 
  {ks_psrc_qcew_naics_2_plt %>%
    filter(naics_custom %in% naics_custom_to_plot) %>% 
    filter(area_label != "Snohomish County") %>% 
    # filter(yr >= 2005)}
    filter(yr >= 2011)}
ggplotly()

g_facet_by_naics %+% 
  {ks_psrc_qcew_naics_2_plt %>%
      filter(naics_custom %in% c("TTL", "51", "54-56", "71-72")) %>% 
      filter(area_label != "Snohomish County") %>% 
      filter(yr >= 2005)}
ggplotly(tooltip = "text")

g_facet_by_naics %+% 
  {ks_psrc_qcew_naics_2_plt %>%
      filter(naics_custom %in% c("TTL", "42, 44-45", "51", "54-56")) %>% 
      filter(area_label != "Snohomish County") %>% 
      filter(yr >= 2005)}
ggplotly(tooltip = "text")
