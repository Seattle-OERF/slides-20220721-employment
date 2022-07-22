esd_qcew_files <- 
  tibble(file_path = dir_ls(path_qcew_sed, regex = "qcew-annual-averages"),
         file_name = path_file(file_path),
         yr = file_name %>% str_sub(22, 25) %>% as.numeric(),
         preliminary = str_detect(file_name, "preliminary"),
         sheets = map(file_path, xlsx_sheet_names))

esd_qcew_files %>% 
  select(file_name, sheets) %>% 
  unnest_longer(sheets, values_to = "sheet_org") %>% 
  count(sheet_org) %>% 
  print(n = 100)

esd_qcew_raw <- 
  esd_qcew_files %>% 
  unnest_longer(sheets, values_to = "sheet_org") %>% 
  filter(sheet_org %in% c("King County", "Snohomish County")) %>% 
  mutate(data_raw = map2(file_path, sheet_org, ~xlsx_cells(.x, .y)))

tidy_esd_qcew_data <- function(.data, .yr) {
  
    if (.yr %in% 2002:2011) {
      data_tmp <- 
        .data %>% 
        filter(row %in% 6:165) %>% 
        behead(direction = "left-up", name = "naics_2") %>% 
        behead(direction = "left", name = "naics_3") %>% 
        behead(direction = "left", name = "naics_label") %>% 
        behead(direction = "up", name = "measure_pt1") %>%
        behead(direction = "up", name = "measure_pt2") %>% 
        replace_na(list(measure_pt1 = "")) %>% 
        mutate(measure = str_c(measure_pt1, measure_pt2, sep = " ") %>% str_trim())
    } else if (.yr >= 2012) {
      data_tmp <- 
        .data %>% 
        filter(row %in% 5:141) %>% 
        behead(direction = "left-up", name = "naics_2") %>% 
        behead(direction = "left", name = "naics_3") %>% 
        behead(direction = "left", name = "naics_label") %>% 
        behead(direction = "up", name = "measure") 
    }
  
  data_tmp %>%
    transmute(row, col, 
              naics_2 = as.character(naics_2),
              naics_3 = as.character(naics_3),
              naics_label,
              measure,
              lvl = numeric, 
              suppressed = (data_type == "character" & character == "*")) %>% 
    filter(!is.na(naics_label) & !is.na(measure))
}

esd_qcew_tmp <- 
  esd_qcew_raw %>% 
  filter(!preliminary | yr == 2021) %>% 
  transmute(dataset = sheet_org, yr, data_raw) %>% 
  arrange(dataset, yr) %>% 
  mutate(data_cln = map2(data_raw, yr, ~tidy_esd_qcew_data(.x, .y)))

esd_qcew_tmp %>% 
  select(dataset, data_cln) %>% 
  unnest(data_cln) %>% 
  count(measure) %>% 
  print(n = 500)

esd_qcew_cln <- 
  esd_qcew_tmp %>% 
  select(dataset, yr, data_cln) %>% 
  rename(area_label = dataset) %>% 
  unnest(data_cln) %>% 
  mutate(measure = measure %>% 
                     str_to_title() %>% 
                     str_replace("\\r\\n", " ") %>% 
                     str_replace("  ", " ") %>%
                     str_remove("Paid") %>% 
                     str_replace("Emp\\.", "Employment") %>% 
                     str_replace("Avg\\.", "Average") %>%
                     str_replace("Average Wage", "Average Annual Wage") %>%
                     str_replace("Average Employment", "Average Annual Employment") %>%
                     str_replace("^Firms", "Average Firms") %>%
                     str_replace("Total \\d{4} Wages", "Total Wages") %>% 
                     str_trim(),
         measure = if_else(measure %in% month.abb, str_c(measure, " Employment"), measure),
         measure = factor(measure, levels = c("Jan Employment", "Feb Employment",
                                              "Mar Employment", "Apr Employment",
                                              "May Employment", "Jun Employment",
                                              "Jul Employment", "Aug Employment",
                                              "Sep Employment", "Oct Employment",
                                              "Nov Employment", "Dec Employment",
                                              "Average Annual Employment",
                                              "Total Wages",
                                              "Average Annual Wage",
                                              "Average Weekly Wage",
                                              "Q1 Wages", "Q2 Wages",
                                              "Q3 Wages", "Q4 Wages",
                                              "Average Firms",
                                              "Q1 Firms", "Q2 Firms",
                                              "Q3 Firms", "Q4 Firms")),
         naics_label = naics_label %>% str_to_title() %>% str_replace("Health Care", "Healthcare"),
         naics_2 = case_when(naics_label == "Not Elsewhere Classified" ~ "NEC",
                             str_detect(naics_label, "Government")  ~ "GOV",
                             naics_2 %in% c("NAICS  Code", "Total") ~ "TOTAL",
                             TRUE                                   ~ naics_2),
         naics_3 = if_else(is.na(naics_3) | naics_3 == "", naics_2, naics_3),
         naics_3 = if_else(naics_2 == "TOTAL", "TOTAL", naics_3))

esd_qcew_naics_2_cln <- 
  esd_qcew_cln %>% 
  filter(naics_3 == naics_2,
         naics_label != "Other Industries",
         (naics_2 != "GOV" | naics_label == "Government")) %>% 
  arrange(area_label, measure, naics_2, yr) %>% 
  transmute(area_label, 
            measure, 
            naics_2 = as_factor(naics_2),
            naics_label = as_factor(naics_label), 
            yr, lvl, suppressed)

esd_qcew_naics_2_cln %>% 
  count(naics_2, naics_label) %>% 
  print(n = 50)

esd_qcew_naics_2_cln %>% 
  count(measure) %>% 
  print(n = 50)

esd_qcew_naics_2_cln %>% 
  # filter(measure %in% c("Average Annual Employment", 
  #                       "Jan Employment", "Feb Employment", "Mar Employment")) %>%
  filter(measure == "Average Annual Employment") %>% 
  ggplot(aes(x = yr, y = lvl, col = area_label)) +
    geom_line() +
    facet_wrap(~naics_label, scales = "free")
ggplotly()

naics_crosswalk_qcew <- 
  read_csv(str_c(path_data_root, "naics_crosswalk_qcew.csv")) %>% 
  mutate(across(c(naics_custom, naics_custom_label), as_factor))

ks_esd_qcew_naics_2_tmp <- 
  left_join(esd_qcew_naics_2_cln %>% 
              filter(measure %in% c("Average Annual Employment", "Mar Employment")) %>% 
              rename(naics_label_esd = naics_label),
            naics_crosswalk_qcew,
            by = "naics_2") 

ks_esd_qcew_naics_2_plt <- 
  bind_rows(ks_esd_qcew_naics_2_tmp,
            ks_esd_qcew_naics_2_tmp %>% 
              filter(area_label %in% c("King County", "Snohomish County")) %>% 
              mutate(area_label = "Seattle MD")) %>% 
  group_by(area_label, naics_custom, naics_custom_label, measure, yr) %>% 
  summarise(lvl = sum(lvl)) %>% 
  ungroup() %>% 
  mutate(area_label = factor(area_label, levels = c("Seattle", "Seattle MD", "King County", "Snohomish County"))) %>% 
  arrange(area_label, naics_custom, measure, yr) %>% 
  group_by(area_label, naics_custom, measure) %>% 
  mutate(gyy = lvl / lag(lvl) - 1,
         dyy = lvl - lag(lvl),
         hover_label = str_c(area_label, "<br>",
                             yr, ": ",
                             label_number(accuracy = 0.1, scale = 1e-3, suffix = "k")(lvl), ", ",
                             label_percent(accuracy = 0.1)(gyy))) %>% 
  ungroup() %>% 
  arrange(area_label, naics_custom, measure, yr)

ks_esd_qcew_naics_2_plt %>% 
  write_csv(here("data", "wrangled", "ks_esd_qcew_naics_2_plt.csv"))
  
naics_custom_to_plot <- c("TTL", "23", "31-33", "42, 44-45", "51", "52-53", "54-56", "61-62", "71-72", "GOV")

ks_esd_qcew_naics_2_plt %>% 
  filter(naics_custom %in% naics_custom_to_plot,
         measure == "Average Annual Employment") %>% 
  ggplot(aes(x = yr, y = lvl, col = area_label)) +
    geom_line() +
    labs(x = NULL, y = NULL, col = NULL) +
    facet_wrap(~naics_custom_label, scales = "free")
ggplotly()  
