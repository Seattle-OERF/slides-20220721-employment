
# PAYEMS                  All Employees, Total Nonfarm in U.S., thousands, SA
# PAYNSA                  All Employees, Total Nonfarm in U.S., thousands, NSA

# SEAT653NA               All Employees: Total Nonfarm in Seattle-Tacoma-Bellevue, WA (MSA), thousands,SA
# SEAT653NAN              All Employees: Total Nonfarm in Seattle-Tacoma-Bellevue, WA (MSA), thousands, NSA

# CE16OV                  U.S. Employment Level, thousands, SSA
# LNU02000000             U.S. Employment Level, thousands, NSA
# LAUMT534266000000005    Employed Persons in Seattle-Tacoma-Bellevue, WA (MSA), NSA

bls_emp_raw <- 
  c("PAYEMS", "PAYNSA", "CE16OV", "LNU02000000",
    "SEAT653NA", "SEAT653NAN",
    "LAUMT534266000000005") %>% 
  tq_get(get = "economic.data", from = "1990-01-01", to  = today())

bls_emp_cln <- 
  bls_emp_raw %>% 
  transmute(yearm = as.yearmon(date),
            var_code = recode(symbol,
                              "PAYEMS" = "US_N_CES_SA", 
                              "PAYNSA" = "US_N_CES_NSA", 
                              "CE16OV" = "US_N_CPS_SA",
                              "LNU02000000" = "US_N_CPS_NSA",
                              "SEAT653NA" = "KSP_N_CES_SA",
                              "SEAT653NAN" = "KSP_N_CES_NSA",
                              "LAUMT534266000000005" = "KSP_N_LAUS_NSA"),
            lvl = if_else(symbol == "LAUMT534266000000005", price / 1000, price)) %>% 
  separate(var_code, into = c("area_code", "var_stub", "data_survey", "seas"), remove = FALSE)

bls_emp_plt <- 
  bls_emp_cln %>% 
  group_by(var_code) %>% 
  mutate(dmm = lvl - lag(lvl)) %>% 
  ungroup() %>% 
  pivot_longer(c(lvl, dmm), names_to = "measure") %>% 
  mutate(area_label = recode(area_code,
                             "US" = "United States",
                             "KSP" = "Seattle MSA"),
         data_survey_label = recode(data_survey,
                                    "CES" = "Establishment survey (thousands of jobs)",
                                    "CPS" = "Households survey (thousands of persons)"),
         data_survey_type = recode(data_survey,
                                   "CES" = "CES",
                                   "CPS" = "CPS/LAUS",
                                   "LAUS" = "CPS/LAUS"),
         data_survey_type_label = recode(data_survey_type,
                                         "CES" = "Establishment survey (thousands of jobs)",
                                         "CPS/LAUS" = "Households survey (thousands of persons)"),
         hover_label = str_c("<b>", yearm, "</b>, ", area_label, ", <b>", data_survey, " ", seas, "</b><br>",
                             label_number(accuracy = 0.1)(value)))

bls_vintage <- bls_emp_plt %>% pull(yearm) %>% max() %>% label_date(format = "%Y%m")()

bls_emp_plt %>% 
  write_csv(here("data", "wrangled", str_c("bls_emp_plt_", bls_vintage, ".csv")))

g_bls_emp_lvl <- 
  bls_emp_plt %>% 
  filter(seas == "NSA",
         measure == "lvl") %>% 
  ggplot(aes(x = yearm, y = value, col = data_survey_type_label, # alpha = seas, linetype = seas, 
             text = hover_label, group = var_code)) +
    geom_line(size = 1) +
    scale_y_continuous(# limits = c(-500, 1250),
      labels = label_number(big.mark = ",")) +
    scale_alpha_manual(values = c(1, 0.5)) +
    labs(x = NULL, y = NULL, col = NULL, alpha = NULL, linetype = NULL,
         title = "Employment, Not seasonally adjusted",
         caption = "Source: U.S. Bureau of Labor Statistics")  +
  facet_wrap(~ area_label, ncol = 1, scales = "free_y")
g_bls_emp_lvl
ggplotly(g_bls_emp_lvl, tooltip = "text")

g_bls_emp_lvl_seattle_msa <- 
  bls_emp_plt %>% 
  filter(area_code == "KSP",
         seas == "NSA",
         measure == "lvl") %>% 
  ggplot(aes(x = yearm, y = value, col = data_survey_type_label, # alpha = seas, linetype = seas, 
             text = hover_label, group = var_code)) +
    geom_line(size = 1) +
    scale_y_continuous(# limits = c(-500, 1250),
                       labels = label_number(big.mark = ",")) +
    scale_alpha_manual(values = c(1, 0.5)) +
    labs(x = NULL, y = NULL, col = NULL, alpha = NULL, linetype = NULL,
         title = "Seattle MSA Employment",
         caption = "Source: U.S. Bureau of Labor Statistics") 
g_bls_emp_seattle_msa
ggplotly(g_bls_emp_lvl_seattle_msa, tooltip = "text")

g_bls_emp_chg <- 
  bls_emp_plt %>% 
  filter(yearm >= "Jan 2021",
         measure == "dmm") %>% 
  ggplot(aes(x = yearm, y = value, col = data_survey_type_label, alpha = seas, linetype = seas, 
             text = hover_label, group = var_code)) +
    geom_hline(yintercept = 0, col = "gray50") +
    geom_line(size = 1) +
    scale_y_continuous(# limits = c(-500, 1250),
                       labels = label_number(big.mark = ",")) +
    scale_alpha_manual(values = c(1, 0.5)) +
    labs(x = NULL, y = NULL, col = NULL, alpha = NULL, linetype = NULL,
         title = "Change in employment from previous month",
         caption = "Source: U.S. Bureau of Labor Statistics") +
    facet_wrap(~ area_label, ncol = 1, scales = "free_y")
g_bls_emp_chg
ggplotly(g_bls_emp_chg, tooltip = "text")

