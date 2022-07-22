library(here)
source(here("scripts", "00-initialize.R"))
source(here("scripts", "01-helpers.R"))

path_data_root <- "C:/Users/DurasJ/OneDrive - City of Seattle/CBO/Data/"
path_esd_emp <- str_c(path_data_root, "esd.wa.gov/labormarketinfo/employment-estimates/")
path_esd_lf  <- str_c(path_data_root, "esd.wa.gov/labormarketinfo/laus/")

path_ihs_processed <- "//ad.seattle.gov/dept/ERF/Data/OERF/Forecast/Economic/IHS/processed/"
path_cbo_ksm       <- "//ad.seattle.gov/dept/ERF/Data/OERF/Forecast/Economic/CBO/Models/KSM/"
path_oerf_ksm      <- "//ad.seattle.gov/dept/ERF/Data/OERF/Forecast/Economic/OERF/Models/KSM/"

path_qcew_sed <- "//COSFS01/CBO/Data/DOF/Econ/DATA/Labor Market/ESD/Employment/QCEW ESD web/"
path_qcew_psrc <- "//COSFS01/CBO/Data/DOF/Econ/DATA/Labor Market/PSRC/Employment/PSRC web/"

source(here("scripts", "02-load_ihs_data.R"))
source(here("scripts", "03-load_oerf_forecast.R"))

source(here("scripts", "11-prepare_emp_esd.R"))
source(here("scripts", "12-prepare_emp_bls.R"))
source(here("scripts", "15-get_cpi_bls_data.R"))
source(here("scripts", "19-prepare_salestax_dor.R"))

source(here("scripts", "20-prepare_inc_actual_vs_forecast.R"))
source(here("scripts", "21-prepare_emp_actual_vs_forecast.R"))
source(here("scripts", "25-prepare_cpi_actual_vs_forecast.R"))

source(here("scripts", "30-table_actual_vs_forecast.R"))
