#*****************************************************************
#*                  CareyLabVT Blacksburg, VA USA                *
#*                                                               *
#* TITLE:   Practice_script_NeonStore.R                         *
#* AUTHOR:  Ryan McClure                                         *
#* DATE:    24Sep2020                                            *
#* PROJECT: CIBR                                                 *
#* PURPOSE: Test the new CRAN neonstore package                  *      
#*****************************************************************

# Load packages
# -------------------------------------------------------------------

if (!require('pacman')) install.packages('pacman'); library('pacman')
pacman::p_load(tidyverse, neonstore)

# -------------------------------------------------------------------

# Refer to the PDF for more information on neonstore 
# -------------------------------------------------------------------
#https://cran.r-project.org/web/packages/neonstore/neonstore.pdf
# -------------------------------------------------------------------

# Cite NEON
# -------------------------------------------------------------------
neon_citation()                     # Cites NEON as a whole

neon_citation("DP1.10003.001")      # Cites the specific data product used
# -------------------------------------------------------------------


# Set up a NEON data directory  ### This might be the hardest part for people to understand
# "C:\\Users\\Owner\\AppData\\Local\\neonstore\\neonstore"##!!! is there a list of the different directories tempdir(), etc.?
# -------------------------------------------------------------------

Sys.unsetenv("NEONSTORE_HOME")
neon_dir()

# -------------------------------------------------------------------

# Download NEON data product into the local neon_dir()
# Primary meteorological data needed to run GLM includes Shortwave, Longwave, Airtemp, Relative Humidity, 
# Windspeed, Rain, and Snow

# Relative Humidity
neon_download(
  "DP1.00098.001",
  start_date = "2013-01-01",
  end_date = "2020-08-01",
  site = c("BARC","CRAM","PRLA", "PRPO", "SUGG", "TOOK"),
  type = "expanded",
  file_regex = "[.]csv",
  quiet = FALSE,
  verify = TRUE,
  dir = neon_dir(),
  unzip = TRUE,
  api = "https://data.neonscience.org/api/v0",
  .token = Sys.getenv("NEON_TOKEN")
)

rel_hum_dat <- neon_read(
  table = "RH_30min-expanded",
  product = "DP1.00098.001",
  site = c("BARC","CRAM","PRLA", "PRPO", "SUGG", "TOOK"),
  start_date = "2018-01-01",
  end_date = "2019-01-01",
  ext = "csv",
  timestamp = NA,
  dir = neon_dir(),
  files = NULL,
  sensor_metadata = TRUE,
  altrep = FALSE
)
