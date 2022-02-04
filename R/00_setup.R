library(tidyverse)
library(cli)
library(sf)
library(redist)
library(geomander)
library(wacolors)
library(patchwork)
library(scales)
library(ggrepel)
library(here)
library(fs)
library(redistmetrics)
library(ggredist)
library(censable)
library(PL94171)


# make a key with https://api.census.gov/data/key_signup.html
# uncomment and run the next 3 lines with your key:
# tidycensus::census_api_key(
# key = "YOUR_KEY_GOES_HERE"
# )

lapply(Sys.glob(here('R/utils/*.R')), source)

cli_alert_success('Packages loaded and util files sourced.')
