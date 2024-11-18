###############################################################
# Project:  National Health Survey 2022 Analysis
# Purpose:  Read and clean self-assessed health data by state
# Inputs:   TABLE 2, sheet: Table 2.3_Proportions
# Outputs:  solutions/3-clean-data/vis1.csv
#           solutions/3-clean-data/vis1.Rda
# Author:   Mark Hanly
###############################################################

##################
# Load libraries #
##################

library(readxl)
library(dplyr)
library(tidyr)

#################
# Read the data #
#################

# Spreadsheet url
myURL <- "https://www.abs.gov.au/statistics/health/health-conditions-and-risks/national-health-survey/2022/NHSDC02.xlsx"

# Download the file locally
download.file(url = myURL,
              destfile = 'solutions/2-raw-data/national-health-survey-2022-table-2.xlsx',
              mode = 'wb')

# Read in the raw data
df1Raw <- read_excel(
            path = 'solutions/2-raw-data/national-health-survey-2022-table-2.xlsx',
            sheet = 'Table 2.3_Proportions',
            .name_repair = ~ make.names(.x, unique = TRUE),
            skip = 5
          )

# Define health status levels
healthStatus <- c("Poor", "Fair", "Good", "Very good", "Excellent")

# Clean the raw data
df1Clean <- df1Raw |>
  filter(X %in% healthStatus) |>
  select(c(X, ends_with(".1"), -'Australia.1')) |>
  pivot_longer(
    cols = ends_with(".1"),
    names_to = 'state',
    values_to = 'percent') |>
  rename(status = X) |>
  mutate(
    state = factor(
      case_when (
        state == 'NSW.1' ~ 'New South Wales',
        state == 'Vic..1' ~ 'Victoria',
        state == 'Qld.1' ~ 'Queensland',
        state == 'SA.1' ~ 'South Australia',
        state == 'WA.1' ~ 'Western Australia',
        state == 'Tas..1' ~ 'Tasmania',
        state == 'NT.1' ~ 'Northern Territory',
        state == 'ACT.1' ~ 'Australian Capital Territory'
      )
    ),
    status = factor(status, levels = healthStatus)
  ) |>
  arrange(state, status)


# Save the data in .Rda format
save(df1Clean, file=here::here('solutions/3-clean-data/vis1.Rda'))

# Save the data in .csv format
write.csv(df1Clean,
          file=here::here('solutions/3-clean-data/vis1.csv'),
          row.names = FALSE)

### # JAGO # ###
