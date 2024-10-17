###############################################################
# Project:  National Health Survey 2022 Analysis
# Purpose:  Read and clean self-assessed health data by state
# Inputs:   TABLE 2, sheet: Table 2.3_Proportions
# Outputs:  clean-data/vis1.csv
#           clean-data/vis1.Rda
# Author:   Mark Hanly
###############################################################

##################
# Load libraries #
##################

library(dplyr)
library(openxlsx)
library(tidyr)

#################
# Read the data #
#################

# Spreadsheet url
url <- "https://www.abs.gov.au/statistics/health/health-conditions-and-risks/national-health-survey/2022/NHSDC02.xlsx"

# Download the file locally
download.file(url, 'raw-data/national-health-survey-2022-table-2.xlsx', mode = "wb")

# Read in the raw data
df1Raw <- openxlsx::read.xlsx(
  xlsxFile = 'raw-data/national-health-survey-2022-table-2.xlsx',
  sheet = 'Table 2.3_Proportions',
  startRow = 6
  )

# Make sure column names are unique
names(df1Raw) <- make.unique(names(df1Raw))

# Define health status levels
healthStatus <- c("Poor", "Fair", "Good", "Very good", "Excellent")

# Clean the raw data
df1Clean <- df1Raw |>
  filter(X1 %in% healthStatus) |>
  select(c(X1, ends_with(".1"), -'Australia.1')) |>
  pivot_longer(
    cols = ends_with(".1"),
    names_to = 'state',
    values_to = 'percent') |>
  rename(status = X1) |>
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
save(df1Clean, file=here::here('clean-data/vis1.Rda'))

# Save the data in .csv format
write.csv(df1Clean,
          file=here::here('clean-data/vis1.csv'),
          row.names = FALSE)

### # JAGO # ###
