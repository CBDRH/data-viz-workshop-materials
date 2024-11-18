# Load libraries
library(readxl)

# Read the data

dataLocation <- here::here('exercises/2-raw-data/pivot_longer_exercise.xlsx')

df <- read_excel(
        path = dataLocation,
        sheet = 'Sheet1')

df |>
  pivot_longer(c('male', 'female'),
               names_to = 'sex',
               values_to = 'count') |>
  mutate(sex = factor(sex, levels=c('female', 'male')))
