# Load packages ----------------------------------------------------------------

library(shiny)
library(tidyverse)
library(ggthemes)
library(scales)
library(countrycode)

# Load and prep data -----------------------------------------------------------

manager_survey <- read_csv(
  "data/survey.csv",
  na = c("", "NA"),
  show_col_types = FALSE
)

manager_survey <- manager_survey |>
  filter(
    !is.na(industry),
    !is.na(highest_level_of_education_completed),
    currency == "USD"
  ) |>
  mutate(
    industry_other = fct_lump_min(industry, min = 100),
    country = countrycode(country, origin = "country.name", destination = "cldr.name.en"),
    highest_level_of_education_completed = fct_relevel(
      highest_level_of_education_completed,
      "High School",
      "Some college",
      "College degree",
      "Master's degree",
      "Professional degree (MD, JD, etc.)",
      "PhD"
    ),
    highest_level_of_education_completed = fct_recode(
      highest_level_of_education_completed,
      "Professional degree" = "Professional degree (MD, JD, etc.)"
    )
  )

# Find all industries ----------------------------------------------------------

industry_choices <- manager_survey |>
  distinct(industry_other) |>
  arrange(industry_other) |>
  pull(industry_other)

# Randomly select 3 industries to start with -----------------------------------

selected_industry_choices <- sample(industry_choices, 3)

# Define UI --------------------------------------------------------------------

ui <- fluidPage(
  titlePanel(title = "___"),
  sidebarLayout(
    
    # Sidebar panel
    sidebarPanel(
      # Checkbox for selecting industries
      # add code here
    ),
    
    # Main panel
    mainPanel(
      # Informational text
      "Showing only results for those with salaries in USD who have provided information on their industry and highest level of education completed.",
      
      # Print number of selected industries
      # add code here
      
      # Make a table of filtered data
      # add code here
    )
  )
)

# Define server function -------------------------------------------------------

server <- function(input, output, session) {
  
  # Print number of selected industries
  # add code here
  
  # Filter data for selected industries
  # add code here
  
  # Make a table of filtered data
  # add code here
  
}

# Create the Shiny app object --------------------------------------------------

shinyApp(ui = ui, server = server)
