library(tidyverse)
library(stringr)

##############
# ANALYZE DATA
##############
# External data: https://catalog.data.gov/dataset?publisher=data.lacity.org

dd <- read_rds("dd.rds")

# Challenging roles to fill.
# In the future: IT ("Applications Programmer"), Wastewater, Inspector, Journey-level.

challenging_roles <- c("Accountant",
               "Accounting Clerk",
               "Applications Programmer",
               "Assistant Street Lighting Electrician",
               "Building Mechanical Inspector",
               "Detention Officer",
               "Electrical Mechanic",
               "Equipment Mechanic",
               "Field Engineering Aide",
               "Housing Inspector",
               "Housing Investigator",
               "Librarian",
               "Security Officer",
               "Senior Administrative Clerk",
               "Senior Custodian",
               "Senior Equipment Mechanic",
               "Tree Surgeon")

# Is diversity correlated with number of pathways?
# Which 



