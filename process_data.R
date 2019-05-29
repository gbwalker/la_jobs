library(tidyverse)
library(stringr)
library(stringi)

#################
# DATA PROCESSING
#################

### Read in the job bulletins.

# Obtain a list of the bulletin file names.

bulletins_list <- list.files("CityofLA/Job Bulletins",
                             pattern = "*.txt")

# Read in each raw text file.

setwd("CityofLA/Job Bulletins/")
raw <- lapply(bulletins_list, read_delim, delim = "\n")

# Make a list of all of the job titles.
# Clean the whitespace and change to title case for ease of reading.

titles <- map_chr(raw, names) %>% 
  str_to_title() %>% 
  str_trim()

###
# Group information in the job files that are under the same heading.
###

# Iterate through all of the job listings for j in 1:length(raw)

j <- 1

#################################################
# Function for capturing lines below header rows.
#################################################

# Make a list of all of the possible capitalized headers to check.

headers <- c("ANNUAL SALARY",
             "DUTIES",
             "REQUIREMENT",
             "WHERE TO APPLY",
             "APPLICATION DEADLINE",
             "SELECTION PROCESS",
             "PROCESS NOTES")

# Create a regex stopping rule for headers. It must start with a series of capital letters.

rule <- "^[A-Z][A-Z][A-Z]"

# This function captures the information below headers in job postings, including associated notes.
# Takes a line n from a listing j in the raw data.
# Takes a header that's a string of capital letters.

capture_header <- function(j, n, header) {
  
  if (str_detect(raw[[j]][n, ], header)) {
    
    # Check the next ten rows for a stopping point, i.e., the next title of all capitals.
    
    for (r in 1:15) {
      
      # If the lines are out of bounds, then stop checking them.
      
      if (is.na(raw[[j]][n + r, ])) {
        stop <- r - 1
        break
      }
      
      # Otherwise, set the stop point for non-note headers.
      
      if (str_detect(raw[[j]][n + r, ], rule) &
          !str_detect(raw[[j]][n + r, ], "^NOTE")) {
        stop <- r
        break
      }
    }
    
    # Return all of the lines after a header.
    
    return (c(raw[[j]][(n+1):(n+stop-1), ]))
  }
}

#######################
# SCRAPE EVERY LISTING.
#######################

# Initiate a tibble with all of the possible variable names of interest.

listing <- tibble(title = titles[j],
                  class_code = NA,
                  open_date = NA,
                  exam_status = NA,
                  salary = NA,
                  duties = NA,
                  notice = NA,
                  requirements = NA,
                  process_notes = NA,
                  where = NA,
                  deadline = NA,
                  selection_process = NA)

for (n in 1:nrow(raw[[j]])) {
  
  # Capture the class code.
  # Add a fix to capture the class code from the job title.
  
  if (str_detect(raw[[j]][n, ], "Class Code")) {
    listing$class_code[1] <- str_extract(raw[[j]][n, ], "\\d\\d\\d\\d")
  }
  
  # Capture the open date.
  
  if (str_detect(raw[[j]][n, ], "Open Date")) {
    listing$open_date[1] <- str_extract(raw[[j]][n, ], "\\d\\d-\\d\\d-\\d\\d")
  }
  
  # Capture the exam status.
  
  if (str_detect(raw[[j]][n, ], "THIS EXAMINATION")) {
    listing$exam_status[1] <- as.character(raw[[j]][(n+1), ]) %>% 
      str_to_sentence()
  }
  
  # Capture any additional notice.
  
  if (str_detect(raw[[j]][n, ], "Notice:") | 
      str_detect(raw[[j]][n, ], "NOTICE:") |
      str_detect(raw[[j]][n, ], "NOTE")) {
    listing$notice[1] <- raw[[j]][n + 1, ]
  }
  
  ########################################
  ### Scrape the data beneath header rows.
  ########################################
  
  if (str_detect(raw[[j]][n, ], rule)) {
        
    # Capture salary.
    
    if (str_detect(raw[[j]][n, ], "SALARY")) {
      listing$salary[1] <- capture_header(j, n, "SALARY")
    }

    # Capture duties.
    
    if (str_detect(raw[[j]][n, ], "DUTIES")) {
      listing$duties[1] <- capture_header(j, n, "DUTIES")
    }
    
    # Capture job requirements.
    
    if (str_detect(raw[[j]][n, ], "REQUIREMENT")) {
      listing$requirements[1] <- capture_header(j, n, "REQUIREMENT")
    }
      
    # Capture process notes.
    
    if (str_detect(raw[[j]][n, ], "PROCESS NOTES")) {
      listing$process_notes[1] <- capture_header(j, n, "PROCESS NOTES")
    }
    
    # Capture where to apply.
    
    if (str_detect(raw[[j]][n, ], "WHERE TO APPLY")) {
      listing$where[1] <- capture_header(j, n, "WHERE TO APPLY")
    }
    
    # Capture application deadline.
    
    if (str_detect(raw[[j]][n, ], "APPLICATION DEADLINE")) {
      listing$deadline[1] <- capture_header(j, n, "APPLICATION DEADLINE")
    }
    
    # Capture selection process.
    
    if (str_detect(raw[[j]][n, ], "SELECTION PROCESS")) {
      listing$selection_process[1] <- capture_header(j, n, "SELECTION PROCESS")
    }
    
  }
}

#####
# SECOND ATTEMPT USING CAPITAL LETTERS
#####

# Read in all the files as one block of text each.

raw2 <- lapply(bulletins_list, read_file)

test <- stri_extract_all_charclass(raw2[[1]], "[A-Z]")
