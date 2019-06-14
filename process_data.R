library(tidyverse)
library(stringr)
library(lubridate)

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
setwd("C:/Users/Gabriel/Documents/R/la_jobs")

# Make a list of all of the job titles.
# Clean the whitespace and change to title case for ease of reading.

titles <- map_chr(raw, names) %>% 
  str_to_title() %>% 
  str_trim()

#################################################
# Function for capturing lines below header rows.
#################################################

# Create a regex stopping rule for headers. It must start with a series of capital letters.

rule <- "^[A-Z][A-Z][A-Z]"

# This function captures the information below headers in job postings, including associated notes.
# Takes a line n from a listing j in the raw data.
# Takes a header that's a string of capital letters.

capture_header <- function(j, n, header) {
  
  if (str_detect(raw[[j]][n, ], header)) {
    
    # Check all of the following rows for a stopping point, i.e., the next title of all capitals.
    
    for (r in 1:nrow(raw[[j]])) {
      
      # If the lines are out of bounds, then stop checking them.
      
      if (is.na(raw[[j]][n + r, ])) {
        stop_point <- r - 1
        break
      }
      
      # Otherwise, set the stop point for non-note headers.
      
      if (str_detect(raw[[j]][n + r, ], rule) &
          !str_detect(raw[[j]][n + r, ], "^NOTE")) {
        stop_point <- r
        break
      }
      
      # Set a stop point for "notices" because it's already captured in another field.
      
      if (str_detect(raw[[j]][n + r, ], "Notice:") |
          str_detect(raw[[j]][n + r, ], "NOTICE:")) {
        stop_point <- r
        break
      }
    }
    
    # Return all of the lines after a header.
    
    return (c(raw[[j]][(n + 1):(n + stop_point - 1), ]))
  }
}

#######################
# SCRAPE EVERY LISTING.
#######################

# Note that I corrected the file name of "vocational worker" (in the PWD) in order for it to be read correctly.
# Initiate a final blank dataframe for all of the listing results.

df <- tibble()

# Iterate through all of the raw listings.

for (j in 1:length(raw)) {
  
  # Initiate a tibble with all of the possible variable names of interest.
  
  listing <- tibble(file = bulletins_list[j],
                    title = titles[j],
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
                    selection_process = NA,
                    expert = NA,
                    discriminate = NA,
                    equal = NA,
                    info = NA)
  
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
    
    # Capture any additional notice in the same field or next field.
    # In a few instances the notice is on the same line.
    
    if (str_detect(raw[[j]][n, ], "Notice:") | 
        str_detect(raw[[j]][n, ], "NOTICE:")) {
      
      # Ignore the notice with a mention of an examination in the next line.
      # Otherwise capture the next line.
      
      if (str_detect(raw[[j]][n + 1, ], "THIS EXAMINATION")) {
        listing$notice[1] <- raw[[j]][n, ]
      }
      else (listing$notice[1] <- raw[[j]][n + 1, ])
    }
    
    # Capture "does not discriminate" clause.
    
    if (str_detect(raw[[j]][n, ], "does not discriminate")) {
      listing$discriminate[1] <- as.character(raw[[j]][n, ])
    }
    
    # Capture "equal opportunity employer" clause.
    
    if (str_detect(raw[[j]][n, ], "EQUAL EMPLOYMENT OPPORTUNITY") |
        str_detect(raw[[j]][n, ], "equal employment opportunity")) {
      listing$equal[1] <- as.character(raw[[j]][n, ])
    }
    
    # Capture "additional information" clause.
    
    if (str_detect(raw[[j]][n, ], "additional information")) {
      listing$info[1] <- as.character(raw[[j]][n, ])
    }
    
    ########################################
    ### Scrape the data beneath header rows.
    ########################################
    
    # Only perform this loop for header rows to save some computation time.
    
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
      
      # Capture expert review committee.
      
      if (str_detect(raw[[j]][n, ], "EXPERT")) {
        listing$expert[1] <- capture_header(j, n, "EXPERT")
      }
    }
  }
  
  # Combine all of the tidy data.

  df <- bind_rows(df, listing)
}

# Manually turn the NULL values to NAs.

for (col in 1:ncol(df)) {
  for (row in 1:nrow(df)) {
    if (is_null(df[[row, col]])) {
      df[[row, col]] <- NA
    }
  }
}

# Fix some of the titles that include a class code.

df$title  <- str_remove(df$title, "\\s+Class Code.+")

# Save the final dataframe.

write_rds(df, "df.rds")

#################
# DATA DICTIONARY
#################

### Arrange the data into an easily usable format.

###################################
# Extract the initial salary range.
###################################

salaries <- tibble(salary_low = rep(NA, nrow(df)),
                   salary_high = rep(NA, nrow(df)),
                   salary_dwp = rep(NA, nrow(df)))

for (n in 1:nrow(df)) {
  
  # Only identify salaries for fields that have some salary information listed.
  
  if (! is.na(df$salary[[n]][[1]])) {
    
    # Extract all of the salaries listed.
    
    all_salaries <- str_extract_all(df$salary[[n]][[1]], "\\$\\d+,\\d+")
    
    # Identify the low salary.
    
    salaries$salary_low[n] <- all_salaries[[1]][1] %>% 
      str_remove("\\$") %>% 
      str_remove(",") %>% 
      as.numeric()
    
    # Identify the second salary listed.
    
    salaries$salary_high[n] <- all_salaries[[1]][2] %>% 
      str_remove("\\$") %>% 
      str_remove(",") %>% 
      as.numeric()
    
    # Identify the DWP salary (most likely the third listed).
    
    salaries$salary_dwp[n] <- all_salaries[[1]][3] %>% 
      str_remove("\\$") %>% 
      str_remove(",") %>% 
      as.numeric()
    
    }
}

#############################
### Extract the requirements.
#############################

# A function to collect the lettered items following a certain entry.
# Accepts the list location of a specific entry with n and i.
# Returns a collated list of lettered sub-items.

collect_letters <- function(n, i) {
  
  collected <- c()
  
  for (j in i:length(df$requirements[[n]])) {
    
    # If the entry starts with a number it should be the start of an entry.
    
    if (str_detect(df$requirements[[n]][j], "^\\d.")) {
      collected <- c(df$requirements[[n]][j])
    }
    
    # If the entry starts with a lettered item, add it to the collection.
    # Ignore it if it starts with a digit.
    
    if ((str_detect(df$requirements[[n]][j], "^\\w.") | 
        str_detect(df$requirements[[n]][j], "^\\(\\w\\)") | 
        str_detect(df$requirements[[n]][j], "^\\w\\)")) &
        !str_detect(df$requirements[[n]][j], "^\\d.")) {
      collected <- paste0(collected, " ", df$requirements[[n]][j])
    }
    
    # If the next item is a digit or if it's run out of entries to check, then return the collection.
    
    if (is.na(df$requirements[[n]][(j + 1)]) |
        str_detect(df$requirements[[n]][(j + 1)], "^\\d.")) {
      return (collected)
    }
  }
}

# Split requirements into 1, 2, 3, etc. and then "other" based on whether they contain 1., 2., 3., etc.

requirements <- tibble(req_1 = rep(NA, nrow(df)),
                       req_2 = rep(NA, nrow(df)),
                       req_3 = rep(NA, nrow(df)),
                       req_4 = rep(NA, nrow(df)),
                       req_5 = rep(NA, nrow(df)),
                       req_6 = rep(NA, nrow(df)),
                       req_7 = rep(NA, nrow(df)),
                       req_8 = rep(NA, nrow(df)),
                       req_notes = rep(NA, nrow(df)))

for (n in 1:nrow(df)) {
  
  # Only identify requirements that are listed.
  
  if (! is.na(df$requirements[[n]])) {
    
    # Go through each item of each requirement.
    
    for (i in 1:length(df$requirements[[n]])) {
      
      # Pull out single-item requirements or ones that don't contain numbered items.
      
      if (length(df$requirements[[n]]) == 1 | 
          !str_detect(df$requirements[[n]][1], "^1. ")) {
        
        requirements$req_1[n] <- paste0(df$requirements[[n]], collapse = " ")
        
        break
      }
      
      # Pull out each of the first three items.
      
      if (str_detect(df$requirements[[n]][[i]], "^1. ")) {
        requirements$req_1[n] <- collect_letters(n, i)
      }
      if (str_detect(df$requirements[[n]][i], "^2. ")) {
        requirements$req_2[n] <- collect_letters(n, i)
      }
      if (str_detect(df$requirements[[n]][i], "^3. ")) {
        requirements$req_3[n] <- collect_letters(n, i)
      }
      if (str_detect(df$requirements[[n]][i], "^4. ")) {
        requirements$req_4[n] <- collect_letters(n, i)
      }
      if (str_detect(df$requirements[[n]][i], "^5. ")) {
        requirements$req_5[n] <- collect_letters(n, i)
      }
      if (str_detect(df$requirements[[n]][i], "^6. ")) {
        requirements$req_6[n] <- collect_letters(n, i)
      }
      if (str_detect(df$requirements[[n]][i], "^7. ")) {
        requirements$req_7[n] <- collect_letters(n, i)
      }
      if (str_detect(df$requirements[[n]][i], "^8. ")) {
        requirements$req_8[n] <- collect_letters(n, i)
      }
      
      # Put everything else into the last slot.
     
      if (str_detect(df$requirements[[n]][i], "NOTES")) {
        requirements$req_notes[n] <- paste0(df$requirements[[n]][i:length(df$requirements[[n]])],
                                            collapse = " ")
        break
      }
    }
  }
}

# Combine all the cleaned information into one final data dictionary.

dd <- df %>% 
  bind_cols(salaries) %>% 
  mutate(exam_status = str_trim(exam_status),
         open_date = mdy(open_date),
         title = str_trim(title),
         notice = str_trim(notice),
         discriminate = str_trim(discriminate),
         equal = str_trim(equal),
         info = str_trim(info)) %>% 
  select(-salary)



############
# NEXT STEPS
############
# Turn this df into one that fits nice variable categories (salary, etc.).
# Figure out the promotion task and how to identify that text.
# Find what language counts as "biased" (see AMA).
# Turn this df into one that's multidimensional (tons of specific markers for each job).
# Perform analysis, make visualizations. :)