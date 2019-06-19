library(tidyverse)
library(stringr)
library(lubridate)
library(DiagrammeR)

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

# titles <- map_chr(raw, names) %>% 
#   str_to_title() %>% 
#   str_trim() %>% 
#   str_replace(" And ", " and ") %>% 
#   str_replace(" - ", " ") %>% 
#   str_replace("Campus Interviews Only", "Architectural Associate")

titles <- bulletins_list %>% 
  str_extract(pattern = "^...[-\\s\\A-Z\\_]+") %>% 
  str_trim() %>% 
  str_to_title() %>% 
  str_replace(" And ", " and ") %>% 
  str_replace(" Of ", " of ") %>% 
  str_replace(" _ ", " and ") %>% 
  str_replace("Sr ", "Senior ") %>% 
  str_replace("s_", "s'")

#################################################
# Function for capturing lines below header rows.
#################################################

# Create a regex stopping rule for headers. It must start with a series of capital letters or spaces and then capitals.

# rule <- "^[A-Z][A-Z][A-Z]"

rule <- "^?[\t\\sA-Z][A-Z][A-Z]"

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
    
    # Check the second line if the first is empty.
    
    if (is_empty(all_salaries[[1]]) &
        !is.na(df$salary[[n]][2])) {
      all_salaries <- str_extract_all(df$salary[[n]][2], "\\$\\d+,\\d+")
    }
    
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
    
    if (!is.na(df$salary[[n]][2]) &
        str_detect(df$salary[[n]][2], "Department of Water and Power")) {
      
      salaries$salary_dwp[n] <- str_extract(df$salary[[n]][2], "\\$\\d+,\\d+") %>% 
        str_remove("\\$") %>% 
        str_remove(",") %>% 
        as.numeric()
      }
    
    else {
      salaries$salary_dwp[n] <- all_salaries[[1]][3] %>% 
        str_remove("\\$") %>% 
        str_remove(",") %>% 
        as.numeric()
      }
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

requirements <- tibble(req1 = rep(NA, nrow(df)),
                       req2 = rep(NA, nrow(df)),
                       req3 = rep(NA, nrow(df)),
                       req4 = rep(NA, nrow(df)),
                       req5 = rep(NA, nrow(df)),
                       req6 = rep(NA, nrow(df)),
                       req7 = rep(NA, nrow(df)),
                       req8 = rep(NA, nrow(df)),
                       req_notes = rep(NA, nrow(df)))

for (n in 1:nrow(df)) {
  
  # Only identify requirements that are listed.
  
  if (! is.na(df$requirements[[n]])) {
    
    # Go through each item of each requirement.
    
    for (i in 1:length(df$requirements[[n]])) {
      
      # Pull out single-item requirements or ones that don't contain numbered items.
      
      if (length(df$requirements[[n]]) == 1 | 
          !str_detect(df$requirements[[n]][1], "^1. ")) {
        
        requirements$req1[n] <- paste0(df$requirements[[n]], collapse = " ")
        
        break
      }
      
      # Pull out each of the first three items.
      
      if (str_detect(df$requirements[[n]][[i]], "^1. ")) {
        requirements$req1[n] <- collect_letters(n, i)
      }
      if (str_detect(df$requirements[[n]][i], "^2. ")) {
        requirements$req2[n] <- collect_letters(n, i)
      }
      if (str_detect(df$requirements[[n]][i], "^3. ")) {
        requirements$req3[n] <- collect_letters(n, i)
      }
      if (str_detect(df$requirements[[n]][i], "^4. ")) {
        requirements$req4[n] <- collect_letters(n, i)
      }
      if (str_detect(df$requirements[[n]][i], "^5. ")) {
        requirements$req5[n] <- collect_letters(n, i)
      }
      if (str_detect(df$requirements[[n]][i], "^6. ")) {
        requirements$req6[n] <- collect_letters(n, i)
      }
      if (str_detect(df$requirements[[n]][i], "^7. ")) {
        requirements$req7[n] <- collect_letters(n, i)
      }
      if (str_detect(df$requirements[[n]][i], "^8. ")) {
        requirements$req8[n] <- collect_letters(n, i)
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
 
####################################################################
### Combine the process, location, duties, and deadline information.
### Create full requirements and selection process fields.
####################################################################
  
misc <- tibble(process = rep(NA, nrow(df)),
               location = rep(NA, nrow(df)),
               duties_temp = rep(NA, nrow(df)),
               deadline_temp = rep(NA, nrow(df)),
               req_all = rep(NA, nrow(df)),
               selection_temp = rep(NA, nrow(df)),
               selection_notes = rep(NA, nrow(df)))

# Iterate through every job listing to combine the process notes, location, and deadline information.

for (n in 1:nrow(df)) {
  
  misc$process[n] <- paste0(df$process_notes[[n]][1:length(df$process_notes[[n]])], collapse = " ") %>% 
    str_remove_all("NA")
  
  misc$location[n] <- paste0(df$where[[n]][1:length(df$where[[n]])], collapse = " ") %>% 
    str_remove_all("NA")
  
  misc$duties_temp[n] <- paste0(df$duties[[n]][1:length(df$duties[[n]])], collapse = " ") %>% 
    str_remove_all("NA")
  
  misc$deadline_temp[n] <- paste0(df$deadline[[n]][1:length(df$deadline[[n]])], collapse = " ") %>% 
    str_remove_all("NA")

  # Get main selection information and notes.
    
  selection <- paste0(df$selection_process[[n]][1:length(df$selection_process[[n]])], collapse = " ") %>% 
    str_remove_all("NA") 
  
  misc$selection_temp[n] <- selection %>% 
    str_split("NOTES:", 2, simplify = TRUE) %>% 
    first() %>% 
    str_trim()
  
  misc$selection_notes[n] <- selection %>% 
    str_split("NOTES:", 2, simplify = TRUE) %>% 
    last() %>% 
    str_trim()

  # Create a full requirements field.
    
  misc$req_all[n] <- paste(requirements$req1[n], 
                            requirements$req2[n], 
                            requirements$req3[n], 
                            requirements$req4[n], 
                            requirements$req5[n], 
                            requirements$req6[n], 
                            requirements$req7[n], 
                            requirements$req8[n], 
                            requirements$req_notes[n],
                            sep = " ") %>% 
    str_remove_all("NA") %>% 
    str_remove("NOTES:")
}

### Combine all the cleaned information into a data dictionary.

dd <- df %>% 
  bind_cols(salaries, requirements, misc) %>% 
  mutate(exam_status = str_trim(exam_status),
         open_date = mdy(open_date),
         title = str_trim(title),
         notice = str_trim(notice),
         discriminate = str_trim(discriminate),
         equal = str_trim(equal),
         info = str_trim(info),
         salary_low = as.numeric(salary_low),
         salary_high = as.numeric(salary_high),
         salary_dwp = as.numeric(salary_dwp),
         process_req = NA,
         license = NA,
         education = NA,
         education_req = NA,
         semesters = NA,
         quarters = NA,
         degree = NA,
         experience = NA,
         y1 = NA,
         y2 = NA,
         y3 = NA,
         y4 = NA,
         y5 = NA,
         experience_time = NA,
         pathway = NA) %>% 
  select(-salary, -requirements, -process_notes, -where, -deadline, -duties, -selection_process) %>% 
  rename(deadline = deadline_temp,
         duties = duties_temp,
         selection = selection_temp)

### Go through each entry to identify other requirements such as licenses, education, years of work, etc.

for (n in 1:nrow(dd)) {
  
  ### Process requirements.
  
  if (str_detect(dd$selection[n], "[Ii]nterview")) {
    dd$process_req[n] <- paste(dd$process_req[n], "interview", sep = ", ")
  }
  if (str_detect(dd$selection[n], "[Tt]est")) {
    dd$process_req[n] <- paste(dd$process_req[n], "test", sep = ", ")
  }
  if (str_detect(dd$selection[n], "[Ee]ssay")) {
    dd$process_req[n] <- paste(dd$process_req[n], "essay", sep = ", ")
  }
  if (str_detect(dd$selection[n], "[Qq]uestionnaire")) {
    dd$process_req[n] <- paste(dd$process_req[n], "questionnaire", sep = ", ")
  }
  if (str_detect(dd$selection[n], "[Aa]pplication [Rr]eview")) {
    dd$process_req[n] <- paste(dd$process_req[n], "review", sep = ", ")
  }
  
  # Remove the first comma and space and NA value.
  
  dd$process_req[n] <- str_remove(dd$process_req[n], "NA") %>% 
    str_remove(", ")
  
  ### License requirements.
  
  if (str_detect(dd$req_all[n], "driver's license") |
      str_detect(dd$req_all[n], "drivers' license")) {
    dd$license[n] <- paste(dd$license[n], "driver's license", sep = ", ")
  }
  if (str_detect(dd$req_all[n], "CPR")) {
    dd$license[n] <- paste(dd$license[n], "CPR", sep = ", ")
  }
  if (str_detect(dd$req_all[n], "Nurse") |
      str_detect(dd$req_all[n], "Physician Assistant")) {
    dd$license[n] <- paste(dd$license[n], "nursing or PA", sep = ", ")
  }
  if (str_detect(dd$req_all[n], "Engineer")) {
    dd$license[n] <- paste(dd$license[n], "engineer", sep = ", ")
  }
  if (str_detect(dd$req_all[n], "Water Treatment Operator")) {
    dd$license[n] <- paste(dd$license[n], "water treatment operator", sep = ", ")
  }
  if (str_detect(dd$req_all[n], "Architect")) {
    dd$license[n] <- paste(dd$license[n], "architect", sep = ", ")
  }
  if (str_detect(dd$req_all[n], "Mechanic")) {
    dd$license[n] <- paste(dd$license[n], "mechanic", sep = ", ")
  }
  if (str_detect(dd$req_all[n], "Plumber")) {
    dd$license[n] <- paste(dd$license[n], "plubmer", sep = ", ")
  }
  if (str_detect(dd$req_all[n], "license") &
      str_detect(dd$req_all[n], "United States Coast Guard")) {
    dd$license[n] <- paste(dd$license[n], "coast guard", sep = ", ")
  }
  if (str_detect(dd$req_all[n], "license") &
      (str_detect(dd$req_all[n], "construction") | 
       str_detect(dd$req_all[n], "contractor"))) {
    dd$license[n] <- paste(dd$license[n], "construction", sep = ", ")
  }

  # Clean up the aesthetics of the field.
  
  dd$license[n] <- str_remove(dd$license[n], "NA") %>% 
    str_remove(", ")
  
  ### Education requirements.
  
  if (str_detect(dd$req_all[n], "[Cc]ollege") |
      str_detect(dd$req_all[n], "[Uu]niversity")) {
    dd$education[n] <- paste(dd$education[n], "college or university", sep = ", ")
  }
  if (str_detect(dd$req_all[n], "[Tt]rade school")) {
    dd$education[n] <- paste(dd$education[n], "trade school", sep = ", ")
  }
  if (str_detect(dd$req_all[n], "[Hh]igh school")) {
    dd$education[n] <- paste(dd$education[n], "high school", sep = ", ")
  }
  if (str_detect(dd$req_all[n], "[Aa]pprentice")) {
    dd$education[n] <- paste(dd$education[n], "apprenticeship", sep = ", ")
  }
  if (str_detect(dd$req_all[n], "[Cc]ertifi")) {
    dd$education[n] <- paste(dd$education[n], "certificate", sep = ", ")
  }
  if (str_detect(dd$req_all[n], "[Mm]aster's degree")) {
    dd$education[n] <- paste(dd$education[n], "master's", sep = ", ")
  }
  
  # Specific degree information.
  
  if (str_detect(dd$req_all[n], "degree in")) {
    dd$degree[n] <- str_extract(dd$req_all[n], "degree in[\\w\\W\\s,]+?[\\;\\.]") %>% 
      str_remove("degree in") %>% 
      str_remove(";") %>% 
      str_remove(".") %>% 
      str_trim() %>% 
      str_squish()
  }
  
  dd$education[n] <- str_remove(dd$education[n], "NA") %>% 
    str_remove(", ")
  
  ### Get required number of semesters or quarters.
  
  if (str_detect(dd$req_all[n], "semester")) {
    dd$education_req[n] <- str_extract(dd$req_all[n], "[Cc]ompletion of [\\d\\w\\s]+or[\\d\\w\\s]+")
  }
  
  # Get the numeric values for semesters and quarters.
  
  if (!is.na(dd$education_req[n])) {
    
    dd$semesters[n] <- str_extract(dd$education_req[n], "\\d?\\d semester") %>% 
      str_remove(" semester") %>% 
      as.numeric()
    
    # If the field contains text rather than a number, convert the text to a number.
    
    if (is.na(dd$semesters[n])) {
      semesters <- str_extract(dd$education_req[n], "\\w+ semester") %>% 
        str_remove(" semester")
      
      dd$semesters[n] <- case_when(semesters == "three" ~ 3,
                                   semesters == "four" ~ 4,
                                   semesters == "sixty" ~ 60,
                                   semesters == "ninety" ~ 90,
                                   TRUE ~ dd$semesters[n])  
    }
    
    dd$quarters[n] <- str_extract(dd$education_req[n], "\\d?\\d quarter") %>% 
      str_remove(" quarter") %>% 
      as.numeric()
  
  # If the field contains text rather than a number, convert the text to a number.
  
  if (is.na(dd$quarters[n])) {
    quarters <- str_extract(dd$education_req[n], "\\w+ quarter") %>% 
      str_remove(" quarter")
    
    dd$quarters[n] <- case_when(quarters == "three" ~ 3,
                                quarters == "four" ~ 4,
                                quarters == "sixty" ~ 60,
                                quarters == "ninety" ~ 90,
                                TRUE ~ dd$quarters[n]) 
    }
  }
    
  ### Get all the possible experience lengths.
  
  # Reset the experience counter.
  
  exp <- NA
  
  # Capture content when "year of full/part-time" or years are mentioned in the requirements.
  
  if (str_detect(dd$req_all[n], "years? of ")) {
    
    exp <- str_extract_all(dd$req_all[n], "[\\w]+ (\\(\\d+\\) )?years? of [\\w\\s,-]+[;.]?")
    
    for (i in 1:length(exp[[1]])) {
      
      dd$experience[n] <- paste(dd$experience[n], exp[[1]][i], sep = "/")
    
      }
  }
  
  # If the position requires months instead, find the number of months.
  
  if (is.na(dd$experience[n]) &
      str_detect(dd$req_all[n], "months of ")) {
    
    exp <- str_extract_all(dd$req_all[n], "[\\w]+ (\\(\\d+\\) )?months of [\\w\\s,-]+[;.]?")
    
    for (i in 1:length(exp[[1]])) {
      
      dd$experience[n] <- paste(dd$experience[n], exp[[1]][i], sep = "/")
      
    }
  }
  
  # Clean up the experience variable if it exists.
  
  if (!is.na(dd$experience[n])) {
  
    dd$experience[n] <- str_remove(dd$experience[n], "NA/") %>% 
      str_trim() %>% 
      str_squish()
  
  }
  
  # Pull out the number of years or months required for each experience line.
  
  for (i in 1:length(exp[[1]])) {
    
    # If the unit is years.
    
    if (!is.na(exp[[1]][i]) &
        str_detect(exp[[1]][i], "years? of ")) {
    
      t <- str_extract(exp[[1]][i], "[\\w]+") %>% 
        tolower()
      
      # Translate the word into a number.
        
        years <- case_when(t == "one" ~ 1,
                          t == "two" ~ 2,
                          t == "three" ~ 3,
                          t == "four" ~ 4,
                          t == "five" ~ 5,
                          t == "six" ~ 6,
                          t == "seven" ~ 7,
                          t == "eight" ~ 8,
                          t == "nine" ~ 9,
                          t == "ten" ~ 10,
                          t == "half" ~ .5,
                          t == "21" ~ NA_real_,
                          t == "each" ~ NA_real_,
                          TRUE ~ NA_real_)
        
        # Assign it to a variable.
        
        if (i == 1) {
          dd$y1[n] <- years
        }
        if (i == 2) {
          dd$y2[n] <- years
        }
        if (i == 3) {
          dd$y3[n] <- years
        }
        if (i == 4) {
          dd$y4[n] <- years
        }
        if (i == 5) {
          dd$y5[n] <- years
        }
    }
   
    # Do the same for months (i.e., if the above script caught no years).
    
    if (!is.na(exp[[1]][i]) &
        is.na(dd$y1[n])) {
      
      t <- str_extract(exp[[1]][i], "[\\w]+") %>% 
        tolower()
      
      months <- case_when(t == "three" ~ .25,
                          t == "six" ~ .5,
                          t == "eight" ~ .75,
                          t == "eighteen" ~ 1.5,
                          TRUE ~ NA_real_)
      
      # Catch a few special cases.
      
      months <- case_when(str_detect(dd$req_all[n], "[Ee]ighteen") ~ 1.5,
                          str_detect(dd$req_all[n], "[Ee]ighteen months of ") ~ 1.5,
                          str_detect(dd$req_all[n], "18 months of ") ~ 1.5,
                          str_detect(dd$req_all[n], "6 months of ") ~ .5,
                          TRUE ~ months)
      
        if (i == 1) {
          dd$y1[n] <- months
        }
        if (i == 2) {
          dd$y2[n] <- months
        }
        if (i == 3) {
          dd$y3[n] <- months
        }
        if (i == 4) {
          dd$y4[n] <- months
        }
        if (i == 5) {
          dd$y5[n] <- months
        }
      }
  }
  
  ### Identify full-time or part-time experience.
  
  if (str_detect(dd$req_all[n], "[Ff]ull[\\s-]time")) {
    dd$experience_time[n] <- paste(dd$experience_time[n], "full-time", sep = ", ")
  }
  
  if (str_detect(dd$req_all[n], "[Pp]art[\\s-]time")) {
    dd$experience_time[n] <- paste(dd$experience_time[n], "part-time", sep = ", ")
  }
  
  dd$experience_time[n] <- str_remove(dd$experience_time[n], "NA") %>% 
    str_remove(", ")
  
  
  ### Identify promotional pathways.
  
  # Set a current salary as a reference point (see below comment).
  
  current_salary <- dd$salary_low[n]
  
  for (i in 1:length(dd$title)) {
   
    # The current salary should be greater than the pathway salaries to have them count as pathways.
    
    pathway_salary <- dd$salary_low[i]
    
    # Only check pathways for positions with confirmed salaries.
    
    # if (!is.na(current_salary) &
        # !is.na(pathway_salary)) {
    
      # If another job title appears in the requirements of one add it to a list.
      # Ensure that it's not the position itself.
      
      if (str_detect(dd$req_all[n], regex(paste0(dd$title[i], "?[\\s\\.,;]"))) &
          dd$title[n] != dd$title[i]) {
          # current_salary > pathway_salary) {
        
        # If the position doesn't have the word "assistant" in it AND the pathway position is a subset of the current position,
        # add the confirmed pathway to the list of pathways.
        
        if (!(str_detect(dd$title[n], "Assistant") &
            str_detect(dd$title[n], dd$title[i]))) {
        
          dd$pathway[n] <- paste(dd$pathway[n], dd$title[i], sep = "; ")
        
          }
        }
    # }
  }
  
  # Clean up the look of the pathway variable.
  
  dd$pathway[n] <- dd$pathway[n] %>% 
    str_remove("NA") %>% 
    str_remove("; ")
}

### Refine the pathways so that lower-level positions with similar names are not included.
# Do so by comparing their salaries. The lower one should be eliminated.

for (n in 1:nrow(dd)) {
  
  # Only test positions that have more than one existing pathway.
  
  if (!is.na(dd$pathway[n]) &
      str_detect(dd$pathway[n], ";")) {
    
    pathways <- str_split(dd$pathway[n], "; ")
    
    # Reset the original pathways field for rewriting later.
    
    dd$pathway[n] <- NA
    
    # Save a final version of pathways to modify.
    
    final <- pathways
    
    # Find the salary for the current position.
    
    current_salary <- dd$salary_low[n]
    
    # Test each position named in the list of pathways.
    
    for (i in 1:length(pathways[[1]])) {
     
      title_test <- pathways[[1]][i]
      
      # Find the salary associated with that title.
      
      title_salary <- dd %>% 
        filter(title == title_test) %>% 
        select(salary_low)
      
      # Set the salary to $0 if none is found.
      
      if (is.na(title_salary$salary_low[1])) {
        
        title_salary <- 0
        
      }
      
      # Otherwise use the found value.
      
      else (title_salary <- title_salary$salary_low[1])
      
      for (j in 1:length(pathways[[1]])) {
       
        # If the titles overlap at all but are not the same, then compare their salaries.
        
        if (str_detect(title_test, pathways[[1]][j]) &
            title_test != pathways[[1]][j]) {
          
          # If the first is greater than the second, remove the second.
          
          if (title_salary > current_salary) {
            
            final[[1]][i] <- NA
            
          }
          
          # If the second is greater than the first, remove the first.
          
          if (title_salary < current_salary) {
            
            final[[1]][j] <- NA
            
          }
        }
      }
    }
    
    # Collapse the final revised pathway list.
    
    for (f in 1:length(final[[1]])) {
      
      dd$pathway[n] <- paste(dd$pathway[n], final[[1]][f], sep = "; ")
      
    }
    
    # Clean up the final version of the pathway.
    
    dd$pathway[n] <- str_remove_all(dd$pathway[n], "NA; ") %>% 
      str_remove("; NA")
    
  }
}


# Save the data dictionary version.

write_rds(dd, "dd.rds")



##########
# PATHWAYS
##########
### Visualize promotional pathways for all positions.

# First create a node list and edge list for DiagrammeR to use.

nodes_all <- tibble(id = 1:nrow(dd),
                    label = dd$title,
                    shape = "rectangle")

# Remove duplicate nodes (in this case the duplicated file are "Chief Clerk Police" and "Senior Utility Services Specialist").

nodes_all <- nodes_all[!duplicated(nodes_all$label), ]

# Initialize an empty set of edges (arrows).

edge_list <- tibble()

# Iterate through each position to identify sub-paths.

for (n in 1:nrow(dd)) {
  
  positions <- str_split(dd$pathway[n], "; ")
  
  # Append an additional row to the edge list for each promotional pathway that exists.
  
  for (i in 1:length(positions[[1]])) {
    
    # Find the position id of the sub-paths.
    
    location <- nodes_all %>% 
      filter(label == positions[[1]][i])
    
    # Add the sub-path to the total edge list.
    
    edge <- tibble(from = location$id,
                   to = n)
    
    edge_list <- bind_rows(edge_list, edge)
  }
}

# Reduce the edge list to only include those positions with known pathways.

known <- unique(c(edge_list$from, edge_list$to))

node_list <- nodes_all %>% 
  filter(id %in% known)

### Visualize all the possible job pathways!

# Strip the apostrophe in the label names so DiagrammeR can print them.

# Plot a random sample of pathways.
# edge_list_clean <- sample_n(edge_list, 50)

# Plot the most connected pathways.

most_connected <- count(edge_list, to) %>% 
  arrange(desc(n))

edge_list_clean <- edge_list
# edge_list_clean <- edge_list %>% 
#   filter(from %in% most_connected$to[1:100] | to %in% most_connected$to[1:100])

# Subset and clean the data for plotting.

node_list_clean <- node_list %>% 
  filter(id %in% edge_list_clean$from | id %in% edge_list_clean$to)

node_list_clean$label <- str_remove(node_list_clean$label, "'")

g <- create_graph(attr_theme = NULL) %>% 
  add_global_graph_attrs(attr = "overlap",
                         value = "false",
                         attr_type = "graph") %>% 
  add_global_graph_attrs(attr = "layout",
                         value = "twopi",
                         attr_type = "graph") %>% 
  add_nodes_from_table(
    table = node_list_clean,
    label_col = label) %>% 
  add_edges_from_table(
    table = edge_list_clean,
    from_col = from,
    to_col = to,
    from_to_map = id_external)
  # render_graph(layout = "neatly")

export_graph(g, 
             file_name = "plot.svg",
             file_type = "svg")

###########################
# RENDER PATHWAYS FUNCTIONS
###########################

### render_pathway()
### This function takes a position title as an argument and renders a visualization of all the possible pathways from it.

render_pathway <- function(title) {
  
  # If it has pathways associated with it.
  
  if (title %in% node_list$label) {
  
    # Find the correct id for the position title.
    
    id <- node_list %>%
      filter(label == title) %>% 
      select(id) %>% 
      as.numeric()
    
    # Select only the original pathways of interest (one step away).
    
    edge_list_subset <- edge_list %>% 
      filter(from == id)
    
    # Return a warning if there are no higher positions.
    
    if (nrow(edge_list_subset) == 0) {
      
      return ("No pathways exist.")
      
    }
    
    # Identify pathways that are further than one step away.
    
    extra_edges <- edge_list %>% 
      filter(from %in% edge_list_subset$to)
    
    # Repeat the selection a few times until we've found all the pathways.
    
    for (i in 1:5) {
    
      # Save the original extra rows.
      
      edge_list_subset <- bind_rows(edge_list_subset, extra_edges)
    
      # And identify new pathways.
      
      extra_edges <- edge_list %>% 
        filter(from %in% extra_edges$to)

    }
    
    # Remove duplicate edges just in case.
    
    edge_list_subset <- distinct(edge_list_subset)
    
    # Subset the node list based on the identified connections.
    
    node_list_subset <- node_list %>%
      filter(id %in% edge_list_subset$from | id %in% edge_list_subset$to)
    
    # Strip the apostrophe in the label names so DiagrammeR can print them.
    
    node_list_subset$label <- str_remove(node_list_subset$label, "'")
    
    # Render the pathway visualization.
    
    create_graph(attr_theme = NULL) %>% 
      add_global_graph_attrs(attr = "overlap",
                             value = "false",
                             attr_type = "graph") %>% 
      add_nodes_from_table(
        table = node_list_subset,
        label_col = label) %>% 
      add_edges_from_table(
        table = edge_list_subset,
        from_col = from,
        to_col = to,
        from_to_map = id_external) %>%
      render_graph(layout = "neat")
  }
  
  # If it has no pathways, tell the user.
  
  else (return ("No pathways exist."))
}

### render_all()
# This function takes a position title as an argument and renders a visualization of all the possible pathways both to AND from the position.

render_all <- function(title) {

  # If it has pathways associated with it.
  
  if (title %in% node_list$label) {
    
    # Find the correct id for the position title.
    
    id <- node_list %>%
      filter(label == title) %>% 
      select(id) %>% 
      as.numeric()
    
    # Select only the original pathways of interest (one step away to or from).
    
    edge_list_subset <- edge_list %>% 
      filter(from == id | to == id)
    
    # Identify pathways that are further than one step away.
    
    extra_edges <- edge_list %>% 
      filter(from %in% edge_list_subset$to | to %in% edge_list_subset$from) %>% 
      filter(from != id)
    
    # Repeat the selection a few times until we've found all the pathways.
    
    for (i in 1:5) {
      
      # Save the original extra rows.
      
      edge_list_subset <- bind_rows(edge_list_subset, extra_edges)
      
      # And identify new pathways.
      
      extra_edges <- edge_list %>% 
        filter(to %in% extra_edges$from)

    }
    
    # Remove duplicate edges just in case.
    
    edge_list_subset <- distinct(edge_list_subset)
    
    # Subset the node list based on the identified connections.
    
    node_list_subset <- node_list %>%
      filter(id %in% edge_list_subset$from | id %in% edge_list_subset$to)
    
    # Strip the apostrophe in the label names so DiagrammeR can print them.
    
    node_list_subset$label <- str_remove(node_list_subset$label, "'")
    
    # Render the pathway visualization.
    
    create_graph(attr_theme = NULL) %>% 
      add_global_graph_attrs(attr = "overlap",
                             value = "false",
                             attr_type = "graph") %>% 
      add_nodes_from_table(
        table = node_list_subset,
        label_col = label) %>% 
      add_edges_from_table(
        table = edge_list_subset,
        from_col = from,
        to_col = to,
        from_to_map = id_external) %>%
      render_graph(layout = "neat")
  }
  
  # If it has no pathways, tell the user.
  
  else (return ("No pathways exist."))
}

# Explore some random pathways.

render_pathway(dd$title[sample(nrow(dd), 1)])
render_all(dd$title[sample(nrow(dd), 1)])
  
# Test the first function for every position.

for (title in dd$title) {
  print(title)
  render_all(title)  
}

#########
# PROMOTE
#########
# promote() is a function that returns elligibility information for a promotion from a current position.
# Its arguments are a current job position and years of employment.

promote <- function(title, years) {
  
  # Get the id associated with the current position.
  
  id <- node_list %>%
    filter(label == title) %>% 
    select(id) %>% 
    as.numeric()
  
  # Identify all the immediate pathways from that id.
  
  edge_list_subset <- edge_list %>% 
    filter(from == id)
  
  # Get titles that are immediate next steps.
  
  promotions <- node_list %>% 
    filter(id %in% edge_list_subset$to) %>% 
    select(label)
  
  # Get the required promotion information.
  
  requirements <- dd %>% 
    filter(title %in% promotions$label) %>% 
    select(exam_status, salary_low, salary_high, duties, deadline, req_all, process_req, education, experience, experience_years)
  
}

############
# NEXT STEPS
############
# Figure out the promotion task and how to identify that text.
# Find what language counts as "biased" (see AMA).
# Perform analysis, make visualizations. :)