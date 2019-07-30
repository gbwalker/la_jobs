#################
# Submission for Kaggle's Data Science for Good: City of Los Angeles competition.
# June 2019
# Gabe Walker
# Competition: https://www.kaggle.com/c/data-science-for-good-city-of-los-angeles/overview
# Final kernel: https://www.kaggle.com/gbwalker/la-jobs-precise-pathway-mapping-bias-analysis
#################

library(tidyverse)
library(stringr)
library(lubridate)
library(DiagrammeR)
library(scales)
library(quanteda)
library(Rtsne)
library(janitor)
library(ggrepel)
library(ngram)

#################
# DATA PROCESSING
#################

### Read in the job bulletins.

# Obtain a list of the bulletin file names.

bulletins_list <- list.files("CityofLA/Job Bulletins",
                             pattern = "*.txt"
)

# Read in each raw text file.

raw <- lapply(bulletins_list, read_delim, delim = "\n")

# write_rds(raw, "raw.rds")

# Make a list of all of the job titles.
# Clean the whitespace and change to title case for ease of reading.

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

rule <- "^[\t\\sA-Z]?[A-Z][A-Z]"

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
    
    return(c(raw[[j]][(n + 1):(n + stop_point - 1), ]))
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
  
  listing <- tibble(
    file = bulletins_list[j],
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
    info = NA
  )
  
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
      listing$exam_status[1] <- as.character(raw[[j]][(n + 1), ]) %>%
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
      else {
        (listing$notice[1] <- raw[[j]][n + 1, ])
      }
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
        listing$expert[1] <- capture_header(j, n, "EXPERT") %>% 
          unlist()
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

# write_rds(df, "df.rds")

#################
# DATA DICTIONARY
#################

### Arrange the data into an easily usable format.

###################################
# Extract the initial salary range.
###################################

salaries <- tibble(
  salary_low = rep(NA, nrow(df)),
  salary_high = rep(NA, nrow(df)),
  salary_dwp = rep(NA, nrow(df))
)

for (n in 1:nrow(df)) {
  
  # Only identify salaries for fields that have some salary information listed.
  
  if (!is.na(df$salary[[n]][[1]])) {
    
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

####################################################################
### Combine the process, location, duties, and deadline information.
### Create full requirements and selection process fields.
####################################################################

misc <- tibble(
  process = rep(NA, nrow(df)),
  location = rep(NA, nrow(df)),
  duties_temp = rep(NA, nrow(df)),
  deadline_temp = rep(NA, nrow(df)),
  req_all = rep(NA, nrow(df)),
  req_notes = rep(NA, nrow(df)),
  selection_temp = rep(NA, nrow(df)),
  selection_notes = rep(NA, nrow(df))
)

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
  
  # Create separate full requirements and requirement notes fields.
  
  requirements <- paste(df$requirements[[n]], collapse = " ") %>%
    str_split("NOTES?:")
  
  misc$req_all[n] <- requirements[[1]][1] %>%
    str_trim()
  
  misc$req_notes[n] <- requirements[[1]][2] %>%
    str_trim()
}

### Combine all the cleaned information into a data dictionary.

dd <- df %>%
  bind_cols(salaries, misc) %>%
  mutate(
    exam_status = str_trim(exam_status),
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
    driver_type = NA,
    education = NA,
    education_req = NA,
    semesters = NA,
    quarters = NA,
    degree = NA,
    experience = NA,
    years = NA,
    experience_time = NA,
    pathway = NA
  ) %>%
  
  # Drop some variables, including the individual requirement fields.
  
  select(-salary, -requirements, -process_notes, -where, -deadline, -duties, -selection_process) %>%
  rename(
    deadline = deadline_temp,
    duties = duties_temp,
    selection = selection_temp
  )

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
  
  # Add an additional field for driver's license type.
  
  if (!is.na(dd$license[n]) &
      str_detect(dd$license[n], "driver") &
      str_detect(dd$req_all[n], "Class")) {
    class <- str_extract(dd$req_all[n], "Class [\\d/]?[ABC][\\s,]+?[ABC]?\\s?(or)?( Class )?[ABC]?")
    
    # If it is not in the main requirements, check the notes.
    
    if (is.na(class)) {
      class <- str_extract(dd$req_notes[n], "Class [\\d/]?[ABC][\\s,]+?[ABC]?\\s?(or)?( Class )?[ABC]?")
    }
    
    # Clean up the text and save it to a blank item.
    
    class <- class %>%
      str_remove("Class")
    
    final <- ""
    
    if (str_detect(class, "A")) {
      final <- "A"
    }
    if (str_detect(class, "B")) {
      final <- paste0(final, "B")
    }
    if (str_detect(class, "C")) {
      final <- paste0(final, "C")
    }
    
    # If it's still blank, reassign NA.
    
    if (final == "") {
      final <- NA
    }
    
    dd$driver_type[n] <- final
  }
  
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
  if (str_detect(dd$req_all[n], "[Mm]aster'?s degree")) {
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
    dd$education_req[n] <- str_extract(dd$req_all[n], "[Cc]ompletion of [\\d\\w\\s,:-]+")
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
      
      dd$semesters[n] <- case_when(
        semesters == "three" ~ 3,
        semesters == "four" ~ 4,
        semesters == "sixty" ~ 60,
        semesters == "ninety" ~ 90,
        TRUE ~ dd$semesters[n]
      )
    }
    
    dd$quarters[n] <- str_extract(dd$education_req[n], "\\d?\\d quarter") %>%
      str_remove(" quarter") %>%
      as.numeric()
    
    # If the field contains text rather than a number, convert the text to a number.
    
    if (is.na(dd$quarters[n])) {
      quarters <- str_extract(dd$education_req[n], "\\w+ quarter") %>%
        str_remove(" quarter")
      
      dd$quarters[n] <- case_when(
        quarters == "three" ~ 3,
        quarters == "four" ~ 4,
        quarters == "sixty" ~ 60,
        quarters == "ninety" ~ 90,
        TRUE ~ dd$quarters[n]
      )
    }
  }
  
  ### Get all the possible experience lengths.
  
  # Reset the experience counter.
  
  exp <- NA
  
  # Capture content when "year of full/part-time" or years are mentioned in the requirements.
  
  if (str_detect(dd$req_all[n], "years? of ") |
      str_detect(dd$req_all[n], "years? as ")) {
    exp <- str_extract_all(dd$req_all[n], "[\\w]+ (\\(\\d+\\) )?years? [\\w\\s',-]+[;.]?")
    
    for (i in 1:length(exp[[1]])) {
      dd$experience[n] <- paste(dd$experience[n], exp[[1]][i], sep = "/")
    }
  }
  
  # If the position requires months instead, find the number of months.
  
  if (is.na(dd$experience[n]) &
      str_detect(dd$req_all[n], "months of ")) {
    exp <- str_extract_all(dd$req_all[n], "[\\w]+ (\\(\\d+\\) )?months [\\w\\s',-]+[;.]?")
    
    for (i in 1:length(exp[[1]])) {
      dd$experience[n] <- paste(dd$experience[n], exp[[1]][i], sep = "/")
    }
  }
  
  # If the experience field is missing, add all the requirements.
  
  if (is.na(dd$experience[n])) {
    dd$experience[n] <- dd$req_all[n]
  }
  
  # Clean up the experience variable.
  
  dd$experience[n] <- str_remove(dd$experience[n], "NA/") %>%
    str_trim() %>%
    str_squish()
  
  # Pull out the number of years or months required for each experience line.
  
  for (i in 1:length(exp[[1]])) {
    
    # If the unit is years.
    
    if (!is.na(exp[[1]][i]) &
        str_detect(exp[[1]][i], "years? ")) {
      t <- str_extract(exp[[1]][i], "[\\w]+") %>%
        tolower()
      
      # Translate the word into a number.
      
      years <- case_when(
        t == "one" ~ 1,
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
        TRUE ~ NA_real_
      )
      
      # Assign it to a variable.
      
      dd$years[n] <- paste(dd$years[n], years, sep = "/")
    }
    
    # Do the same for months (i.e., if the above script caught no years).
    
    if (!is.na(exp[[1]][i]) &
        is.na(dd$years[n])) {
      t <- str_extract(exp[[1]][i], "[\\w]+") %>%
        tolower()
      
      months <- case_when(
        t == "three" ~ .25,
        t == "six" ~ .5,
        t == "eight" ~ .75,
        t == "eighteen" ~ 1.5,
        TRUE ~ NA_real_
      )
      
      # Catch a few special cases.
      
      months <- case_when(
        str_detect(dd$req_all[n], "[Ee]ighteen") ~ 1.5,
        str_detect(dd$req_all[n], "[Ee]ighteen months of ") ~ 1.5,
        str_detect(dd$req_all[n], "18 months of ") ~ 1.5,
        str_detect(dd$req_all[n], "6 months of ") ~ .5,
        TRUE ~ months
      )
      
      # Add the number to the list.
      
      dd$years[n] <- paste(dd$years[n], months, sep = "/")
    }
  }
  
  # Clean up the years variable if it exists.
  
  if (!is.na(dd$years[n])) {
    dd$years[n] <- str_remove(dd$years[n], "NA/NA") %>%
      str_remove("NA/") %>%
      str_remove("/NA") %>%
      str_remove("NA") %>%
      str_remove("^/")
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
      
      else {
        (title_salary <- title_salary$salary_low[1])
      }
      
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
  
  # Fix the Cement Finisher error.
  
  if ((!is.na(dd$pathway[n]) & !is.na(dd$title[n])) &
      (dd$pathway[n] == "Cement Finisher" &
       dd$title[n] == "Cement Finisher Worker")) {
    
    dd$pathway[n] <- NA
    
  }
}


### Save the data dictionary version.

# write_rds(dd, "dd.rds")

##########
# PATHWAYS
##########
### Visualize promotional pathways for all positions.

# First create a node list and edge list for DiagrammeR to use.

nodes_all <- tibble(
  id = 1:nrow(dd),
  label = dd$title,
  shape = "rectangle"
)

# Remove duplicate nodes (in this case the duplicated file are "Chief Clerk Police" and "Senior Utility Services Specialist").
# Make a list of them to remove from the edge list.

duplicated_nodes <- nodes_all[duplicated(nodes_all$label), ]

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
    
    edge <- tibble(
      from = location$id,
      to = n,
      color = "black"
    )
    
    edge_list <- bind_rows(edge_list, edge)
  }
}

# Reduce the node list to only include those positions with known pathways.

known <- unique(c(edge_list$from, edge_list$to))

node_list <- nodes_all %>%
  filter(id %in% known)

# And remove pathways to duplicated nodes that were removed.

edge_list <- edge_list %>%
  filter(!from %in% duplicated_nodes$id) %>%
  filter(!to %in% duplicated_nodes$id)

### Visualize all the possible job pathways!

# Strip the apostrophe in the label names so DiagrammeR can print them.

# Plot a random sample of pathways.
# edge_list_clean <- sample_n(edge_list, 50)

# Plot the most connected pathways.

most_connected <- count(edge_list, to) %>%
  arrange(desc(n))

edge_list_clean <- edge_list

# Use this code to only map the most connected points.

# edge_list_clean <- edge_list %>%
#   filter(from %in% most_connected$to[1:25] | to %in% most_connected$to[1:25])

# Subset and clean the data for plotting.

node_list_clean <- node_list %>%
  filter(id %in% edge_list_clean$from | id %in% edge_list_clean$to)

node_list_clean$label <- str_remove(node_list_clean$label, "'")

g <- create_graph(attr_theme = NULL) %>%
  add_global_graph_attrs(
    attr = "overlap",
    value = "false",
    attr_type = "graph"
  ) %>%
  add_global_graph_attrs(
    attr = "layout",
    value = "twopi",
    attr_type = "graph"
  ) %>%
  add_nodes_from_table(
    table = node_list_clean,
    label_col = label
  ) %>%
  add_edges_from_table(
    table = edge_list_clean,
    from_col = from,
    to_col = to,
    from_to_map = id_external
  )

###########################
# RENDER PATHWAYS FUNCTIONS
###########################

### render_pathway()
### This function takes a position title as an argument and renders a visualization of all the possible pathways above it.

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
      return("No pathways exist.")
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
      add_global_graph_attrs(
        attr = "overlap",
        value = "false",
        attr_type = "graph"
      ) %>%
      add_nodes_from_table(
        table = node_list_subset,
        label_col = label
      ) %>%
      add_edges_from_table(
        table = edge_list_subset,
        from_col = from,
        to_col = to,
        from_to_map = id_external
      ) %>%
      render_graph(layout = "neat")
  }
  
  # If it has no pathways, tell the user.
  
  else {
    (return("No pathways exist."))
  }
}

### render_pathway_color() does the same thing as render_pathway() but with colored arrows!

render_pathway_color <- function(title, results) {
  
  # If it has pathways associated with it.
  
  if (title %in% node_list$label) {
    
    # Find the correct id for the position title.
    
    id <- node_list %>%
      filter(label == title) %>%
      select(id) %>%
      as.numeric()
    
    # Select only the original pathways of interest (one step away).
    
    edge_list_subset <- results %>%
      select(from, to, color)
    
    # Return a warning if there are no higher positions.
    
    if (nrow(edge_list_subset) == 0) {
      return("No pathways exist.")
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
      add_global_graph_attrs(
        attr = "overlap",
        value = "false",
        attr_type = "graph"
      ) %>%
      add_nodes_from_table(
        table = node_list_subset,
        label_col = label
      ) %>%
      add_edges_from_table(
        table = edge_list_subset,
        from_col = from,
        to_col = to,
        from_to_map = id_external
      ) %>%
      render_graph(layout = "neat")
  }
  
  # If it has no pathways, tell the user.
  
  else {
    (return("No pathways exist."))
  }
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
    
    extra_edges_above <- edge_list %>%
      filter(from %in% edge_list_subset$to)
    
    extra_edges_below <- edge_list %>%
      filter(to %in% edge_list_subset$from) %>%
      filter(from != id)
    
    # Repeat the selection a few times until we've found all the pathways.
    
    for (i in 1:5) {
      
      # Save the original extra rows.
      
      edge_list_subset <- bind_rows(edge_list_subset, extra_edges_above, extra_edges_below)
      
      # And identify new pathways.
      
      extra_edges_above <- edge_list %>%
        filter(from %in% extra_edges_above$to)
      
      extra_edges_below <- edge_list %>%
        filter(to %in% extra_edges_below$from) %>%
        filter(from != id)
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
      add_global_graph_attrs(
        attr = "overlap",
        value = "false",
        attr_type = "graph"
      ) %>%
      add_nodes_from_table(
        table = node_list_subset,
        label_col = label
      ) %>%
      add_edges_from_table(
        table = edge_list_subset,
        from_col = from,
        to_col = to,
        from_to_map = id_external
      ) %>%
      render_graph(layout = "neat")
  }
  
  # If it has no pathways, tell the user.
  
  else {
    (return("No pathways exist."))
  }
}

### count_pathways()
# This function tallies the total number of potential pathways from a given position.

count_pathways <- function(title) {
  
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
      return(0)
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
    
    # Return the total number of possible nodes minus the given position.
    
    return(nrow(node_list_subset) - 1)
  }
  
  # If it has no pathways, tell the user.
  
  else {
    (return(0))
  }
}

# Count the number of promotional possibilities for every position.

possibilities <- tibble(possibilities = rep(NA, nrow(dd)))

for (n in 1:nrow(possibilities)) {
  possibilities$possibilities[n] <- count_pathways(dd$title[n])
}

# Add it to the data dictionary and save a copy.

dd <- bind_cols(dd, possibilities)

# write_rds(dd, "dd.rds")

### Explore some random pathways.

# render_pathway(dd$title[sample(nrow(dd), 1)])
# render_all(dd$title[sample(nrow(dd), 1)])

test <- function() {
  title <- dd$title[sample(nrow(dd), 1)]
  print(title)
  render_all(title)
}

# Test the first function for every position.

# for (title in dd$title) {
#  print(title)
#  render_all(title)
# }


#########
# PROMOTE
#########
# promote() is a function that returns eligibility information for a promotion from a current position.
# Its arguments are a current job position and years of employment.

promote <- function(title, years) {
  
  # Get the id associated with the current position.
  
  id <- node_list %>%
    filter(label == title) %>%
    select(id) %>%
    as.numeric()
  
  # Catch the error if years is invalid.
  
  if (!is.numeric(years) |
      years <= 0) {
    return("Please enter a valid number of years.")
  }
  
  # Identify all the immediate pathways from that id.
  
  edge_list_subset <- edge_list %>%
    filter(from == id)
  
  # Catch errors if there are no pathways associated with the role.
  
  if (is.na(edge_list_subset$from[1])) {
    return("There are no promotional pathways associated with that role.")
  }
  
  # Get titles that are immediate next steps.
  
  promotions <- node_list %>%
    filter(id %in% edge_list_subset$to) %>%
    select(label)
  
  # Initialize a results list to keep track of the changing arrow colors. Make the default red (ineligible).
  
  results <- node_list %>%
    filter(label %in% promotions$label) %>%
    rename(to = id) %>%
    left_join(edge_list_subset, by = "to") %>%
    mutate(color = "red")
  
  # Get the required promotion information.
  
  requirements <- dd %>%
    filter(title %in% promotions$label) %>%
    select(title, exam_status, salary_low, salary_high, duties, deadline, req_all, process_req, education, experience, years)
  
  # Eliminate duplicated titles from the position list.
  
  requirements <- requirements[!duplicated(requirements$title), ]
  
  for (n in 1:nrow(requirements)) {
    
    # Identify the necessary information for a promotion for each higher position.
    
    exp <- str_split(requirements$experience[n], "/")
    
    y <- str_split(requirements$years[n], "/")
    
    # Set the default qualification to "not qualified."
    
    qualified <- FALSE
    
    # Iterate through each possible experience requirement.
    
    for (i in 1:length(exp[[1]])) {
      
      # Identify the required number of years, and set it to 0 if it's not found.
      
      required_years <- as.numeric(y[[1]][i])
      
      if (is.na(required_years)) {
        required_years <- 0
      }
      
      # Check if the years of experience and title are enough.
      
      if (years >= required_years &
          str_detect(exp[[1]][i], title)) {
        qualified <- TRUE
        
        results$color[n] <- "forestgreen"
      }
    }
    
    # If the person has enough years but their position is not mentioned in any of the lines.
    
    if (years >= required_years &
        !str_detect(paste(exp[[1]], collapse = " "), title) &
        !qualified) {
      cat(paste0(
        "You are POTENTIALLY qualified to apply for the ", requirements$title[n], " position at this time.",
        "\n",
        "\n"
      ))
      
      results$color[n] <- "yellow"
    }
    
    # Otherwise, make a clear distinction of eligibility.
    
    else {
      (
        
        cat(paste0(
          "You ", ifelse(qualified, "ARE ", "are NOT "),
          "qualified to apply for the ", requirements$title[n], " position at this time.",
          "\n",
          "\n"
        ))
      )
    }
    
    # Explain the specific requirements.
    
    cat(paste0(
      "The specific requirements are: ", "\n",
      str_squish(str_remove(str_trim(str_replace_all(requirements$req_all[n], "/", " ")), "; (or)?(and)?.?$")),
      "\n",
      "\n"
    ))
    
    # If other information is available, print that too.
    
    if (!is.na(requirements$process_req[n])) {
      cat(paste0("Application requirements: ", requirements$process_req[n]))
      
      if (!is.na(requirements$education[n])) {
        cat(paste0(" and ", requirements$education[n]))
      }
      
      cat(paste0(".", "\n"))
    }
    
    if (!is.na(requirements$exam_status[n])) {
      cat(paste0("Exam status: ", requirements$exam_status[n], ".", "\n"))
    }
    
    if (!is.na(requirements$salary_low[n])) {
      cat(paste0("The salary range begins at ", dollar(requirements$salary_low[n])))
      
      if (!is.na(requirements$salary_high[n])) {
        cat(paste0(" to ", dollar(requirements$salary_high[n])))
      }
      
      cat(paste0(".", "\n", "\n"))
    }
  }
  
  # Generate a pathway graph based on the title.
  
  return(render_pathway_color(title, results))
}

# Test the promote() function.

test_promote <- function() {
  title <- dd$title[sample(nrow(dd), 1)]
  years <- sample(1:10, 1)
  print(title)
  print(years)
  promote(title, years)
}

# Test every title.

# for (t in dd$title) {
#  promote(t, 5)
# }

#######################
# TEST PROMOTE FUNCTION
#######################

# This function interactively reports the number and percentage of correct results from the promote function.
# Longest run (tested so far) is 100% accuracy for 25 positions (excluding positions without promotional pathways).

test_promote_tally <- function() {
  response <- ""
  
  correct <- 0
  incorrect <- 0
  
  while (response != "e") {
    
    # Get a test result.
    
    sample <- test_promote()
    
    print(sample)
    
    # Get the user's response.
    
    response <- readline("Correct? y/n/s to skip/e to exit. ")
    
    # Save the result.
    
    if (response == "y") {
      correct <- correct + 1
    }
    
    if (response == "n") {
      incorrect <- incorrect + 1
    }
  }
  
  return(paste0(
    correct, " correct, ", incorrect, " incorrect for a total accuracy of ",
    round((correct / (correct + incorrect))),
    "."
  ))
}

##############
# ANALYZE DATA
##############
# This corresponds to the final part of the writeup (analysis).

# External data: https://catalog.data.gov/dataset?publisher=data.lacity.org

# Challenging roles to fill.
# In the future: IT ("Applications Programmer"), Wastewater, Inspector, Journey-level.

challenging_roles <- c(
  "Accountant",
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
  "Tree Surgeon"
)

#################
# GENDER VARIABLE
#################
# Test of gendered language from: https://github.com/lovedaybrooke/gender-decoder/blob/master/app/wordlists.py

f_words = c(
  "agree",
  "affectionate",
  "child",
  "cheer",
  "collab",
  "commit",
  "communal",
  "compassion",
  "connect",
  "considerate",
  "cooperat",
  "co-operat",
  "depend",
  "emotiona",
  "empath",
  "feel",
  "flatterable",
  "gentle",
  "honest",
  "interpersonal",
  "interdependen",
  "interpersona",
  "inter-personal",
  "inter-dependen",
  "inter-persona",
  "kind",
  "kinship",
  "loyal",
  "modesty",
  "nag",
  "nurtur",
  "pleasant",
  "polite",
  "quiet",
  "respon",
  "sensitiv",
  "submissive",
  "support",
  "sympath",
  "tender",
  "together",
  "trust",
  "understand",
  "warm",
  "whin",
  "enthusias",
  "inclusive",
  "yield", 
  "share", 
  "sharin")

m_words = c(
  "active",
  "adventurous",
  "aggress",
  "ambitio",
  "analy",
  "assert",
  "athlet",
  "autonom",
  "battle",
  "boast",
  "challeng",
  "champion",
  "compet",
  "confident",
  "courag",
  "decid",
  "decision",
  "decisive",
  "defend",
  "determin",
  "domina",
  "dominant",
  "driven",
  "fearless",
  "fight",
  "force",
  "greedy",
  "head-strong",
  "headstrong",
  "hierarch",
  "hostil",
  "impulsive",
  "independen",
  "individual",
  "intellect",
  "lead",
  "logic",
  "objective",
  "opinion",
  "outspoken",
  "persist",
  "principle",
  "reckless",
  "self-confiden",
  "self-relian",
  "self-sufficien",
  "selfconfiden",
  "selfrelian",
  "selfsufficien",
  "stubborn",
  "superior",
  "unreasonab"
)

# Collapse the raw files into long strings.

raw_text <- tibble(text = rep(NA, length(raw)))

for (n in 1:length(raw)) {
  
  text <- paste(unlist(raw[n]), collapse = " ")
  
  raw_text$text[n] <- text
  
}

# Initialize a gender dataframe and fill it with the word counts from each list.

gender <- tibble(masc = rep(NA, nrow(dd)),
                 fem = rep(NA, nrow(dd)),
                 masc_req = rep(NA, nrow(dd)),
                 fem_req = rep(NA, nrow(dd)),
                 gendered = rep(NA, nrow(dd)))

for (n in 1:nrow(dd)) {
  
  gender$masc[n] <- sum(str_count(raw_text$text[n], m_words))
  
  gender$fem[n] <- sum(str_count(raw_text$text[n], f_words))
  
  gender$masc_req[n] <- sum(str_count(dd$req_all[n], m_words)) + sum(str_count(dd$req_notes[n], m_words))
  
  gender$fem_req[n] <- sum(str_count(dd$req_all[n], f_words)) + sum(str_count(dd$req_notes[n], f_words))
  
  gender$gendered[n] = gender$masc[n] + gender$fem[n]
  
}


# Add it to dd and make a masculine "ratio" score (ratio of masculine to feminine words).

dd_all <- dd %>% 
  bind_cols(gender) %>% 
  mutate(m_ratio = masc / fem,
         m_ratio_req = masc_req / fem_req)

############
# LEGIBILITY
############

# Create word count and readability scores.

legibility <- tibble(word_count = rep(NA, nrow(raw_text)),
                     req_count = rep(NA, nrow(dd)))

for (n in 1:nrow(raw_text)) {
  
  legibility$word_count[n] <- wordcount(raw_text$text[n])
  
  legibility$req_count[n] <- wordcount(dd$req_all[n]) + wordcount(dd$req_notes[n])
  
}

# Calculate mean sentence lengths for each document.

sentence_length <- textstat_readability(raw_text$text, 
                                        measure = "meanSentenceLength")

# Calculate Flesch-Kincaid Readability Scores.

fk_score <- textstat_readability(raw_text$text, 
                                 measure = "Flesch.Kincaid")

### Create a feature co-occurrence matrix for each document.

# Set a list of common words to ignore.

ignore <- c(stopwords(), "may", "city", "los", "angeles", "candidates", "applicants")

# First start with a tokenizer and document feature matrix.

tokens <- tokens(raw_text$text, 
                 remove_numbers = TRUE,
                 remove_punct = TRUE,
                 remove_symbols = TRUE,
                 remove_url = TRUE)

dfm_all <- dfm(tokens,
               verbose = TRUE,
               remove = ignore)

# Reduce the DFM to only include words that occur more than five times.

dfm_small <- dfm_trim(dfm_all,
                      min_termfreq = 5)

# Reduce the dimensionality down to two dimensions for plotting on an x/y axis.

dfm_reduced <- Rtsne(as.matrix(dfm_small))

# Bind all the text analysis results together.

dd_all <- dd_all %>% 
  mutate(sentence_length = sentence_length$meanSentenceLength,
         fk_score = fk_score$Flesch.Kincaid,
         x = dfm_reduced$Y[,1],
         y = dfm_reduced$Y[,2])


###########
# DIVERSITY
###########

# Read in the external data about applicant diversity.

applicants <- read_csv("../input/la-data/Job_Applicants_by_Gender_and_Ethnicity.csv") %>% 
  clean_names() %>% 
  select(-fiscal_year, -job_number)

# Make a list of the titles.

titles <- applicants$job_description %>%
  str_extract(pattern = "^...[-\\s\\A-Z\\_a-z]+") %>%
  str_trim() %>%
  str_to_title() %>%
  str_replace(" And ", " and ") %>%
  str_replace(" Of ", " of ") %>%
  str_replace(" _ ", " and ") %>%
  str_replace("Sr ", "Senior ") %>%
  str_replace("s_", "s'")

# Add it to the applicants list.

applicants <- applicants %>% 
  mutate(title = titles) %>% 
  select(-job_description) %>% 
  
  # Add columns about applicant diversity.
  # mf_ratio is the ratio of known male to female applicants.
  # diversity is the ratio of non-caucasian applicants to caucasian ones.
  
  mutate(mf_ratio = male/female,
         diversity = (black + hispanic + asian + american_indian_alaskan_native + filipino)/caucasian ) %>% 
  
  # Only select the columns of interest.
  
  select(title, mf_ratio, diversity)

# Save it to the full dataset.

dd_all <- dd_all %>% 
  left_join(applicants, by = "title")

# Save the results.

# write_rds(dd_all, "dd_all.rds")


########
# GRAPHS
########

# Create a dataframe just with necessary graphing information for ease of use.

ddg <- dd_all %>%
  select(title, 
         salary_low, 
         process_req, 
         license, 
         education, 
         years, 
         possibilities, 
         gendered,
         m_ratio, 
         sentence_length, 
         fk_score, 
         x, 
         y, 
         mf_ratio, 
         diversity) %>% 
  mutate(college = case_when(str_detect(education, "college") ~ 1,
                             TRUE ~ 0),
         interview = case_when(str_detect(process_req, "interview") ~ 1,
                               TRUE ~ 0),
         test = case_when(str_detect(process_req, "test") ~ 1,
                          TRUE ~ 0),
         essay = case_when(str_detect(process_req, "essay") ~ 1,
                           TRUE ~ 0)) %>% 
  mutate(challenge = case_when(title %in% challenging_roles ~ 1,
                               TRUE ~ 0)) %>% 
  mutate(challenge = factor(challenge),
         college = factor(college),
         interview = factor(interview),
         test = factor(test),
         essay = factor(essay))

# Fix infinite and NaN values.

m_ratio_fix <- ddg %>% 
  filter(!is.nan(m_ratio) & !is.infinite(m_ratio))

mf_ratio_fix <- ddg %>% 
  filter(!is.na(mf_ratio) & !is.infinite(mf_ratio))

diversity_fix <- ddg %>% 
  filter(!is.na(diversity) & !is.infinite(diversity))

ddg <- ddg %>% 
  mutate(m_ratio = case_when(is.nan(m_ratio) ~ 0,
                             is.infinite(m_ratio) ~ max(m_ratio_fix$m_ratio),
                             TRUE ~ m_ratio),
         mf_ratio = case_when(is.infinite(mf_ratio) ~ max(mf_ratio_fix$mf_ratio),
                              TRUE ~ mf_ratio),
         diversity = case_when(is.infinite(diversity) ~ max(diversity_fix$diversity),
                               TRUE ~ diversity))

# Remove duplicate name values.

ddg <- ddg[!duplicated(ddg$title), ]

# And subsets for challenging roles and ones with diversity information.

ddg_challenge <- ddg %>% 
  filter(title %in% challenging_roles)

ddg_diversity <- ddg %>% 
  filter(!is.na(diversity))

# Graph 1.

ddg %>% 
  mutate(label = case_when(gendered >= 40 | fk_score >= 19.5 ~ title,
                           gendered == 0 | fk_score <= 12.5 ~ title,
                           TRUE ~ "")) %>% 
  ggplot(aes(x = fk_score, y = gendered, alpha = .5), ) +
  geom_point(col = "gray") +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  geom_label_repel(aes(label = label),
                   size = 2.5,
                   box.padding = .5,
                   point.padding = .5,
                   force = 100,
                   segment.size = .2,
                   segment.colour = "gray",
                   label.size = NA) + 
  labs(
    x = "Flesch-Kincaid readability score",
    y = "Number of gendered words",
    title = "1. More challenging readability is associated with more \n gendered language."
  ) +
  theme(
    axis.title.y = element_text(color = "gray50"),
    axis.title.y.right = element_text(color = "skyblue4"),
    panel.background = element_blank(),
    panel.grid = element_line(color = "gray90"),
    axis.title.x = element_text(color = "gray50"),
    text = element_text(size = 14),
    legend.position = "none"
  )

# Graph 2.

ddg %>% 
  mutate(possibilities_bin = case_when(possibilities == 0 ~ "Low",
                                       possibilities > 0 & possibilities < 10 ~ "Medium",
                                       possibilities >= 10 ~ "High")) %>% 
  mutate(possibilities_bin = factor(possibilities_bin, levels = c("High", "Medium", "Low"))) %>% 
  ggplot(aes(x = possibilities_bin, y = salary_low, fill = possibilities_bin, col = possibilities_bin, alpha = .5)) +
  geom_violin() +
  labs(
    x = "Job mobility (promotional pathways)",
    y = "Starting salary",
    title = "2. Lower-paying positions have more \n advancement potential."
  ) +
  scale_y_continuous(labels = dollar_format()) +
  theme(
    axis.title.y = element_text(color = "gray50"),
    axis.title.y.right = element_text(color = "skyblue4"),
    panel.background = element_blank(),
    panel.grid = element_line(color = "gray90"),
    axis.title.x = element_text(color = "gray50"),
    text = element_text(size = 14),
    legend.position = "none"
  )

# Graph 3.

ddg %>% 
  mutate(label = case_when(possibilities >= 36 ~ title,
                           TRUE ~ "")) %>% 
  ggplot(aes(x = x, y = y, alpha = possibilities, color = possibilities > 15)) +
  geom_point() +
  stat_ellipse(linetype = 2) +
  labs(
    x = "Text dimension 1",
    y = "Text dimension 2",
    title = "3. Positions with more mobility share textual similarities."
  ) +
  geom_label_repel(aes(label = label),
                   size = 2.5,
                   box.padding = .5,
                   point.padding = .5,
                   force = 100,
                   segment.size = .2,
                   segment.colour = "gray",
                   label.size = NA) + 
  theme(
    axis.title.y = element_text(color = "gray50"),
    axis.title.y.right = element_text(color = "skyblue4"),
    panel.background = element_blank(),
    panel.grid = element_line(color = "gray90"),
    axis.title.x = element_text(color = "gray50"),
    text = element_text(size = 14),
    legend.position = "none"
  )

# Graph 4.

ddg %>% 
  mutate(label = case_when(salary_low <= 40000 & m_ratio == 16 ~ title,
                           salary_low >= 175000 & m_ratio <= 3 ~ title,
                           TRUE ~ "")) %>% 
  
  ggplot(aes(x = salary_low, y = m_ratio, alpha = possibilities)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_label_repel(aes(label = label),
                   alpha = .75,
                   size = 2.5,
                   box.padding = .5,
                   point.padding = .5,
                   force = 100,
                   segment.size = .2,
                   segment.colour = "gray",
                   label.size = NA) +
  scale_x_continuous(labels = dollar_format()) +
  labs(
    x = "Starting salary",
    y = "Ratio of masculine to feminine words.",
    title = "4. Lower-level positions have more masculine text \n on average."
  ) +
  theme(
    axis.title.y = element_text(color = "gray50"),
    axis.title.y.right = element_text(color = "skyblue4"),
    panel.background = element_blank(),
    panel.grid = element_line(color = "gray90"),
    axis.title.x = element_text(color = "gray50"),
    text = element_text(size = 14),
    legend.position = "none"
  )

# Graph 5.

ddg %>% 
  mutate(label = case_when(challenge == 1 & abs(x) > 10 ~ title,
                           TRUE ~ "")) %>% 
  
  ggplot(aes(x = x, y = y, alpha = .5)) +
  geom_point(col = "gray") +
  geom_label_repel(aes(label = label),
                   col = "blue",
                   alpha = .75,
                   size = 2.5,
                   box.padding = .5,
                   point.padding = .5,
                   force = 20,
                   segment.size = .2,
                   segment.colour = "gray",
                   label.size = NA) +
  geom_point(data = ddg_challenge, 
             mapping = aes(x = x, y = y, alpha = .5), col = "blue", inherit.aes = FALSE) +
  labs(
    x = "Text dimension 1",
    y = "Text dimension 2",
    title = "5. Only some hard-to-fill roles share \n textual similarities."
  ) +
  theme(
    axis.title.y = element_text(color = "gray50"),
    axis.title.y.right = element_text(color = "skyblue4"),
    panel.background = element_blank(),
    panel.grid = element_line(color = "gray90"),
    axis.title.x = element_text(color = "gray50"),
    text = element_text(size = 14),
    legend.position = "none"
  )


# Graph 6.

ddg %>% 
  mutate(label = case_when(challenge == 1 & fk_score > 16 ~ title,
                           challenge == 1 & gendered > 15 ~ title,
                           TRUE ~ "")) %>% 
  
  ggplot(aes(x = fk_score, y = gendered, alpha = .5)) +
  geom_point(col = "gray") +
  geom_label_repel(aes(label = label),
                   col = "blue",
                   alpha = .75,
                   size = 2.5,
                   box.padding = .5,
                   point.padding = .5,
                   force = 40,
                   segment.size = .2,
                   segment.colour = "gray",
                   label.size = NA) +
  geom_point(data = ddg_challenge, mapping = aes(x = fk_score, y = gendered, alpha = .5), col = "blue", inherit.aes = FALSE) +
  geom_smooth(data = ddg_challenge, method = "lm", se = FALSE) +
  labs(
    x = "Flesch-Kincaid readability score",
    y = "Number of gendered words",
    title = "6. Challenging roles to fill also display the same pattern \n in gendered/difficult language."
  ) +
  theme(
    axis.title.y = element_text(color = "gray50"),
    axis.title.y.right = element_text(color = "skyblue4"),
    panel.background = element_blank(),
    panel.grid = element_line(color = "gray90"),
    axis.title.x = element_text(color = "gray50"),
    text = element_text(size = 14),
    legend.position = "none"
  )


# Graph 7.

ddg %>% 
  mutate(label = case_when(challenge == 1 & m_ratio > 5 ~ title,
                           challenge == 1 & salary_low > 75000 ~ title,
                           TRUE ~ "")) %>% 
  
  ggplot(aes(x = salary_low, y = m_ratio, alpha = .5)) +
  geom_point(col = "gray") +
  geom_label_repel(aes(label = label),
                   col = "blue",
                   alpha = .75,
                   size = 2.5,
                   box.padding = .5,
                   point.padding = .5,
                   force = 30,
                   segment.size = .2,
                   segment.colour = "gray",
                   label.size = NA) +
  geom_smooth(data = ddg, method = "lm", se = FALSE, col = "gray") +
  geom_point(data = ddg_challenge, mapping = aes(x = salary_low, y = m_ratio, alpha = .5), col = "blue", inherit.aes = FALSE) +
  geom_smooth(data = ddg_challenge, method = "lm", se = FALSE) +
  scale_x_continuous(labels = dollar_format()) +
  labs(
    x = "Starting salary",
    y = "Ratio of masculine to feminine words.",
    title = "7. Hard-to-fill roles have a higher ratio of masculine \n words for a given position."
  ) +
  theme(
    axis.title.y = element_text(color = "gray50"),
    axis.title.y.right = element_text(color = "skyblue4"),
    panel.background = element_blank(),
    panel.grid = element_line(color = "gray90"),
    axis.title.x = element_text(color = "gray50"),
    text = element_text(size = 14),
    legend.position = "none"
  )

# Graph 8.

ddg_diversity %>% 
  mutate(label = case_when(log(diversity) > 2.85 ~ title,
                           salary_low > 150000 ~ title,
                           TRUE ~ "")) %>% 
  
  ggplot(aes(x = salary_low, y = log(diversity), alpha = .35)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_label_repel(aes(label = label),
                   size = 2.5,
                   box.padding = .5,
                   point.padding = .5,
                   force = 100,
                   segment.size = .2,
                   segment.colour = "gray",
                   label.size = NA) + 
  scale_x_continuous(labels = dollar_format()) +
  labs(
    x = "Starting Salary",
    y = "Diversity (log)",
    title = "8. Positions with higher salaries have a less diverse \n applicant pool."
  ) +
  theme(
    axis.title.y = element_text(color = "gray50"),
    axis.title.y.right = element_text(color = "skyblue4"),
    panel.background = element_blank(),
    panel.grid = element_line(color = "gray90"),
    axis.title.x = element_text(color = "gray50"),
    text = element_text(size = 14),
    legend.position = "none"
  )

# Graph 9.

ddg_diversity %>% 
  mutate(label = case_when(log(diversity) > 3 & log(possibilities) < -.5 ~ title,
                           log(diversity) < -.5 & log(possibilities) < -.5 ~ title,
                           log(possibilities) > 2.25 ~ title,
                           TRUE ~ "")) %>% 
  
  ggplot(aes(x = log(possibilities), y = log(diversity), alpha = .35)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_label_repel(aes(label = label),
                   size = 2.5,
                   box.padding = .5,
                   point.padding = .5,
                   force = 100,
                   segment.size = .2,
                   segment.colour = "gray",
                   label.size = NA) + 
  labs(
    x = "Mobility (log)",
    y = "Diversity (log)",
    title = "9. Positions with more mobility have a more diverse \n applicant pool."
  ) +
  theme(
    axis.title.y = element_text(color = "gray50"),
    axis.title.y.right = element_text(color = "skyblue4"),
    panel.background = element_blank(),
    panel.grid = element_line(color = "gray90"),
    axis.title.x = element_text(color = "gray50"),
    text = element_text(size = 14),
    legend.position = "none"
  )

# Graph 10.

ddg %>% 
  mutate(label = case_when(log(diversity) > 2.65 ~ title,
                           TRUE ~ "")) %>% 
  
  ggplot(aes(x = x, y = y, alpha = .5)) +
  geom_point(col = "gray") +
  geom_label_repel(aes(label = label),
                   col = "red",
                   alpha = .75,
                   size = 2.5,
                   box.padding = .5,
                   point.padding = .5,
                   force = 30,
                   segment.size = .2,
                   segment.colour = "gray",
                   label.size = NA) +
  geom_point(data = ddg_diversity, mapping = aes(x = x, y = y, alpha = diversity), col = "blue", inherit.aes = FALSE) +
  theme(
    axis.title.y = element_text(color = "gray50"),
    axis.title.y.right = element_text(color = "skyblue4"),
    panel.background = element_blank(),
    panel.grid = element_line(color = "gray90"),
    axis.title.x = element_text(color = "gray50"),
    text = element_text(size = 14),
    legend.position = "none"
    # legend.title = element_blank(),
    # legend.key = element_blank()
  ) +
  labs(
    x = "Text dimension 1",
    y = "Text dimension 2",
    title = "10. Listings corresponding to more diverse applicant pools \n have few textual similarities."
  )

