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

# Create a tibble to store all of the collected information.

storage <- tibble(title = titles[1])

# Go through each line of each job post.

for (n in 1:nrow(raw[[j]])) {
  
  # Capture the class code.
  # Add a fix to capture the class code from the job title.
  
  if (str_detect(raw[[j]][n, ], "Class Code")) {
    class_code <- str_extract(raw[[j]][n, ], "\\d\\d\\d\\d")
  }
  
  # Capture the open date.
  
  if (str_detect(raw[[j]][n, ], "Open Date")) {
    open_date <- str_extract(raw[[j]][n, ], "\\d\\d-\\d\\d-\\d\\d")
  }
  
  # Capture the exam status.
  
  if (str_detect(raw[[j]][n, ], "Exam Open to")) {
    exam_status <- str_extract(raw[[j]][n, ], ".+")
  }
  
  # Capture the salary ranges.
  
  if (str_detect(raw[[j]][n, ], "\\$")) {
    salary <- raw[[j]][n, ]
  }
  
  # Capture the duties.
  
  if (str_detect(raw[[j]][n, ], paste0("An ", titles[j])) | 
      str_detect(raw[[j]][n, ], paste0("A ", titles[j]))) {
    duties <- raw[[j]][n, ]
  }
}


#####
# SECOND ATTEMPT USING CAPITAL LETTERS
#####

# Read in all the files as one block of text each.

raw2 <- lapply(bulletins_list, read_file)

test <- stri_extract_all_charclass(raw2[[1]], "[A-Z]")
