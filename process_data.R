library(tidyverse)

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

titles <- map_chr(raw, names)

###
# Group information in the job files that are under the same heading.
###

# Iterate through all of the job listings for j in 1:length(raw)

# Create a tibble to store all of the collected information.

storage <- tibble(title = titles[1])

# Go through each line of each job post.

for (n in 1:nrow(raw[[1]])) {
  
  # Capture the class code.
  
  if (str_detect(raw[[1]][n, ], "Class Code")) {
    class_code <- str_extract(raw[[1]][n, ], "\\d\\d\\d\\d")
  }
  
  # Capture the open date.
  
  if (str_detect(raw[[1]][n, ], "Open Date")) {
    open_date <- str_extract(raw[[1]][n, ], "\\d\\d-\\d\\d-\\d\\d")
  }
  
  # Capture the exam status.
  
  if (str_detect(raw[[1]][n, ], "Exam Open to")) {
    exam_status <- str_extract(raw[[1]][n, ], ".+")
  }
  
  # Capture variables in all caps...
  
  
}

