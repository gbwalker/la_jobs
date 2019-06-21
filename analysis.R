#################
# RECOMMENDATIONS
#################
# 1. Need an outcome variable! What does diversity look like?
#   Do an experiment. Same job, two different descriptions. Simulates an RCT.
# 2. Job postings are only one dimension of recruiting and maintaining a diverse workforce (interviews, benefits, etc.).
# 3. Fix readability. Probably same readability for college + noncollege jobs.
#   Short is better: https://storage.googleapis.com/kaggle-forum-message-attachments/530963/13211/820tips20from2035020million20job20posts.pdf
# 4. Let your values shine. If you want a diverse workforce, SAY IT. "Does not discriminate" legalese doesn't cut it.

# Language ability. LA is a diverse place! Use that to your advantage. Localize the listings.
#   https://diversity.berkeley.edu/sites/default/files/recruiting_a_more_diverse_workforce_uhs.pdf
# Don't give people so many reasons NOT to apply. Too many specific requirements.

#################################################################################

library(tidyverse)
library(stringr)
library(ngram)

##############
# ANALYZE DATA
##############
# External data: https://catalog.data.gov/dataset?publisher=data.lacity.org

# Read in the raw text and cleaned data.

raw <- read_rds("raw.rds")
dd <- read_rds("dd.rds")

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
                 fem_req = rep(NA, nrow(dd)))

for (n in 1:nrow(dd)) {

  gender$masc[n] <- sum(str_count(raw_text$text[n], m_words))
  
  gender$fem[n] <- sum(str_count(raw_text$text[n], f_words))
  
  gender$masc_req[n] <- sum(str_count(dd$req_all[n], m_words)) + sum(str_count(dd$req_notes[n], m_words))
  
  gender$fem_req[n] <- sum(str_count(dd$req_all[n], f_words)) + sum(str_count(dd$req_notes[n], f_words))
  
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


### DO READABILITY SCORES
### THEN MAYBE MAKE GROUPS??
### THEN MAKE A FEW NICE GRAPHS



#########
# OUTLINE
#########

# 1. Make gender variable.
# 2. Make readability score.
# 3. Process diversity score from the external data.


# Is diversity correlated with number of pathways?
# What makes the challenging positions different?

# Scatterplots: reading score vs. diversity
#   reading vs. possibilities
# Is process requirements related to competitiveness, pathways, education?

# scatterplot (salary on y, gender on x; color something)
# violin plot (salary on y, pathway buckets on x)

# Diverging lollipop chart for pathways? or gendered language?
# Topic modeling: once they're categorized by requirement text, do they skew along certain lines?
# Supervised text classification: https://cfss.uchicago.edu/notes/supervised-text-classification/
#   use the raw text to classify documents into categories.

# density plot of starting salaries
# 17 roles are "challenging." Do they have certain textual characteristics that others share? Can classify them...
# Certain roles have lower diversity scores. What textual characteristics do they have in common?

