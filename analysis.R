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
library(quanteda)
library(Rtsne)
library(janitor)
library(ggrepel)
library(scales)

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

applicants <- read_csv("Job_Applicants_by_Gender_and_Ethnicity.csv") %>% 
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

write_rds(dd_all, "dd_all.rds")


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


### Charts

# 1. Higher reading score (challenging) is correlated with more gendered language.
# Address both at the same time. The "worst" posts are both gendered and complex.

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
    title = "More challenging readability is associated with more \n gendered language."
  ) +
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
  )


# 2. There is a clear tradeoff between number of possibilities and compensation, because
# lower-level jobs have more advancement potential.
# Make the pathways clear, and you'll get more applicants for less well-compensated positions.

# ggplot(ddg, aes(x = log(possibilities), y = salary_low, alpha = .5)) +
#   geom_point()

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
    title = "Lower-paying positions have more \n advancement potential."
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
    # legend.title = element_blank(),
    # legend.key = element_blank()
  )


# 3. Lower-level positions share textual similarities.

ddg %>% 
  mutate(label = case_when(possibilities >= 36 ~ title,
                           TRUE ~ "")) %>% 
ggplot(aes(x = x, y = y, alpha = possibilities, color = possibilities > 15)) +
  geom_point() +
  stat_ellipse(linetype = 2) +
  labs(
    x = "Text dimension 1",
    y = "Text dimension 2",
    title = "Positions with more mobility share textual similarities."
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
    # legend.title = element_blank(),
    # legend.key = element_blank()
  )


# 4. Lower-level positions are not actually more biased...
# So if you want to fix gendered words, it's a different battle.
# Target could be: show pathways for lower-tier jobs; fight gendered wording for jobs that require a test...

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
    title = "Lower-level positions have more masculine text \n on average."
  ) +
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
  )

# 5. LOWER male-biased wording for higher-paid jobs.
# ggplot(ddg, aes(x = salary_low, y = m_ratio, alpha = .5)) +
#   geom_point(col = "gray") +
#   geom_smooth(data = ddg, method = "lm", se = FALSE)


### Challenging roles.

# 1. There aren't recognizable textual similarities of challenging roles.
# So it's quite feasible that the job posting itself is not an issue in filling them

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
                   force = 100,
                   segment.size = .2,
                   segment.colour = "gray",
                   label.size = NA) +
  geom_point(data = ddg_challenge, 
             mapping = aes(x = x, y = y, alpha = .5), col = "blue", inherit.aes = FALSE) +
  labs(
    x = "Text dimension 1",
    y = "Text dimension 2",
    title = "More challenging roles do not share textual similarities."
  ) +
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
  )


# 2. There is still a pattern of higher illegibility and gendered language.

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
                   force = 100,
                   segment.size = .2,
                   segment.colour = "gray",
                   label.size = NA) +
  geom_point(data = ddg_challenge, mapping = aes(x = fk_score, y = gendered, alpha = .5), col = "blue", inherit.aes = FALSE) +
  geom_smooth(data = ddg_challenge, method = "lm", se = FALSE) +
  labs(
    x = "Flesch-Kincaid readability score",
    y = "Number of gendered words",
    title = "Challenging roles to fill also display the same pattern \n in gendered/difficult language."
  ) +
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
  )


# 3. They have relatively lower gendered language. That confirms that they're "cleaner" already.

# ggplot(ddg, aes(x = possibilities, y = gendered, alpha = .5)) +
#   geom_point(col = "gray") +
#   geom_point(data = ddg_challenge, 
#              mapping = aes(x = possibilities, y = gendered, alpha = .5), col = "blue", inherit.aes = FALSE)

# 4. HIGHER male bias for higher-paid jobs. This is the opposite result than on average.

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
                   force = 100,
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
  title = "Challenging roles have a higher ratio of masculine words for a given position."
) +
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
  )

### Diverse positions.

# 1. There are more diverse applicants to lower-paying jobs and ones with more possibilities.

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
    title = "Positions with higher salaries have a less diverse \n applicant pool."
  ) +
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
  )

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
    title = "Positions with more mobility have a more diverse \n applicant pool."
  ) +
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
  )



# 2. Less diverse applicants to jobs with higher reading score.
# This does NOT mean causation: could be that more diverse applicants cluster here...
# 
# ggplot(ddg_diversity, aes(x = fk_score, y = log(diversity), alpha = .35)) +
#   geom_point() +
#   geom_smooth(method = "lm", se = FALSE) +
#   labs(
#     x = "Flesch-Kincaid readability score",
#     y = "Diversity (log)",
#     title = "Position listings that are more challenging to read \n have a less diverse applicant pool."
#   ) +
#   theme(
#     axis.title.y = element_text(color = "gray50"),
#     axis.title.y.right = element_text(color = "skyblue4"),
#     panel.background = element_blank(),
#     panel.grid = element_line(color = "gray90"),
#     axis.title.x = element_text(color = "gray50"),
#     text = element_text(size = 14),
#     legend.position = "none"
#     # legend.title = element_blank(),
#     # legend.key = element_blank()
#   )

# 3. Jobs with more male applicants are less diverse.

# ggplot(ddg_diversity, aes(x = mf_ratio, y = diversity)) +
#   geom_point() +
#   geom_smooth(method = "lm", se = FALSE)

# 4. Jobs with more diverse applicants do not have textual similarities.
# Text is not where you win people over...

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
                   force = 60,
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
    title = "Listings corresponding to more diverse applicant pools \n do not have textual similarities."
  )



#########
# OUTLINE
#########

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

# 17 roles are "challenging." Do they have certain textual characteristics that others share? Can classify them...
# Certain roles have lower diversity scores. What textual characteristics do they have in common?
