# -----------------------------------------------------------------------------
# SCRIPT: TED Talk Language Analysis
#
# DESCRIPTION: This script performs text analysis on TED Talk transcripts.
# It processes raw transcripts, calculates word counts for 'agency' and
# 'communion' using a dictionary, and runs a series of
# negative binomial regression models (glm.nb) to explore the relationship
# between these linguistic features, speaker gender, and year.
#
# OSF: This script is documented for sharing on the Open Science Framework.
# -----------------------------------------------------------------------------


## 0. SETUP: LOAD LIBRARIES
# -------------------------
# Load all required packages once at the beginning.
library(MetBrewer)       # For color palettes
library(quanteda)      # For text analysis (corpus, tokens, dfm)
library(dplyr)         # For data manipulation (mutate, group_by, summarise, case_when)
library(readr)         # For reading CSV files (read_csv)
library(stringr)       # For string manipulation (str_to_lower, str_remove_all)
library(textstem)      # For lemmatization (not used in final DFM but loaded)
library(ggplot2)       # For plotting
library(patchwork)     # For combining plots
library(interactions)  # For sim_slopes()
library(jtools)        # For summ(), plot_model()
library(ggeffects)     # For plotting model effects
library(MASS)          # For glm.nb()
library(emmeans)       # For calculating estimated marginal means


## 1. DATA INGEST & PREPROCESSING
# ---------------------------------
# Load the dictionary file
my_dict <- dictionary(file = "TED/a_AgencyCommunion.dic")

# Load the raw transcript data (only needs to be done once)
expanded_word_counts <- read_csv("TED/expanded_word_counts.csv")

# Clean the transcript text field:
# - Convert to lowercase
# - Remove parenthetical content (e.g., "(Applause)")
# - Remove hashtags
# - Remove all punctuation
# - Remove extra whitespace
ted_cleaned <- expanded_word_counts %>%
  mutate(transcript_clean = transcript %>%
           str_to_lower() %>%
           str_remove_all("\\(.*?\\)") %>%
           str_remove_all("#\\S+") %>%
           str_remove_all("[[:punct:]]") %>%
           str_squish())


## 2. QUANTEDA TEXT ANALYSIS
# ---------------------------
# Create a quanteda corpus object from the cleaned text
ted_corpus <- corpus(ted_cleaned, text_field = "transcript_clean")

# Tokenize the corpus
corpus_tokens <- quanteda::tokens(ted_corpus)

# Create a Document-Feature Matrix (DFM)
dfm_transcripts <- dfm(corpus_tokens)

# Look up dictionary words and get counts
dfm_dict_counts <- dfm_lookup(dfm_transcripts, dictionary = my_dict)

# Convert the DFM of counts to a data.frame
word_counts_df <- convert(dfm_dict_counts, to = "data.frame")

# Combine the original data with the new word counts
# We use the original 'expanded_word_counts' to ensure all docvars are kept
transcripts_with_counts <- cbind(expanded_word_counts, word_counts_df)


## 3. FEATURE ENGINEERING
# ------------------------
# These exploratory summaries are commented out as they are not used later
# df_summary_agency <- transcripts_with_counts %>%
#   group_by(Gender) %>%
#   summarise(total_count = sum(agency))
#
# df_summary_communion <- transcripts_with_counts %>%
#   group_by(Gender) %>%
#   summarise(total_count = sum(communion))

# Create scaled (z-scored) versions of agency/communion for use as predictors
# Note: 'scale()' returns a matrix, so we use 'as.numeric()'
transcripts_with_counts$agentic <- as.numeric(scale(transcripts_with_counts$agency))
transcripts_with_counts$communal <- as.numeric(scale(transcripts_with_counts$communion))

# Create STopic (Super Topic) groupings using dplyr::case_when for readability
transcripts_with_counts$STopic <- case_when(
  transcripts_with_counts$dominant_topic %in% c('1','3','5','6','8','16') ~ "Natural Sciences",
  transcripts_with_counts$dominant_topic %in% c('12','15') ~ "Well-Being",
  transcripts_with_counts$dominant_topic %in% c('10','14') ~ "Identity and Justice",
  transcripts_with_counts$dominant_topic %in% c('18','19') ~ "Environmental Sciences",
  transcripts_with_counts$dominant_topic %in% c('7','11','9') ~ "Social Sciences",
  transcripts_with_counts$dominant_topic %in% c('4','13') ~ "Performing Arts",
  TRUE ~ "Culture" # Default for all other topics
)

# Check the distribution of new topics

print(table(transcripts_with_counts$STopic))


## 4. EXPLORATORY ANALYSIS
# -------------------------

# Plot the relationship
# FIX: Corrected typo from 'agenic' to 'agentic'
plot(transcripts_with_counts$agentic, transcripts_with_counts$communal,
     xlab = "Agentic frequency (scaled)", ylab = "Communal frequency (scaled)")


## 5. TOPIC-SPECIFIC MODELING
# ----------------------------
# We run a series of models for each of the 7 topics.
# Models explore predictors of 'agency', 'communion', and 'like_count'.

# ---
# Topic 1: Environmental Sciences
# ---
print("--- STARTING ANALYSIS: Environmental Sciences ---")
# BUGFIX: This was originally 'Natural', subsetting "EVS".
# The redundant 'EVS' section later is now removed.
EVS_data <- subset(transcripts_with_counts, transcripts_with_counts$STopic == "Environmental Sciences")

# Model 1.1: Agency as outcome
model_evs_ag <- glm.nb(agency ~ year * Gender + fame_google + duration, data = EVS_data)
summary(model_evs_ag)
plot_model(model_evs_ag, type = "int")
sim_slopes(model_evs_ag, pred = "year", modx = "Gender", digits = 4)

# Model 1.2: Communion as outcome
model_evs_co <- glm.nb(communion ~ year * Gender + fame_google + duration, data = EVS_data)
summary(model_evs_co)
plot_model(model_evs_co, type = "int")
sim_slopes(model_evs_co, pred = "year", modx = "Gender", digits = 4)

# Model 1.3: Like Count (Base)
model_evs_lk <- glm.nb(like_count ~ Gender * year + fame_google + duration, data = EVS_data)
summary(model_evs_lk)
# Example of using emmeans (Estimated Marginal Means)
mean_year_evs <- mean(EVS_data$year)
predicted_likes_evs <- emmeans(
  model_evs_lk,
  specs = ~ Gender,
  at = list(year = mean_year_evs),
  type = "response"
)
print(predicted_likes_evs)
sim_slopes(model_evs_lk, pred = "year", modx = "Gender", digits = 4)

# Model 1.4: Like Count (Communion Interaction)
model_evs_lk_co <- glm.nb(like_count ~ Gender * communion * year + agency + fame_google + duration, data = EVS_data)
summary(model_evs_lk_co)
sim_slopes(model_evs_lk_co, pred = "year", modx = "Gender", mod2 = "communion", digits = 4)

# Model 1.5: Like Count (Agency Interaction)
model_evs_lk_ag <- glm.nb(like_count ~ Gender * agency * year + communion + fame_google + duration, data = EVS_data)
summary(model_evs_lk_ag)
sim_slopes(model_evs_lk_ag, pred = "year", modx = "Gender", mod2 = "agency", digits = 4)


# ---
# Topic 2: Well-Being
# ---
print("--- STARTING ANALYSIS: Well-Being ---")
WB_data <- subset(transcripts_with_counts, transcripts_with_counts$STopic == "Well-Being")

# Model 2.1: Agency as outcome
model_wb_ag <- glm.nb(agency ~ year * Gender + fame_google + duration, data = WB_data)
summary(model_wb_ag)
plot_model(model_wb_ag, type = "int")
sim_slopes(model_wb_ag, pred = "year", modx = "Gender", digits = 4)

# Model 2.2: Communion as outcome
model_wb_co <- glm.nb(communion ~ year * Gender + fame_google + duration, data = WB_data)
summary(model_wb_co)
plot_model(model_wb_co, type = "int")
sim_slopes(model_wb_co, pred = "year", modx = "Gender", digits = 4)


# ---
# Topic 3: Identity and Justice
# ---
print("--- STARTING ANALYSIS: Identity and Justice ---")
ID_data <- subset(transcripts_with_counts, transcripts_with_counts$STopic == "Identity and Justice")

# Model 3.1: Agency as outcome
model_id_ag <- glm.nb(agency ~ year * Gender + fame_google + duration, data = ID_data)
summary(model_id_ag)
plot_model(model_id_ag, type = "int")
sim_slopes(model_id_ag, pred = "year", modx = "Gender", digits = 4)

# Model 3.2: Communion as outcome
model_id_co <- glm.nb(communion ~ year * Gender + fame_google + duration, data = ID_data)
summary(model_id_co)
plot_model(model_id_co, type = "int")
sim_slopes(model_id_co, pred = "year", modx = "Gender", digits = 4)


# ---
# Topic 4: Social Sciences
# ---
print("--- STARTING ANALYSIS: Social Sciences ---")
# BUGFIX: Changed subset from "SS" to "Social Sciences"
SS_data <- subset(transcripts_with_counts, transcripts_with_counts$STopic == "Social Sciences")

# Model 4.1: Agency as outcome
model_ss_ag <- glm.nb(agency ~ year * Gender + fame_google + duration, data = SS_data)
summary(model_ss_ag)
plot_model(model_ss_ag, type = "int")
sim_slopes(model_ss_ag, pred = "year", modx = "Gender", digits = 4)

# Model 4.2: Communion as outcome
model_ss_co <- glm.nb(communion ~ year * Gender + fame_google + duration, data = SS_data)
summary(model_ss_co)
plot_model(model_ss_co, type = "int")
sim_slopes(model_ss_co, pred = "year", modx = "Gender", digits = 4)


# ---
# Topic 5: Culture
# ---
print("--- STARTING ANALYSIS: Culture ---")
# BUGFIX: Changed subset from "C" to "Culture"
C_data <- subset(transcripts_with_counts, transcripts_with_counts$STopic == "Culture")

# Model 5.1: Agency as outcome
model_c_ag <- glm.nb(agency ~ year * Gender + fame_google + duration, data = C_data)
summary(model_c_ag)
plot_model(model_c_ag, type = "int")
sim_slopes(model_c_ag, pred = "year", modx = "Gender", digits = 4)

# Model 5.2: Communion as outcome
model_c_co <- glm.nb(communion ~ year * Gender + fame_google + duration, data = C_data)
summary(model_c_co)
plot_model(model_c_co, type = "int")
sim_slopes(model_c_co, pred = "year", modx = "Gender", digits = 4)


# ---
# Topic 6: Performing Arts
# ---
print("--- STARTING ANALYSIS: Performing Arts ---")
# BUGFIX: Changed subset from "PA" to "Performing Arts"
PA_data <- subset(transcripts_with_counts, transcripts_with_counts$STopic == "Performing Arts")

# Model 6.1: Agency as outcome
model_pa_ag <- glm.nb(agency ~ year * Gender + fame_google + duration, data = PA_data)
summary(model_pa_ag)
plot_model(model_pa_ag, type = "int")
sim_slopes(model_pa_ag, pred = "year", modx = "Gender", digits = 4)

# Model 6.2: Communion as outcome
model_pa_co <- glm.nb(communion ~ year * Gender + fame_google + duration, data = PA_data)
summary(model_pa_co)
plot_model(model_pa_co, type = "int")
sim_slopes(model_pa_co, pred = "year", modx = "Gender", digits = 4)

# ---
# Topic 7: Natural Sciences
# ---
# Note: This is the 7th topic, not to be confused with "Environmental Sciences"
print("--- STARTING ANALYSIS: Natural Sciences ---")
NS_data <- subset(transcripts_with_counts, transcripts_with_counts$STopic == "Natural Sciences")

# Model 7.1: Agency as outcome
model_ns_ag <- glm.nb(agency ~ year * Gender + fame_google + duration, data = NS_data)
summary(model_ns_ag)
plot_model(model_ns_ag, type = "int")
sim_slopes(model_ns_ag, pred = "year", modx = "Gender", digits = 4)

# Model 7.2: Communion as outcome
model_ns_co <- glm.nb(communion ~ year * Gender + fame_google + duration, data = NS_data)
summary(model_ns_co)
plot_model(model_ns_co, type = "int")
sim_slopes(model_ns_co, pred = "year", modx = "Gender", digits = 4)

