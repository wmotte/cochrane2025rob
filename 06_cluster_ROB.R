outdir <- 'out.04.rob.scraper'
df <- readr::read_tsv( paste0( outdir, '/scraped_rob.tsv' ), quote = "needed" )

head(df)
length(unique(colnames(df)))

# transform wide to long (this actually takes very long, use the already processed file)
out <- list()
tempColnames <- colnames(df)[3:ncol(df)]
for(i in 1:nrow(df)){

  tempVals <- df[i, 3:ncol(df)]
  if (all(is.na(tempVals)))
    next

  out[[i]] <- data.frame(
    infile       = df$infile[i],
    study_name   = df$study_name[i],
    column_name  = tempColnames[!is.na(tempVals)],
    column_value = tempVals[!is.na(tempVals)]
  )
}
cdf <- do.call(rbind, out)

# remove all rows not containing: "Unclear risk" / "Low risk" / "High risk"
cdf <- cdf[cdf$column_value %in% c("Unclear risk", "Low risk", "High risk"), ]

write.csv(cdf, paste0(outdir, '/scraped_rob_long.csv'), row.names = FALSE)
cdf <- read.csv(paste0(outdir, '/scraped_rob_long.csv'), stringsAsFactors = FALSE)

### Risk of Bias categories:
# Random sequence generation (Selection bias)
# Allocation concealment (Selection bias)
# Blinding of participants and personnel (Performance bias)
# Blinding of outcome assessment (Detection bias)
# Incomplete outcome data (Attrition bias)
# Selective outcome reporting (Reporting bias)
# Other bias

cdf$random_sequence_generation <-
  grepl("Random_sequence_generation", cdf$column_name, ignore.case = TRUE) |
  grepl("Randomisation_method_reported", cdf$column_name, ignore.case = TRUE) |
  grepl("Sequence_generation", cdf$column_name, ignore.case = TRUE) |
  grepl("Adequate_sequence_generation", cdf$column_name, ignore.case = TRUE)


cdf$allocation_concealment <-
  grepl("Allocation_concealment", cdf$column_name, ignore.case = TRUE) |
  grepl("Knowledge_of_allocated_intervention", cdf$column_name, ignore.case = TRUE) |
  grepl("Knowledge_of_the_allocation", cdf$column_name, ignore.case = TRUE)

cdf$blinding_of_participants_and_personnel <-
  grepl("Blinding_of_participants", cdf$column_name, ignore.case = TRUE) |
  grepl("Blinding_of_participants", cdf$column_name, ignore.case = TRUE) |
  grepl("Masking_of_participants", cdf$column_name, ignore.case = TRUE) |
  grepl("Blinding_of_care_provider", cdf$column_name, ignore.case = TRUE) |
  grepl("Blinding_of_caregiver", cdf$column_name, ignore.case = TRUE) |
  grepl("Blinding_of_personnel", cdf$column_name, ignore.case = TRUE) |
  grepl("Blinding_of_participant", cdf$column_name, ignore.case = TRUE) |
  grepl("Blinding_of_provider", cdf$column_name, ignore.case = TRUE) |
  grepl("Blinding_of_doctors_personnel", cdf$column_name, ignore.case = TRUE)


cdf$blinding_of_outcome_assessment <-
  grepl("Blinding_of_outcome_assessment", cdf$column_name, ignore.case = TRUE) |
  grepl("Blinding_of_outcome_assessors", cdf$column_name, ignore.case = TRUE) |
  grepl("Masking_of_outcome_assessment", cdf$column_name, ignore.case = TRUE) |
  grepl("Masking_of_outcome_assessors", cdf$column_name, ignore.case = TRUE) |
  grepl("Blinding_of_assessment", cdf$column_name, ignore.case = TRUE) |
  grepl("Blinding_of_the_Outcome_assessor", cdf$column_name, ignore.case = TRUE) |
  grepl("Blinding_Outcome_assessor", cdf$column_name, ignore.case = TRUE) |
  grepl("Blinding_of_radiological_outcomes", cdf$column_name, ignore.case = TRUE) |
  grepl("Blinding_of_clinical_outcomes", cdf$column_name, ignore.case = TRUE) |
  grepl("Blinding_of_clinical_outcomes", cdf$column_name, ignore.case = TRUE)


# many studies perform both blinding of participants and personnel and blinding of outcome assessment in a single item...
cdf$blinding_of_participant_personnel_outcome <- grepl("Blinding_performance_bias_and_detection_bias_All_outcomes", cdf$column_name, ignore.case = TRUE)

cdf$incomplete_outcome_data <-
  grepl("Incomplete_outcome_data", cdf$column_name, ignore.case = TRUE) |
  grepl("Loss_of_clusters", cdf$column_name, ignore.case = TRUE) |
  grepl("Incomplete_follow_up", cdf$column_name, ignore.case = TRUE) |
  grepl("Intention_to_treat_analysis", cdf$column_name, ignore.case = TRUE) |
  grepl("Participants_analysed_in_group_allocated", cdf$column_name, ignore.case = TRUE) |
  grepl("ITT_analysis", cdf$column_name, ignore.case = TRUE) |
  grepl("Intention_to_treat", cdf$column_name, ignore.case = TRUE) |
  grepl("Incomplete_outcome_assessment", cdf$column_name, ignore.case = TRUE)

cdf$selective_outcome_reporting <-
  grepl("Selective_outcome_reporting", cdf$column_name, ignore.case = TRUE) |
  grepl("Bias_in_selection_of_the_reported", cdf$column_name, ignore.case = TRUE) |
  grepl("Selective_reporting", cdf$column_name, ignore.case = TRUE)
# Selection_of_the_non_exposed_cohort

cdf$other_bias <- grepl("Other_bias", cdf$column_name, ignore.case = TRUE)


table(cdf$random_sequence_generation)
table(cdf$allocation_concealment)
table(cdf$blinding_of_participants_and_personnel)
table(cdf$blinding_of_outcome_assessment)
table(cdf$blinding_of_participant_personnel_outcome)
table(cdf$incomplete_outcome_data)
table(cdf$selective_outcome_reporting)
table(cdf$other_bias)

# check which columns are missing
cdf$unknown <- apply(cdf[,c(
  "random_sequence_generation",
  "allocation_concealment",
  "blinding_of_participants_and_personnel",
  "blinding_of_outcome_assessment",
  "blinding_of_participant_personnel_outcome",
  "incomplete_outcome_data",
  "selective_outcome_reporting",
  "other_bias"
)], 1, function(x) all(!x))
round(table(cdf$unknown) / length(cdf$unknown), 3)

# sort the missing columns by frequency -- update the name dispatching
sort(table(cdf$column_name[cdf$unknown]))

# this one could be both the "blinding_of_outcome_assessment" or "blinding_of_participants_and_personnel"
# (especially since there is "performance_bias_and_detection_bias")
# Blinding_performance_bias_and_detection_bias_All_outcomes
# From the frequencies, it seems like that it contains both biases simultaneously
# we could assign both categories, but we keep them separate because we cannot be sure

# verify that only one category was assigned
cdf$number_cat <- apply(cdf[,c(
  "random_sequence_generation",
  "allocation_concealment",
  "blinding_of_participants_and_personnel",
  "blinding_of_outcome_assessment",
  "blinding_of_participant_personnel_outcome",
  "incomplete_outcome_data",
  "selective_outcome_reporting",
  "other_bias"
)], 1, function(x) sum(x))
table(cdf$number_cat) # only 0/1 parallel categories assigned = correct
write.csv(cdf, paste0(outdir, '/scraped_rob_long_processed.csv'), row.names = FALSE)


# transform long to wide and clean up the table a bit
cdf$column_RoB <- NA
cdf$column_RoB[cdf$random_sequence_generation]                <- "Random_sequence_generation"
cdf$column_RoB[cdf$allocation_concealment]                    <- "Allocation_concealment"
cdf$column_RoB[cdf$blinding_of_participants_and_personnel]    <- "Blinding_of_participants_and_personnel"
cdf$column_RoB[cdf$blinding_of_outcome_assessment]            <- "Blinding_of_outcome_assessment"
cdf$column_RoB[cdf$blinding_of_participant_personnel_outcome] <- "Blinding_of_participant_personnel_outcome"
cdf$column_RoB[cdf$incomplete_outcome_data]                   <- "Incomplete_outcome_data"
cdf$column_RoB[cdf$selective_outcome_reporting]               <- "Selective_outcome_reporting"
cdf$column_RoB[cdf$other_bias]                                <- "Other_bias"
table(cdf$column_RoB)

# pilot from long to wide, aggregate multiple ratings from the same
# category based on RoB1 rules - inheriting the worst rating
cdf <- cdf[!is.na(cdf$column_RoB),]
cdf$id <- paste0(cdf$infile, "-", cdf$study_name)


RoB_categories <- c(
  "Random_sequence_generation",
  "Allocation_concealment",
  "Blinding_of_participants_and_personnel",
  "Blinding_of_outcome_assessment",
  "Blinding_of_participant_personnel_outcome",
  "Incomplete_outcome_data",
  "Selective_outcome_reporting",
  "Other_bias"
)

out <- list()
for(i in seq_along(unique(cdf$id))) {

  temp <- cdf[cdf$id == unique(cdf$id)[i], ]

  # if there are multiple ratings for the same category, take the worst one
  temp        <- temp[!duplicated(temp$column_RoB), ]
  temp_values <- split(temp$column_value, temp$column_RoB)

  if(any(lengths(temp_values) > 1)) {
    # if there are multiple values for the same category, take the worst one
    temp_values <- lapply(temp_values, function(x) {
      if(length(x) > 1) {
        if(any(x == "High risk")) {
          return("High risk")
        } else if(any(x == "Unclear risk")) {
          return("Unclear risk")
        } else if(any(x == "Low risk")) {
          return("Low risk")
        } else {
          return(NA)
        }
      } else {
        return(x)
      }
    })
  }

  # fill in missing categories with NA
  temp_values[RoB_categories[!RoB_categories %in% names(temp_values)]] <- NA
  # match the order in RoB_categories
  temp_values <- temp_values[RoB_categories]

  # add remaining information
  temp_values$Review  <- temp$Review[1]
  temp_values$Study   <- temp$Study[1]

  out[[i]] <- data.frame(temp_values)
}
out <- do.call(rbind, out)

write.csv(out, paste0(outdir, '/scraped_rob_wide_processed.csv'), row.names = FALSE)
