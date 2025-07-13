# get the risk of bias from the zipped data
# unzip all of the folders into "unpacked" directory
# some studies have RoB in "analysis-data" other in "study-data" folder
# (probably depends on RoB1/RoB2)
outdir     <- 'out.03.zips'
dirs       <- list.dirs(file.path(outdir, "unpacked"))
rev_names <- unique(substr(dirs, 21 + 1, 21 + 8))
rev_names <- rev_names[rev_names != ""]

RoB1_names <- c(
  "random_sequence_generation",
  "allocation_concealment",
  "blinding_of_participants_and_personnel",
  "blinding_of_outcome_assessment",
  "blinding_of_participant_personnel_outcome",
  "incomplete_outcome_data",
  "selective_outcome_reporting",
  "other_bias"
)

RoB2_names <- c(
  "bias_arising_from_the_randomization_process",
  "bias_due_to_deviations_from_intended_interventions",
  "bias_due_to_missing_outcome_data",
  "bias_in_measurement_of_the_outcome",
  "bias_in_selection_of_the_reported_result",
  "overall_bias"
)

out_RoB1  <- list()
out_RoB2  <- list()
out_RoB2b <- list()
for(i in seq_along(rev_names)) {

  rev   <- rev_names[i]
  files_ad <- list.files(file.path(outdir, "unpacked", paste0(rev, "-analysis-data")))
  files_sd <- list.files(file.path(outdir, "unpacked", paste0(rev, "-study-data")))

  # select estimates data
  file_ad <- files_ad[grepl("data-rows", files_ad)]
  file_sd <- files_sd[grepl("risk-of-bias", files_sd)]

  if(length(file_sd) == 1){

    temp_df <- read.csv(file.path(outdir, "unpacked", paste0(rev, "-study-data"), file_sd))

    if(nrow(temp_df) == 0)
      next

    temp_df$Review    <- rev

    temp_df <- temp_df[,colnames(temp_df) %in% c("Review", "Study", colnames(temp_df)[grepl("Domain..judgement", colnames(temp_df))])]

    if (!any(grepl("Domain..judgement", colnames(temp_df))))
      stop("no-data-detected (RoB1)")

    # move accidental ROB2 to the correct output
    if (any(grepl("Bias.arising", colnames(temp_df)))) {

      # RoB 2 processing
      colnames(temp_df)[grepl("Domain..judgement...Bias.arising.from.the.randomization.process", colnames(temp_df))]        <-"bias_arising_from_the_randomization_process"
      colnames(temp_df)[grepl("Domain..judgement...Bias.due.to.deviations.from.intended.interventions", colnames(temp_df))] <-"bias_due_to_deviations_from_intended_interventions"
      colnames(temp_df)[grepl("Domain..judgement...Bias.due.to.missing.outcome.data", colnames(temp_df))]                   <-"bias_due_to_missing_outcome_data"
      colnames(temp_df)[grepl("Domain..judgement...Bias.in.measurement.of.the.outcome", colnames(temp_df))]                 <-"bias_in_measurement_of_the_outcome"
      colnames(temp_df)[grepl("Domain..judgement...Bias.in.selection.of.the.reported.result", colnames(temp_df))]           <-"bias_in_selection_of_the_reported_result"
      colnames(temp_df)[grepl("Domain..judgement...Overall.bias", colnames(temp_df))]                                       <-"overall_bias"

      temp_df$Analysis.group  <- NA
      temp_df$Analysis.number <- NA
      temp_df$Analysis.name   <- NA
      temp_df$Subgroup        <- NA

      temp_df <- temp_df[,colnames(temp_df) %in% c("Review", "Analysis.group", "Analysis.number", "Analysis.name", "Subgroup", "Study", RoB2_names)]

      out_RoB2b[[rev]] <- temp_df

    } else {
      out_RoB1[[rev]] <- temp_df
    }

  }else if(length(file_ad) == 1){

    temp_df <- read.csv(file.path(outdir, "unpacked", paste0(rev, "-analysis-data"), file_ad))

    if(nrow(temp_df) == 0)
      next

    temp_df$Review    <- rev

    # RoB 2 processing
    colnames(temp_df)[grepl("Bias.arising.from.the.randomization.process..judgement.", colnames(temp_df))]        <-"bias_arising_from_the_randomization_process"
    colnames(temp_df)[grepl("Bias.due.to.deviations.from.intended.interventions..judgement.", colnames(temp_df))] <-"bias_due_to_deviations_from_intended_interventions"
    colnames(temp_df)[grepl("Bias.due.to.missing.outcome.data..judgement.", colnames(temp_df))]                   <-"bias_due_to_missing_outcome_data"
    colnames(temp_df)[grepl("Bias.in.measurement.of.the.outcome..judgement.", colnames(temp_df))]                 <-"bias_in_measurement_of_the_outcome"
    colnames(temp_df)[grepl("Bias.in.selection.of.the.reported.result..judgement.", colnames(temp_df))]           <-"bias_in_selection_of_the_reported_result"
    colnames(temp_df)[grepl("Overall.bias..judgement.", colnames(temp_df))]                                       <-"overall_bias"


    if (!any(colnames(temp_df) %in% RoB2_names))
      stop("no-data-detected (RoB2)")

    temp_df <- temp_df[,colnames(temp_df) %in% c("Review", "Analysis.group", "Analysis.number", "Analysis.name", "Subgroup", "Study", RoB2_names)]

    out_RoB2[[rev]] <- temp_df

  }
}

### format and save RoB2 ----
out_RoB2 <- do.call(rbind, out_RoB2)
colnames(out_RoB2)[colnames(out_RoB2) == "Analysis.group"]  <- "Analysis_group"
colnames(out_RoB2)[colnames(out_RoB2) == "Analysis.number"] <- "Analysis_number"
colnames(out_RoB2)[colnames(out_RoB2) == "Analysis.name"]   <- "Analysis_name"
rownames(out_RoB2) <- NULL
out_RoB2 <- out_RoB2[,c("Review", "Analysis_group", "Analysis_number", "Analysis_name", "Subgroup", "Study", RoB2_names)]
write.csv(out_RoB2, file = file.path(outdir, "RoB2.csv"), row.names = FALSE)


out_RoB2b <- do.call(rbind, out_RoB2b)
colnames(out_RoB2b)[colnames(out_RoB2b) == "Analysis.group"]  <- "Analysis_group"
colnames(out_RoB2b)[colnames(out_RoB2b) == "Analysis.number"] <- "Analysis_number"
colnames(out_RoB2b)[colnames(out_RoB2b) == "Analysis.name"]   <- "Analysis_name"
rownames(out_RoB2b) <- NULL
out_RoB2b <- out_RoB2b[,c("Review", "Analysis_group", "Analysis_number", "Analysis_name", "Subgroup", "Study", RoB2_names)]
write.csv(out_RoB2b, file = file.path(outdir, "RoB2b.csv"), row.names = FALSE)



### format and save RoB1 ----
# use the same process ad in 06_cluster_ROB
out <- list()
for(i in seq_along(out_RoB1)){

  df <- out_RoB1[[i]]
  tempColnames <- colnames(df)[!colnames(df) %in% c("Review", "Study")]

  tempVals <- df[, tempColnames]
  if (all(is.na(tempVals)))
    next

  temp_out <- list()
  for(j in 1:ncol(tempVals)){
    temp_out[[j]] <- data.frame(
      Review       = df$Review[j],
      Study        = df$Study,
      column_name  = colnames(tempVals)[j],
      column_value = tempVals[,j]
    )
  }

  out[[i]] <- do.call(rbind, temp_out)
}
cdf <- do.call(rbind, out)

cdf$column_name <- gsub("Domain..judgement...", "", cdf$column_name)
cdf$column_name <- gsub(".", "_", cdf$column_name, fixed = TRUE)


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
cdf$blinding_of_participant_personnel_outcome <-
  grepl("Blinding_performance_bias_and_detection_bias_All_outcomes", cdf$column_name, ignore.case = TRUE) |
  grepl("Blinding__performance_bias_and_detection_bias", cdf$column_name, ignore.case = TRUE)

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
cdf$id <- paste0(cdf$Review, "-", cdf$Study)


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
  temp_values$infile       <- temp$infile[1]
  temp_values$study_name   <- temp$study_name[1]

  out[[i]] <- data.frame(temp_values)
}
out <- do.call(rbind, out)

# how many studies have all RoB?
mean(apply(out[, RoB_categories], 1, function(x) all(!is.na(x))))

write.csv(out, paste0(outdir, '/scraped_rob_wide_processed.csv'), row.names = FALSE)
