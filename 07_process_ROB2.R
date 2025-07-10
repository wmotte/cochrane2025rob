# unzip all of the folders into the directory (unzip here with 7zip)
outdir <- 'out.03.zips'
dirs   <- list.dirs(outdir)
dirs   <- dirs[grepl("analysis-data", dirs)]

out <- list()
for(i in seq_along(dirs)) {

    dir   <- dirs[i]
    files <- list.files(dir)

    # select estimates data
    files <- files[grepl("data-rows", files)]

    for(j in seq_along(files)) {
        file <- files[j]
        df   <- read.csv(file.path(dir, file))

        if(nrow(df) == 0)
            next

        df$dir  <- dir
        df$file <- file
        out[[length(out) + 1]] <- df
    }
}

# fix some formatting
for(i in seq_along(out)) {
    out[[i]]$Study.year <- as.numeric(out[[i]]$Study.year)
}

# rbind all data frames into one (fill in empty columns with NA)
out <- dplyr::bind_rows(out)

# check the number of trials with RoB
table(out$Overall.bias..judgement.)
table(out$Bias.in.selection.of.the.reported.result..judgement.)
table(out$Bias.in.measurement.of.the.outcome..judgement.)
table(out$Bias.due.to.missing.outcome.data..judgement.)
table(out$Bias.due.to.deviations.from.intended.interventions..judgement.)
table(out$Bias.arising.from.the.randomization.process..judgement.)
sum(!is.na(out$Overall.bias..judgement.))
sum(!is.na(out$Bias.in.selection.of.the.reported.result..judgement.))
sum(!is.na(out$Bias.in.measurement.of.the.outcome..judgement.))
sum(!is.na(out$Bias.due.to.missing.outcome.data..judgement.))
sum(!is.na(out$Bias.due.to.deviations.from.intended.interventions..judgement.))

# number of analyses (TODO: verify that group - number results in the correct nesting)
length(unique(paste0(out$file, "-", out$Analysis.group, "-", out$Analysis.number)))
# number of analyses with RoB
length(unique(paste0(out$file, "-", out$Analysis.group, "-", out$Analysis.number)[!is.na(out$Bias.due.to.deviations.from.intended.interventions..judgement.)]))

write.csv(out, file.path(outdir, 'rob2.csv'), row.names = FALSE, na = "")


# add separate risk of bias files ("risk-of-bias")
# This is a very different formating, might it be RoB 1 again?
outdir <- 'out.03.zips'
dirs   <- list.dirs(outdir)
dirs   <- dirs[grepl("study-data", dirs)]

out <- list()
for(i in seq_along(dirs)) {

  dir   <- dirs[i]
  files <- list.files(dir)

  # select estimates data
  files <- files[grepl("risk-of-bias", files)]

  for(j in seq_along(files)) {
    file <- files[j]
    df   <- read.csv(file.path(dir, file))

    if(nrow(df) == 0)
      next

    df$dir  <- dir
    df$file <- file
    out[[length(out) + 1]] <- df
  }
}

# rbind all data frames into one (fill in empty columns with NA)
out <- dplyr::bind_rows(out)
