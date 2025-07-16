# prepare study-level data
# rm5 input
outdir     <- 'out.03.rm5s'
files      <- list.files(file.path(outdir, "unpacked"))

# this seems to be an empty file of timed-out script?
files <- files[files != "14651858.CD001027.pub2_data.rm5"]

out <- list()
err <- list()

for(i in seq_along(files)) {

  temp_df <- try(meta::read.rm5(file.path(outdir, "unpacked", files[i])))

  if(is.null(temp_df) || (!inherits(temp_df, "try-error") && nrow(temp_df) == 0))
    next

  if(inherits(temp_df, "try-error")) {
    err[[length(err)+1]] <- list(
      file = files[i],
      error = temp_df
    )
    next
  }

  temp_df <- as.data.frame(temp_df[,!colnames(temp_df) %in% c(
    "O.E"            ,"V"              ,"TE"             ,"seTE"           ,"lower.TE"       ,"upper.TE"       ,"weight",
    "order"          ,"level"          ,
    "model"          ,"random"         ,"k"              ,"event.e.pooled",
    "n.e.pooled"     ,"event.c.pooled" ,"n.c.pooled"     ,"TE.pooled"      ,"lower.pooled"   ,"upper.pooled"   ,"level.ma",
    "weight.pooled"  ,"Z.pooled"       ,"pval.TE.pooled" ,"Q"              ,"pval.Q"         ,"I2"             ,"tau2",
    "Q.w"            ,"pval.Q.w"       ,"I2.w"           ,"swap.events"    ,"logscale"       ,"fixed"
  )])
  temp_df$infile <- files[i]

  out[[i]] <- temp_df
}
df <- do.call(rbind, out)
write.csv(df, file.path(outdir, "rm5_data.csv"), row.names = FALSE)


# csv input
outdir     <- 'out.03.zips'
dirs       <- list.dirs(file.path(outdir, "unpacked"))
rev_names <- unique(substr(dirs, 21 + 1, 21 + 8))
rev_names <- rev_names[rev_names != ""]

out <- list()
for(i in seq_along(rev_names)) {

  rev   <- rev_names[i]
  files <- list.files(file.path(outdir, "unpacked", paste0(rev, "-analysis-data")))
  file  <- files[grepl("data-rows", files)]

  temp_df <- read.csv(file.path(outdir, "unpacked", paste0(rev, "-analysis-data"), file))

  if (is.null(temp_df) || nrow(temp_df) == 0)
    next

  temp_df <- temp_df[,colnames(temp_df) %in% c(
    "Analysis.group",
    "Analysis.number",
    "Analysis.name",
    "Subgroup",
    "Study",
    "Study.year",
    "GIV.Mean",
    "GIV.SE",
    "Experimental.mean",
    "Experimental.SD",
    "Experimental.cases",
    "Experimental.N",
    "Control.mean",
    "Control.SD",
    "Control.cases",
    "Control.N",
    "O.E",
    "Variance",
    "Weight",
    "Mean",
    "CI.start",
    "CI.end",
    "Footnotes"
  )]

  temp_df$review <- rev

  out[[i]] <- temp_df
}

df <- do.call(rbind, out)
write.csv(df, file.path(outdir, "zip_data.csv"), row.names = FALSE)
