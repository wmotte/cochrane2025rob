outdir <- 'out.06.primary.vs.secondary'
files <- list.files(file.path(outdir, "processing_data"), pattern = "labeled.json")

# read the json files
out <- list()
for (file in files) {
    json_file <- file.path(outdir, "processing_data", file)
    if (file.exists(json_file)) {
        data <- jsonlite::fromJSON(json_file)
        data <- data$labeled_outcomes
        data$filename <- gsub("_labeled.json", "", file)
        out[[length(out) + 1]] <- data
    }
}
out <- do.call(rbind, out)
write.csv(out, file.path(outdir, "primary_vs_secondary_outcomes.csv"), row.names = FALSE)


# read the tcsvs
tcsv_files <- list.files(file.path(outdir, "processing_data"), pattern = "meta.tsv")
out <- list()
for (file in tcsv_files) {
    tsv_file <- file.path(outdir, "processing_data", file)
    if (file.exists(tsv_file)) {
        data <- readr::read_tsv(tsv_file, )
        data$filename <- gsub(".tsv", "", file)
        out[[length(out) + 1]] <- data
    }
}
