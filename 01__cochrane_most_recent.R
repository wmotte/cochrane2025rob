#!/usr/bin/env Rscript
#
# W.M. Otte (w.m.otte@umcutrecht.nl)
#
# Check duplicates
# NOTE: there should be NO duplicates in the Cochrane export.
################################################################################

library( "dplyr" )
library( "stringr" )


# output directory
outdir <- 'out.01.cochrane.most.recent'
dir.create( outdir, showWarnings = FALSE )

# read all systematic reviews
df <- read.csv( '00__cochrane_search/citation-export.csv' )
df$Abstract <- NULL

# create helper columns
df$base_id <- sub("\\.PUB.*$", "", df$Cochrane.Review.ID )
df$version <- ifelse( grepl("\\.PUB\\d+", df$Cochrane.Review.ID ), NA, 1 )
df[ is.na( df$version ), 'version' ] <- as.numeric(sub(".*\\.PUB(\\d+).*", "\\1", df[ is.na( df$version ), 'Cochrane.Review.ID' ] ) )

# First, check if you have duplicate base_ids
sum(duplicated(df$base_id))

# Method 1: Using ave() function (most efficient)
df_filtered <- df[ave(df$version, df$base_id, FUN = max) == df$version, ]

# sort
df_final <- df_filtered[ sort.int( df_filtered$base_id, index.return = TRUE )$ix, ]

# trim
df_final$Publisher <- NULL
df_final$ISSN <- NULL
df_final$Issue <- NULL
df_final$Source <- NULL

# output file
outfile <- paste0( outdir, '/reviews.tsv' )

# Write
readr::write_tsv( df_final, outfile, quote = "needed" )

