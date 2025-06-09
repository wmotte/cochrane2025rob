#!/usr/bin/env Rscript
#
# W.M.Otte (w.m.otte@umcutrecht.nl)
#
# Download Cochrane data.
################################################################################

library( 'httr' )
library( 'tools' )
library( 'rlang' ) # for || operator

###
# Helper function for safe download
##
safe_download <- function(url, file_path, description)
    {
    tryCatch({
        message("  Downloading ", description, "...")
        
        # Set user agent to avoid blocking
        response <- GET(url, 
                        user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"),
                        timeout(30))
        
        if (status_code(response) == 200) {
            # Write content to file
            writeBin(content(response, "raw"), file_path)
            message("  ✓ ", description, " saved successfully")
            return(TRUE)
        } else {
            error_msg <- paste("HTTP", status_code(response), "for", description)
            results$errors <<- c(results$errors, error_msg)
            message("  ✗ ", error_msg)
            return(FALSE)
        }
    }, error = function(e) {
        error_msg <- paste("Error downloading", description, ":", e$message)
        results$errors <<- c(results$errors, error_msg)
        message("  ✗ ", error_msg)
        return(FALSE)
    })
}


###
# Download function
##
download_cochrane_review <- function(url_ref, url_data, outdir)
{
    # Load required libraries
    if (!require(httr, quietly = TRUE)) {
        stop("Package 'httr' is required. Please install it with: install.packages('httr')")
    }
    if (!require(tools, quietly = TRUE)) {
        stop("Package 'tools' is required.")
    }
    
    # Extract main URL by removing "/references" suffix
    main_url <- gsub("/references$", "", url_ref)
    
    # Extract review ID from URL for consistent naming
    review_id <- basename(parse_url(main_url)$path)
    if (review_id == "" || is.na(review_id)) {
        review_id <- gsub("[^A-Za-z0-9]", "_", basename(main_url))
    }
    
    # Define file paths
    main_file <- file.path(outdir, paste0(review_id, "_main.html"))
    ref_file <- file.path(outdir, paste0(review_id, "_references.html"))
    
    # Extract file extension for data file
    data_ext <- tools::file_ext(basename(parse_url(url_data)$path))
    if (data_ext == "") 
        stop( "*** ERROR ***: no extension extracted!" )
    
    data_file <- file.path(outdir, paste0(review_id, "_data.", data_ext))
    
    # Check if main file already exists (skip condition)
    if (file.exists(main_file)) {
        message("Skipping ", review_id, " - main file already exists")
        return(list(
            skipped = TRUE, 
            review_id = review_id,
            main_file = main_file,
            ref_file = ref_file,
            data_file = data_file
        ))
    }
    
    message("Processing ", review_id, "...")
    
    # Initialize results tracking
    results <- list(
        skipped = FALSE,
        review_id = review_id,
        main_file = main_file,
        ref_file = ref_file,
        data_file = data_file,
        main_success = FALSE,
        ref_success = FALSE,
        data_success = FALSE,
        errors = c()
    )
    
    # Download main site HTML
    results$main_success <- safe_download(main_url, main_file, "main site")

    # Download data file
    results$data_success <- safe_download(url_data, data_file, "data file")
    
    # If ZIP is given, references and RoB are within zip
    
    if( data_ext == "rm5" )
    {
        # Download references HTML
        results$ref_success <- safe_download(url_ref, ref_file, "references")
    }

    # Summary message
    successes <- sum(results$main_success, results$ref_success, results$data_success)
    message("Completed ", review_id, ": ", successes, "/3 files downloaded successfully")
    
    if (length(results$errors) > 0) {
        message("Errors encountered:")
        for (error in results$errors) {
            message("  - ", error)
        }
    }
    
    return(results)
}

###
# Usage function for your loop
##
process_cochrane_batch <- function(data_frame, outdir)
{
    # data_frame should have columns: url_ref, url_data
    
    if (!all(c("url_ref", "url_data") %in% names(data_frame))) {
        stop("Data frame must contain columns 'url_ref' and 'url_data'")
    }
    
    total_rows <- nrow(data_frame)
    results_list <- list()
    
    message("Starting batch processing of ", total_rows, " Cochrane reviews...")
    message("Output directory: ", outdir)
    
    for (i in 1:total_rows) {
        message("\n[", i, "/", total_rows, "] Processing row ", i)
        
        tryCatch({
            result <- download_cochrane_review(
                url_ref = data_frame$url_ref[i],
                url_data = data_frame$url_data[i],
                outdir = outdir
            )
            results_list[[i]] <- result
            
        }, error = function(e) {
            error_msg <- paste("Fatal error processing row", i, ":", e$message)
            message("✗ ", error_msg)
            results_list[[i]] <<- list(
                skipped = FALSE,
                review_id = paste0("row_", i),
                fatal_error = error_msg
            )
        })
        
        # Small delay to be respectful to the server
        Sys.sleep(0.5)
    }
    
    message("\nBatch processing completed!")
    
    # Summary statistics
    skipped <- sum(sapply(results_list, function(x) x$skipped %||% FALSE))
    fatal_errors <- sum(sapply(results_list, function(x) !is.null(x$fatal_error)))
    
    message("Summary:")
    message("  - Skipped (already downloaded): ", skipped)
    message("  - Fatal errors: ", fatal_errors)
    message("  - Attempted downloads: ", total_rows - skipped - fatal_errors)
    
    return(results_list)
}


################################################################################

# output dir
outdir <- 'out.03.cochrane.downloads'
dir.create( outdir, showWarnings = FALSE )

# get meta
df <- readr::read_tsv( 'out.02.cochrane.check.data/reviews_with_rm5_url.tsv', show_col_types = FALSE )

# data could be rm5 or zip or NA if not available
df$url_data <- df$url_rm5
df <- df[ !is.na( df$url_data ), ]
df$url_ref <- paste0( df$urls, '/references' )

# Run
results <- process_cochrane_batch( df, outdir )


