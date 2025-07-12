#!/usr/bin/env Rscript
#
# W.M. Otte (w.m.otte@umcutrecht.nl)
#
###################################
library( 'xml2' )
library( 'meta' )
library( 'jsonlite' )
library( 'tibble' )
library( 'dplyr' )

# outdir
outdir <- 'processing_data'
dir.create( outdir, showWarnings = FALSE )

#################################
#### PRIMARY / SECONDARY TEXTS ##
#################################
# get combined html texts
raw <- readr::read_tsv( '../out.05.outcomes.scraper/scraped_outcomes.tsv', show_col_types = FALSE )

# only keep complete data
df <- na.omit( raw )

html_infiles <- df$infile

i <- 1

# loop over 4060 files
#for( html_infile in html_infiles[ 1:20 ] )
for( html_infile in html_infiles[ html_infiles ] )
{
    print( i )
    i <- i + 1
    
    # select singe review
    review_name <- gsub( '_main.html', '', html_infile )
    
    # get raw text data
    text_data <- df[ df$infile == html_infile, ]
    
    # write primary/secondary html information to output file
    outfile <- paste0( outdir, '/', gsub( 'html', 'tsv', html_infile ) )
    readr::write_tsv( text_data, outfile, quote = 'all' )
    
    ##########################
    ######## RM5 #############
    ##########################
    # Updated to look for .rm5.gz files
    rm5_gz_infile <- paste0( '../out.03.rm5s/', gsub( "_main.html", "_data.rm5.gz", html_infile ) )
    
    if( !file.exists( rm5_gz_infile ) )
    {
        print( paste0( "*** WARNING: FILE NOT THERE ***", rm5_gz_infile ) )    
        next
    }
    
    # use tryCatch to handle potential errors and continue the loop
    tryCatch({
        
        rmm <- NULL
        
        # Create temporary file for decompressed rm5
        temp_rm5 <- tempfile(fileext = ".rm5")
        
        # Decompress the gzipped file to temporary location
        gunzip_command <- paste0("gunzip -c '", rm5_gz_infile, "' > '", temp_rm5, "'")
        system(gunzip_command)
        
        # Check if decompression was successful
        if( !file.exists( temp_rm5 ) || file.size( temp_rm5 ) == 0 )
        {
            print( paste0( "*** WARNING: DECOMPRESSION FAILED ***", rm5_gz_infile ) )
            return()
        }
        
        # Try with xml2's recovery mode as default as some rm5 are not properly structured
        xml_content <- xml2::read_xml( temp_rm5, options = "RECOVER")
        
        # Write the recovered XML to a new file
        temp_recovered <- tempfile(fileext = ".rm5")
        write_xml(xml_content, temp_recovered)
        
        # Try reading with meta package (this is where the original error occurred)
        rmm <- meta::read.rm5(file = temp_recovered, numbers.in.labels = FALSE)
        
        # Clean up temporary decompressed file
        unlink(temp_rm5)
        
        #####################
        ##### JSON ##########
        #####################
        
        # extract meta-outcomes
        outcomes <- unique( rmm$outclab )
        
        if( is.null( outcomes ) | length( outcomes ) == 0 )
        {
            print( paste0( "*** WARNING: OUTCOMES EMPTY ***", rm5_gz_infile ) )  
            # Use return() to exit the tryCatch block for this iteration (replaces 'next')
            return()
        }
        
        # combine them into a named list
        combined <- list( primary = text_data$primary_outcomes_html, secondary = text_data$secondary_outcomes_html, outcomes = outcomes )
        
        # json output
        json_outfile <- paste0( outdir, '/', review_name, '.json' )
        
        # Write to JSON file
        write_json( combined, json_outfile, pretty = TRUE, auto_unbox = TRUE )
        
        ################
        #### LLM #######
        ################
        
        input_llm_file <- json_outfile
        output_llm_file <- gsub( ".json", "_labeled.json", input_llm_file )
        
        # run LLM labeling primary-secondary-unknown
        if( file.exists( input_llm_file ) & !file.exists( output_llm_file ) )
        {
            print( paste0( "*** 1. PROCESSING LLM LABELING ***: ", input_llm_file ) )
            command <- paste0( "./llm_labeler_gemini.py ", "-i ", input_llm_file, " -o ", output_llm_file )
            system( command )
        }
        
        ###################################
        # MERGE tables with outcome labels
        ###################################
        
        # meta output
        meta_outfile <- paste0( outdir, '/', review_name, '_meta.tsv' )
        
        # read labels
        if( file.exists( output_llm_file ) & !file.exists( meta_outfile ) )
        {
            print( paste0( "*** 2. MERGING LABELS USING ***: ", input_llm_file ) )    
            
            json_data <- jsonlite::fromJSON( output_llm_file )
            
            df_labels <- as_tibble( json_data$labeled_outcomes )
            df_labels$outclab <- df_labels$outcome
            df_labels$outcome <- NULL
            
            comb <- merge( df_labels, rmm )
            
            readr::write_tsv( comb, meta_outfile, quote = 'all' )    
        }
        
        
    }, error = function(e) {
        
        # This block executes if any code inside tryCatch throws an error.
        # It prints a helpful message, and then the loop continues to the next file.
        message(paste("\n*** ❗️ ERROR processing file:", html_infile, "***"))
        message("Skipping this file. The specific error was:")
        message(conditionMessage(e))
        message("********************************\n")
    })
    
} # end loop html_infiles
