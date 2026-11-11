# Student: Gabriella Delgado
# Student Number: EDUV4815041
# Module: ITBDA4-14
# PROJECT 2
# QUESTION 1

# DATA PREPARATION
prepare_articles <- function(zip_file = "map_reduce_test_docs.zip") {
  
  if (!file.exists(zip_file)) stop("ZIP file not found")
  
  folder <- tempfile("articles")
  dir.create(folder)
  on.exit(unlink(folder, recursive = TRUE))
  unzip(zip_file, exdir = folder)
  
  files <- list.files(folder, pattern = "^doc[0-9]+\\.txt$",
                      recursive = TRUE, full.names = TRUE)
  
  if (length(files) == 0) stop("No news articles found")
  
  docs <- lapply(files, readLines, warn = FALSE, encoding = "UTF-8")
  names(docs) <- basename(files)
  
  dir.create("prepared", showWarnings = FALSE)
  
  old <- list.files("prepared",
                    pattern = "^doc[0-9]+\\.txt\\.tsv$",
                    full.names = TRUE)
  if (length(old) > 0) file.remove(old)
  
  for (i in seq_along(docs)) {
    records <- paste(names(docs)[i],
                     seq_along(docs[[i]]),
                     docs[[i]], sep = "\t")
    
    writeLines(records,
               file.path("prepared",
                         paste0(names(docs)[i], ".tsv")),
               useBytes = TRUE)
  }
  
  docs
}

# Question 1.2 - MAPPER
map_keyword <- function(doc_name, row, text, keyword) {
  
  # Find all occurrences of the keyword, case insensitive
  columns <- gregexpr(tolower(keyword), tolower(text),
                      fixed = TRUE)[[1]]
  
  # Keep all valid positions
  columns <- columns[columns > 0]
  
  if (length(columns) == 0) return(list())
  
  # Match complete words only
  columns <- columns[sapply(columns, function(col) {
    before <- if (col == 1) "" else substr(text, col - 1, col - 1)
    after <- substr(text, col + nchar(keyword), col + nchar(keyword))
    
    !grepl("[[:alnum:]_]", before) &&
      !grepl("[[:alnum:]_]", after)
  })]
  
  # Produce key-value pairs
  lapply(columns, function(col) {
    list(key = doc_name, value = paste0(row, ":", col))
  })
}

# QUESTION 1.3 - REDUCER
reduce_keyword <- function(key, values) {
  
  rc <- do.call(rbind, strsplit(values, ":", fixed = TRUE))
  
  values <- values[order(as.integer(rc[, 1]),
                         as.integer(rc[, 2]))]
  
  list(key = key, value = paste(values, collapse = ", "))
}

# Local MapReduce Demonstration
run_local <- function(keyword) {
  
  keyword <- trimws(keyword)
  if (!nzchar(keyword)) stop("Enter a keyword")
  
  docs <- prepare_articles()
  
  # Map
  mapped <- mapply(function(name, lines) {
    unlist(lapply(seq_along(lines), function(row) {
      map_keyword(name, row, lines[row], keyword)
    }), recursive = FALSE)
  }, names(docs), docs, SIMPLIFY = FALSE)
  
  all_pairs <- unlist(mapped, recursive = FALSE)
  
  if (length(all_pairs) == 0) {
    cat("No matches found\n")
    return(invisible(NULL))
  }
  
  # Shuffle
  shuffled <- split(
    sapply(all_pairs, `[[`, "value"),
    sapply(all_pairs, `[[`, "key")
  )
  
  # Reduce
  reduced <- lapply(names(shuffled), function(key) {
    reduce_keyword(key, shuffled[[key]])
  })
  
  # Final Output
  results <- data.frame(
    Document = sapply(reduced, `[[`, "key"),
    Locations = sapply(reduced, `[[`, "value"),
    row.names = NULL
  )
  
  print(results)
  invisible(results)
}

# Hadoop Mapper Interface
stream_mapper <- function() {
  
  keyword <- trimws(Sys.getenv("SEARCH_KEYWORD"))
  if (!nzchar(keyword)) stop("SEARCH_KEYWORD is missing")
  
  input <- file("stdin", "r")
  on.exit(close(input))
  
  repeat {
    record <- readLines(input, n = 1, warn = FALSE)
    if (length(record) == 0) break
    
    parts <- strsplit(record, "\t", fixed = TRUE)[[1]]
    if (length(parts) < 2) next
    
    text <- if (length(parts) > 2) {
      paste(parts[-c(1, 2)], collapse = "\t")
    } else ""
    
    pairs <- map_keyword(parts[1],
                         as.integer(parts[2]),
                         text, keyword)
    
    for (pair in pairs) {
      cat(pair$key, "\t", pair$value, "\n", sep = "")
    }
  }
}

# Hadoop Reducer Interface
stream_reducer <- function() {
  
  input <- file("stdin", "r")
  on.exit(close(input))
  
  current_key <- NULL
  values <- character()
  
  emit <- function(key, values) {
    pair <- reduce_keyword(key, values)
    cat(pair$key, "\t", pair$value, "\n", sep = "")
  }
  
  repeat {
    record <- readLines(input, n = 1, warn = FALSE)
    if (length(record) == 0) break
    
    parts <- strsplit(record, "\t", fixed = TRUE)[[1]]
    if (length(parts) < 2) next
    
    key <- parts[1]
    
    if (!is.null(current_key) && key != current_key) {
      emit(current_key, values)
      values <- character()
    }
    
    current_key <- key
    values <- c(values, parts[2])
  }
  
  if (!is.null(current_key)) emit(current_key, values)
}

# Question 1.4 - Hadoop MapReduce Configuration
run_hadoop <- function(keyword, output = "output_africa") {
  
  # Validate the dynamically supplied keyword
  keyword <- trimws(keyword)
  if (!nzchar(keyword)) stop("Enter a keyword")
  
  # Prepare input data
  prepare_articles()
  
  # Locate Hadoop Streaming
  hadoop_home <- Sys.getenv("HADOOP_HOME")
  
  jar <- list.files(
    file.path(hadoop_home, "share/hadoop/tools/lib"),
    pattern = "^hadoop-streaming-.*\\.jar$",
    full.names = TRUE
  )
  
  if (length(jar) == 0) {
    stop("Hadoop Streaming JAR not found")
  }
  
  if (dir.exists(output)) {
    stop("Output folder already exists")
  }
  
  # Hadoop job configuration
  args <- c(
    "jar", shQuote(jar[1]),
    "-D", "mapreduce.framework.name=local",
    "-D", "fs.defaultFS=file:///",
    "-D", "mapreduce.job.name=KeywordLocationSearch",
    "-D", "mapreduce.job.reduces=1",
    "-files", shQuote(normalizePath("MapReduce.R")),
    "-input", shQuote(normalizePath("prepared")),
    "-output", shQuote(file.path(getwd(), output)),
    "-mapper", shQuote("Rscript MapReduce.R mapper"),
    "-reducer", shQuote("Rscript MapReduce.R reducer"),
    "-cmdenv", shQuote(paste0("SEARCH_KEYWORD=", keyword))
  )
  
  # Execute Hadoop
  status <- system2("hadoop", args = args)
  
  if (status != 0) stop("Hadoop job failed")
  
  # Display results
  cat(readLines(file.path(output, "part-00000")),
      sep = "\n")
}

# The Command Line Execution
if (sys.nframe() == 0) {
  
  args <- commandArgs(trailingOnly = TRUE)
  
  if (length(args) < 1) {
    stop("Specify prepare, local, mapper or reducer")
  }
  
  if (args[1] == "prepare") {
    docs <- prepare_articles()
    cat("Prepared", length(docs), "documents\n")
    
  } else if (args[1] == "local") {
    if (length(args) < 2) stop("Supply a keyword")
    run_local(args[2])
    
  } else if (args[1] == "mapper") {
    stream_mapper()
    
  } else if (args[1] == "reducer") {
    stream_reducer()
    
  } else {
    stop("Unknown execution mode")
  }
}