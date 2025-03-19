# Load necessary library for XML parsing
# #library(xml2)
# 
# library(DBI)
# library(RSQLite)
require(c("DBI","RSQLite"))

# Specify the file path.
file_path <- "x axis.ndf" ##MM update this with relative file paths

# ---------------------------
# Step 1: Read the Header
# ---------------------------
# Increase the number of header bytes to ensure we capture the full header.
file_size <- file.info(file_path)$size

header_bytes_to_read <-round(file_size/100) #8192 #MM: what determines this #? I changed it to be be a generic 1% of total file size

con <- file(file_path, "rb")
header_raw <- readBin(con, what = "raw", n = header_bytes_to_read)
close(con)

# Locate the first occurrence of the '<' character (0x3C) which indicates the start of the XML header.
xml_start_index <- which(header_raw == as.raw(0x3C))[1]
if (is.na(xml_start_index)) {
  stop("Could not find the start of the XML header in the file.")
}

# Define the byte pattern for the closing tag </Channel> in UTF-16LE.
# The string "</Channel>" in UTF-16LE is encoded as:
#   3C 00 2F 00 43 00 68 00 61 00 6E 00 6E 00 65 00 6C 00 3E 00
pattern <- as.raw(c(
  0x3C, 0x00,  # <
  0x2F, 0x00,  # /
  0x43, 0x00,  # C
  0x68, 0x00,  # h
  0x61, 0x00,  # a
  0x6E, 0x00,  # n
  0x6E, 0x00,  # n
  0x65, 0x00,  # e
  0x6C, 0x00,  # l
  0x3E, 0x00   # >
))
pattern_index <- NA
for(i in seq(xml_start_index, length(header_raw) - length(pattern) + 1)) {
  if(identical(header_raw[i:(i+length(pattern)-1)], pattern)) {
    # pattern_index marks the end position of the pattern in the header.
    pattern_index <- i + length(pattern) - 1
    break
  }
}
if (is.na(pattern_index)) {
  stop("Could not determine the end of the XML header by finding the '</Channel>' tag.")
}

# Extract the XML header bytes.
xml_bytes <- header_raw[xml_start_index:pattern_index]

# Because the XML header is in UTF-16LE (each character stored with an interleaved null),
# extract every other byte (this works if the text is plain ASCII) and convert to character strings:
xml_bytes_ascii <- xml_bytes[seq(1, length(xml_bytes), by = 2)]
xml_string <- rawToChar(xml_bytes_ascii)
cat("Extracted XML header:\n", xml_string, "\n")

# ---------------------------
# Step 2: Parse XML Header Information
# ---------------------------
# Parse the XML string using the xml2 package.
doc <- read_xml(xml_string)

# Extract metadata variables from the XML using xml text which returns a character string
channel_name <- xml_text(xml_find_first(doc, "//Name"))
channel_label <- xml_text(xml_find_first(doc, "//Label"))
source_type   <- xml_text(xml_find_first(doc, "//SourceType"))
unit          <- xml_text(xml_find_first(doc, "//Unit"))
samp_rate     <- xml_text(xml_find_first(doc, "//SamplingRate")) 

# Print extracted metadata for confirmation.
cat("Channel Name: ", channel_name, "\n") #MM: "\n" means new line
cat("Channel Label:", channel_label, "\n")
cat("Source Type:  ", source_type, "\n")
cat("Unit:         ", unit, "\n")
cat("Sampling Rate " , samp_rate, "\n")

# ---------------------------
# Step 3: Read the Waveform Data
# ---------------------------
# The header ends at 'pattern_index'.
header_length <- pattern_index

# Get the total file size and compute the number of bytes for the waveform data.
#file_size <- file.info(file_path)$size moved this to above
data_bytes_length <- file_size - header_length

# Open a binary connection to the file and move the pointer to the end of the header.
con <- file(file_path, "rb") #rb means read binary mode; can grab information from this using $
seek(con, where = header_length, origin = "start")

# Read the waveform data (assumed to be stored as 16-bit signed integers in little-endian format).
waveform <- readBin(con, what = "integer", size = 2, n = data_bytes_length / 2, endian = "little")
close(con)

# Create a data frame with sample indices and the waveform data.
df <- data.frame(Time = seq_along(waveform), Signal = waveform)

# ---------------------------
# Step 4: Plotting the Data
# ---------------------------

# Assume a known sampling frequency (Hz); adjust 'fs' as needed.
fs <- 10  # Replace with the actual sampling frequency if known

# Add a time vector (in seconds) to the data frame.
df$TimeSec <- df$Time / fs

# Plot 1: Plot the full waveform signal.
# plot(df$Time, df$Signal, type = "l",
#     xlab = "Sample Index", 
#     ylab = paste("Signal (", unit, ")", sep=""),
#     main = paste("Waveform Signal -", channel_label))

# Plot 2: Plot a 5-minute segment starting 20 minutes into the data.
#start_sample <- 20 * 60 * fs + 1        # +1 because R indices start at 1
#end_sample <- start_sample + 1 * 60 * fs - 1
start_sample <- 1
end_sample <- 100000
if(nrow(df) >= end_sample) {
  plot(df$TimeSec[start_sample:end_sample], df$Signal[start_sample:end_sample], type = "l",
       xlab = "Time (s)", 
       ylab = paste("Signal (", unit, ")", sep=""),
       main = paste("Waveform Signal -", channel_label, "(20-25 Minutes)"))
} else {
  warning("The data does not extend to 25 minutes. Plotting available data starting at 20 minutes.")
  plot(df$TimeSec[start_sample:nrow(df)], df$Signal[start_sample:nrow(df)], type = "l",
       xlab = "Time (s)", 
       ylab = paste("Signal (", unit, ")", sep=""),
       main = paste("Waveform Signal -", channel_label, "(Starting at 20 Minutes)"))
}


#Inspecting data.ndb file for information
library(reticulate)

# Specify the path to the Data.ndb file.
ndb_path <- "C:/Users/dbour/OneDrive/Documents/R-Projects/ndf Import and Export/Data.ndb"

# Create the Python code as a string. 
# We use sprintf() to insert the file path into the Python code.
py_code <- sprintf("
with open('%s', 'rb') as f:
    ndb_header = f.read(512)
print(ndb_header.hex())
", ndb_path)

# Run the Python code using reticulate.
py_run_string(py_code)


# ndb file is SQLLite format. Using RSQLite to read file
# Install RSQLite if needed:
#install.packages("RSQLite")

library(DBI)
library(RSQLite)

# Specify the path to the Data.ndb file.
ndb_path <- "C:/Users/dbour/OneDrive/Documents/R-Projects/ndf Import and Export/Data.ndb"

# Connect to the SQLite database.
con <- dbConnect(SQLite(), ndb_path)

# List all tables in the database.
tables <- dbListTables(con)
cat("Tables in the Data.ndb file:\n")
print(tables)

# For each table, show its structure and a preview of the data.
for(tbl in tables) {
  cat("\n\nTable:", tbl, "\n")
  # Get table structure.
  tbl_info <- dbGetQuery(con, sprintf("PRAGMA table_info(%s);", tbl))
  print(tbl_info)
  
  # Preview the first 10 rows of the table.
  cat("First 10 rows of", tbl, ":\n")
  tbl_data <- dbReadTable(con, tbl)
  print(head(tbl_data, 10))
}

# Disconnect from the database.
dbDisconnect(con)

# loading internal_property table into a datafram 
# Install RSQLite if needed:
# install.packages("RSQLite")

library(DBI)
library(RSQLite)

# Specify the path to the Data.ndb file (the SQLite database)
ndb_path <- "C:/Users/dbour/OneDrive/Documents/R-Projects/ndf Import and Export/Data.ndb"

# Connect to the SQLite database.
con <- dbConnect(SQLite(), ndb_path)

# Load the "internal_property" table into a dataframe.
internal_property_df <- dbReadTable(con, "internal_property")

# Load the "device_info" table into a dataframe.
device_info_df <- dbReadTable(con, "device_info")

# Print the first few rows of the dataframe to inspect its contents.
head(internal_property_df)
head(device_info_df)
# Disconnect from the database when done.
dbDisconnect(con)

# We have edf start and stop time as well as demographics in table. This code will convert
# .NET ticks (100nanoseconds) to a POSIXct value in r
# Helper function to convert .NET ticks to POSIXct date-time (UTC)
# First, ensure that the keys are trimmed (to remove any accidental extra spaces)
internal_property_df$key <- trimws(internal_property_df$key)

# Extract the tick values for RecordingStart and RecordingStop
RecordingStart_ticks <- as.numeric(internal_property_df$value[internal_property_df$key == "RecordingStart"])
RecordingStop_ticks  <- as.numeric(internal_property_df$value[internal_property_df$key == "RecordingStop"])
PatientID  <- as.character(internal_property_df$value[internal_property_df$key == "ID"])

# Helper function to convert .NET ticks to POSIXct date-time (UTC)
convert_ticks <- function(ticks) {
  base_ticks <- 621355968000000000  # .NET ticks for 1970-01-01
  seconds_since_1970 <- (ticks - base_ticks) / 1e7
  as.POSIXct(seconds_since_1970, origin = "1970-01-01", tz = "UTC")
}

# Convert tick values to standard date-times
RecordingStart <- convert_ticks(RecordingStart_ticks)
RecordingStop  <- convert_ticks(RecordingStop_ticks)

# Print the results
print(RecordingStart)
print(RecordingStop)


#Getting information from Device.INI file
# Install the 'ini' package if you haven't already:
# install.packages("ini")

library(ini)

# Read the DEVICE.INI file. Adjust the file path if necessary.
device_ini <- read.ini("DEVICE.INI")

# Extract the channel configuration from the "Channels" section.
channel_config <- device_ini$Channels

# Print the channel configuration.
print(channel_config)

# Split each channel's configuration into frequency and unit.
channel_details <- lapply(channel_config, function(val) {
  parts <- strsplit(val, ";")[[1]]
  # Trim spaces if necessary.
  parts <- trimws(parts)
  list(sampling = parts[1], unit = parts[2])
})

# Print parsed channel details.
print(channel_details)
