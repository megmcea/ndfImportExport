# Set the file path to your .ndf file
file_path <- "x axis.ndf"

# Read a large chunk of the header (adjust if needed)
header_bytes_to_read <- 16384
con <- file(file_path, "rb")
header_raw <- readBin(con, what = "raw", n = header_bytes_to_read)
close(con)

print_hex <- function(raw_vec, start = 1, end = length(raw_vec)) {
  # Convert raw values to integers first
  int_vals <- as.integer(raw_vec[start:end])
  hex_str <- paste(sprintf("%02X", int_vals), collapse = " ")
  cat(hex_str, "\n")
}

# 1. Display the first 128 bytes to see the magic number and version info.
cat("First 128 bytes (hex):\n")
print_hex(header_raw, 1, 128)

# 2. Locate the beginning of the XML header (look for '<' which is 0x3C)
xml_start_index <- which(header_raw == as.raw(0x3C))[1]
cat("First occurrence of '<' (XML start) at byte offset:", xml_start_index, "\n")

# 3. Display the binary portion before the XML block (if any)
if (!is.na(xml_start_index) && xml_start_index > 1) {
  cat("Binary portion before XML (first", xml_start_index - 1, "bytes):\n")
  print_hex(header_raw, 1, xml_start_index - 1)
} else {
  cat("No non-XML binary header found, or XML starts at the beginning.\n")
}

# 4. Display a snippet of the XML block.
# Let's show, for example, 512 bytes starting from the XML start.
if (!is.na(xml_start_index)) {
  xml_end_index <- min(xml_start_index + 1023, length(header_raw))
  cat("XML snippet (hex):\n")
  print_hex(header_raw, xml_start_index, xml_end_index)
  
  # Convert the XML snippet to text by extracting every other byte (UTF-16LE assumption)
  xml_bytes <- header_raw[xml_start_index:xml_end_index]
  xml_bytes_ascii <- xml_bytes[seq(1, length(xml_bytes), by = 2)]
  xml_string <- rawToChar(xml_bytes_ascii)
  cat("\nXML snippet (interpreted as text):\n")
  cat(xml_string, "\n")
} else {
  cat("Could not locate XML header in the file.\n")
}

#---- Gettingmore XML data
# Locate XML start as before
xml_start_index <- which(header_raw == as.raw(0x3C))[1]
xml_end_index <- min(xml_start_index + 2048, length(header_raw))  # Adjust if needed
# Convert the XML snippet raw bytes (assumed UTF-16LE) to text.
xml_bytes <- header_raw[xml_start_index:xml_end_index]

# Extract every other byte (the actual characters in UTF-16LE)
xml_bytes_ascii <- xml_bytes[seq(1, length(xml_bytes), by = 2)]

# Remove any embedded null bytes from the raw vector.
xml_bytes_ascii_clean <- xml_bytes_ascii[xml_bytes_ascii != as.raw(0)]

# Convert the cleaned raw vector to a character string.
xml_string <- rawToChar(xml_bytes_ascii_clean)
cat("\nXML snippet (interpreted as text):\n")
cat(xml_string, "\n")

#------------------------------------
library(xml2)

# Your full string (XML plus extra data)
xml_str_full <- "<Channel><Name>NOX</Name><Label>X Axis</Label><SourceType>Raw</SourceType><Unit>g</Unit><HASH>5289b46c-efd5-4061-832b-82e8e78ea9cb</HASH><Source /><DeviceID>NoxMedical.T3SDevice</DeviceID><DeviceSerial>300100955</DeviceSerial><ChannelNumber>0</ChannelNumber><Format>Int16</Format><Function /><IsBipolar>0</IsBipolar><IsDC>0</IsDC><IsRespiratory>0</IsRespiratory><ImpedanceCheck>0</ImpedanceCheck><Scale>0.0000610370189519944</Scale><Offset>0</Offset><Type>Gravity.X-Thorax</Type><SamplingRate>20</SamplingRate></Channel>,20250131T235908.254000"

# Split the string at the comma and take the first part (the pure XML)
xml_str <- strsplit(xml_str_full, ",")[[1]][1]

# Parse the XML
doc <- read_xml(xml_str)

# Get all child nodes of the <Channel> element
nodes <- xml_children(doc)

# Extract parameter names and their values
params <- sapply(nodes, xml_name)
values <- sapply(nodes, xml_text)

# Create a data frame with the parameters and values
param_table <- data.frame(Parameter = params, Value = values, stringsAsFactors = FALSE)

# Print the table
print(param_table)
