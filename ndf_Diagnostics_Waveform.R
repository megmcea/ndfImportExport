# Assume you already extracted the header and computed 'header_length'
# and that 'file_path' is set to "abdomen.ndf".

# Specify the file path.
file_path <- "x axis.ndf"

# Open a binary connection and read the waveform data with the current header length
con <- file(file_path, "rb")
seek(con, where = header_length, origin = "start")
waveform <- readBin(con, what = "integer", size = 2, n = (file.info(file_path)$size - header_length) / 2, endian = "little")
close(con)

# Diagnostic 1: Look at summary statistics and first few values
cat("Summary of waveform data:\n")
print(summary(waveform))
cat("First 20 samples:\n")
print(head(waveform, 20))

# Diagnostic 2: Plot the raw waveform
plot(waveform, type = "l", main = "Raw Waveform Data", xlab = "Sample Index", ylab = "Raw Value")

# Diagnostic 3: Histogram of the data values
hist(waveform, main = "Histogram of Raw Waveform Data", xlab = "Raw Value", breaks = 50)

# Diagnostic 4: Try adjusting the header offset slightly (e.g., header_length + 2 bytes) 
# and compare the resulting waveform.
con <- file(file_path, "rb")
seek(con, where = header_length + 2, origin = "start")
waveform_offset <- readBin(con, what = "integer", size = 2, n = (file.info(file_path)$size - (header_length + 2)) / 2, endian = "little")
close(con)

# Plot adjusted waveform for visual comparison
plot(waveform_offset, type = "l", main = "Waveform Data (Offset by +2 bytes)", xlab = "Sample Index", ylab = "Raw Value")
