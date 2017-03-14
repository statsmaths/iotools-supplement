# Download the Airline On-time performance data
# Authors: Taylor Arnold and Mike Kane
# Date: 2017-03-14

# Create the airline-data directory and cd into it.
dir.create("airline-data")
setwd("airline-data")

# Download and decompress the data.
url_prefix = "http://stat-computing.org/dataexpo/2009/"
bzip_app = "bzip2"

if (length(system("which pbzip2", intern=TRUE)) > 0) {
  cat("pbzip2 detected. Setting number of cores to", parallel::detectCores()-1,
      "\n")
  bzip_app = paste("pbzip2 -p", parallel::detectCores()-1, sep="")
}

# Cycle through each year of data
file_nums = as.character(1987:2008)
for (file_num in file_nums) {
  url = paste(url_prefix, file_num, ".csv.bz2", sep="")
  download.file(url, destfile=paste(file_num, ".csv.bz2", sep=""))
  cat("Decompressing", paste(file_num, ".csv.bz2", sep=""), "\n")
  system(paste(bzip_app, "--keep -f -d", paste(file_num, ".csv.bz2", sep="")))
}

# Create test datasets by putting all of the files together
make_single_airline_file_str = 
  paste("cp 1987.csv airline_all.csv",
        "for year in {1988..2008}",
        "do",
        "  tail -n+2 $year.csv >> airline_all.csv",
        "done",
        sep="\n")

system(make_single_airline_file_str, intern=TRUE)

df_sizes = c(10, 100, 1000, 10000, 100000, 1000000, 10000000)

for (s in df_sizes) {
  num_lines = format(s, scientific=FALSE)
  cs = paste("head -n", num_lines, "airline_all.csv >", 
             paste("airline_all_first_", num_lines, ".csv", sep=""))
  system(cs)
}

# Return to the original directory
setwd("..")
