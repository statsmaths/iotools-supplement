# Construct plot for the parallel reads of the Airline on-time performance data
# Authors: Taylor Arnold and Mike Kane
# Date: 2017-03-14

col_names = c("Year", "Month", "DayofMonth", "DayOfWeek", "DepTime",
              "CRSDepTime", "ArrTime", "CRSArrTime", "UniqueCarrier",
              "FlightNum", "TailNum", "ActualElapsedTime", "CRSElapsedTime",
              "AirTime", "ArrDelay", "DepDelay", "Origin", "Dest", "Distance",
              "TaxiIn", "TaxiOut", "Cancelled", "CancellationCode", "Diverted",
              "CarrierDelay", "WeatherDelay", "NASDelay", "SecurityDelay",
              "LateAircraftDelay")
col_classes = c(rep("integer", 8), "character", "integer", "character",
                rep("integer", 5), "character", "character", rep("integer", 4),
                "character", rep("integer", 6))

path = "airline-data"
data_files = dir(path)
figure_path = "."

library(microbenchmark)
library(iotools)
library(foreach)
library(readr)
library(tidyr)
library(ggplot2)
library(parallel)
library(doMC)

# I/O Methods and Formatters Section

## colClasses specified
microbenchmark(read.csv(file.path(path, "1987.csv"), header=TRUE,
                        colClasses=col_classes), times=5, unit="s")

microbenchmark(dstrsplit(readAsRaw(file.path(path, "1987.csv")), sep=",",
                         col_types=col_classes), times=5, unit="s")


## Column types inferred
microbenchmark(read.table(file.path(path, "1987.csv"), header=TRUE, sep=","),
               times=5, unit="s")

microbenchmark(read.csv.raw(file.path(path, "1987.csv"), header=TRUE, sep=","),
               times=5, unit="s")

## Read size timings

read_size = 1:7*1e6
read_file="2008.csv"

read_files_small = sprintf("2008_%d.csv", read_size)

# Write the files that we'll do the read tests on.
x = read.csv.raw(file.path(path, read_file))
foreach(i = 1:length(read_files_small)) %dopar% {
  write.csv(x[1:read_size[i],], file.path(path, read_files_small[i]),
            row.names=FALSE)
  NULL
}

dstrsplit_times = foreach(read_file=read_files_small, .combine=c) %do% {
  median(microbenchmark(dstrsplit(readAsRaw(file.path(path, read_file)),
                                  sep=",", col_types=col_classes),
                        times=5, unit="s")$time/1e9)
}

## Create a list of the readr column types.
#readr_col_types = paste(substr(col_classes, 1, 1), collapse="")
#
#read_csv_times = foreach(read_file=read_files_small, .combine=c) %do% {
#  median(microbenchmark(read_csv(file(file.path(path, read_file)),
#                                 progress=FALSE, col_types=readr_col_types),
#                        times=5,unit="s")$time/1e9)
#}
#
read.csv_times = foreach(read_file=read_files_small, .combine=c) %do% {
  median(microbenchmark(read.csv(file.path(path, read_file),
                           colClasses=col_classes),
                        times=5, unit="s")$time/1e9)
}

df = data.frame(list(dstrsplit=dstrsplit_times, read.csv=read.csv_times,
                     read_size=read_size))

dfl = df %>% gather(Function, Time, dstrsplit:read.csv)
dfl$Function = factor(dfl$Function)
dfl$Function = relevel(dfl$Function, ref="read.csv")
p = ggplot(dfl, aes(x=read_size, y=Time, linetype=Function)) + geom_line() +
  ylab("Time (in seconds)") + xlab("Number of Lines Read") +
  scale_color_discrete(name="Read Function") + geom_point() +
  theme_bw() + scale_fill_grey()

ggsave(file.path(figure_path, "import_times.pdf"), p, width=8, height=5)

# Create a single large model matrix

normalize_df = function(x) {
  names(x) = col_names
  x$DayOfWeek = factor(x$DayOfWeek, levels=1:7)
  x$Month = factor(x$Month, levels=1:12)
  x$DepTime = sprintf("%04d", x$DepTime)
  x$DepTime = as.numeric(substr(x$DepTime, 1, 2))*60 +
    as.numeric(substr(x$DepTime, 3, 4))
  x
}

form = ~ ArrDelay + DayOfWeek + DepTime + DepDelay + Month


strip_rownames = function(x) {
  rownames(x) = NULL
  x
}

# Create a single file.
data_files = file.path(path, paste0(1988:2008, ".csv"))
out_file = file("airline_mm.io", "wb")
for (data_file in data_files) {
  data_file %>% readAsRaw %>%
    dstrsplit(sep=",", skip=1, col_types=col_classes) %>% normalize_df %>%
    model.matrix(form, .) %>% strip_rownames %>% as.output(sep=",") %>%
    writeBin(out_file)
}
## Don't forget to close the file when we're done with it!
close(out_file)

# Create 1 model matrix per file. We'll use it later for benchmarking.
for (data_file in data_files) {
  out_file = gsub(".csv", "_io.csv", data_file)
  data_file %>% readAsRaw %>%
    dstrsplit(sep=",", skip=1, col_types=col_classes) %>% normalize_df %>%
    model.matrix(form, .) %>% strip_rownames %>% as.output(sep=",") %>%
    writeBin(out_file)
}

# Fit the OLS slope coefficients.

## Get the factor expansion of the variables.
mm_col_names = data_files[1] %>% read.csv.raw(header=TRUE, nrows=2) %>%
  normalize_df %>% model.matrix(form, .) %>% colnames

ne_chunks = chunk.apply("airline_mm.io",
  function(x) {
    mm = mstrsplit(x, sep=",", type="numeric")
    colnames(mm) = mm_col_names
      list(xtx=crossprod(mm[,-2]),
           xty=crossprod(mm[,-2], mm[,2, drop=FALSE]))
  }, CH.MERGE=list, parallel=4)

xtx = Reduce(`+`, Map(function(x) x$xtx, ne_chunks))
xty = Reduce(`+`, Map(function(x) x$xty, ne_chunks))

qr_xtx = qr(xtx)
keep_vars = qr_xtx$pivot[1:qr_xtx$rank]

## The regression coefficients
solve(xtx[keep_vars,keep_vars]) %*% xty[keep_vars]

# Parallel benchmarks
num_cores = 6#detectCores(TRUE, TRUE)

## pipeline parallel
pipeline_parallel_times = foreach (i=1:num_cores, .combine=c) %do% {
  system.time({
    chunk.apply("airline_mm.io",
      function(x) {
        mm = mstrsplit(x, sep=",", type="numeric")
        colnames(mm) = mm_col_names
          list(xtx=crossprod(mm[,-2]),
               xty=crossprod(mm[,-2], mm[,2, drop=FALSE]))
      }, CH.MERGE=list, parallel=i)
  })[['elapsed']]
}

## 1 reader parallel processing

one_reader_times = foreach (i=1:num_cores, .combine=c) %do% {
  registerDoMC(cores=i)
  system.time({
    foreach(mm=imstrsplit("airline_mm.io", sep=",", type="numeric"),
            .inorder=FALSE) %dopar% {
      colnames(mm) = mm_col_names
      list(xtx=crossprod(mm[,-2]),
           xty=crossprod(mm[,-2], mm[,2, drop=FALSE]))
    }
  })[['elapsed']]
}

## mulitple readers parallel processing
multiple_reader_times = foreach (i=1:num_cores, .combine=c) %do% {
  registerDoMC(cores=i)
  system.time({
    foreach(data_file=data_files) %dopar% {
      out_file = gsub(".csv", "_io.csv", data_file)
      ne_chunks = chunk.apply(out_file,
        function(x) {
          mm = mstrsplit(x, sep=",", type="numeric")
          colnames(mm) = mm_col_names
          list(xtx=crossprod(mm[,-2]),
               xty=crossprod(mm[,-2], mm[,2, drop=FALSE]))
        }, CH.MERGE=list)
      list(xtx=Reduce(`+`, Map(function(x) x$xtx, ne_chunks)),
           xty=Reduce(`+`, Map(function(x) x$xty, ne_chunks)))
    }
  })[['elapsed']]
}

df = data.frame(list("Pipeline.Parallel"=pipeline_parallel_times,
                     "One.Reader.Multi_Process"=one_reader_times,
                     "Multi_Readers.Multi_Process"=multiple_reader_times,
                     "Cores"=1:num_cores))

dfl = df %>% gather(Function, Time,
                    Pipeline.Parallel:Multi_Readers.Multi_Process)
dfl$Function = gsub("\\.", " ", dfl$Function)
dfl$Function = gsub("_", "-", dfl$Function)
dfl$Function = factor(dfl$Function)
dfl$Function = relevel(dfl$Function, "Pipeline Parallel")
dfl$Function = relevel(dfl$Function, "Multi-Readers Multi-Process")
dfl$Function = relevel(dfl$Function, "One Reader Multi-Process")

p = ggplot(dfl, aes(x=Cores, y=Time, linetype=Function)) + geom_line() +
  ylab("Time (in seconds)") + xlab("Number of Cores") +
  scale_color_discrete(name="Type of Parallelism") + geom_point() +
  theme_bw() + scale_fill_grey()

ggsave(file.path(figure_path, "par_lm_times.pdf"), p, width=8, height=5)
