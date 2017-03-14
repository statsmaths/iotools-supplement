# Download the Airline On-time performance data
# Authors: Taylor Arnold and Mike Kane
# Date: 2017-03-14

library(dplyr)
library(readr)
library(iotools)
library(microbenchmark)

n <- 1e6
p <- 25

mb <- function(...) {
  val <- system.time({ eval(...) })
  val[["elapsed"]]
}

mb2 <- function(...) {
  val <- system.time({ eval(...) })
  val[["elapsed"]]
}

out <- matrix(NA_real_, ncol = 7, nrow = 8)

# integer
mat <- matrix(sample(1:1000, n * p, replace = TRUE), ncol = p)
mat <- as_data_frame(mat)
write_csv(mat, tf <- tempfile(), col_names = FALSE)
write_rds(mat, tf_raw <- tempfile())
write_rds(as.matrix(mat), tf_raw_matrix <- tempfile())

out[1,1] <- mb({df <- read_csv(tf, col_names = FALSE, col_types = paste(rep("i", p), collapse = ""))})
out[1,2] <- mb({df <- dstrsplit(readAsRaw(tf), col_types = rep("integer", p))})
out[1,3] <- mb({df <- read_rds(tf_raw)})
out[1,4] <- mb2({df <- read.csv(tf, header = FALSE, colClasses = "integer")})
out[1,5] <- mb({df <- read_rds(tf_raw_matrix)})
out[1,6] <- mb({df <- mstrsplit(readAsRaw(tf), type = "integer")})
out[1,7] <- mb({df <- as.matrix(read_rds(tf_raw))}) + out[1,4]

# numeric
mat <- matrix(runif(n * p), ncol = p)
mat <- as_data_frame(mat)
write_csv(mat, tf <- tempfile(), col_names = FALSE)
write_rds(mat, tf_raw <- tempfile())
write_rds(as.matrix(mat), tf_raw_mat <- tempfile())

out[2,1] <- mb({df <- read_csv(tf, col_names = FALSE, col_types = paste(rep("d", p), collapse = ""))})
out[2,2] <- mb({df <- dstrsplit(readAsRaw(tf), col_types = rep("numeric", p))})
out[2,3] <- mb({df <- read_rds(tf_raw)})
out[2,4] <- mb2({df <- read.csv(tf, header = FALSE, colClasses = "numeric")})
out[2,5] <- mb({df <- read_rds(tf_raw_matrix)})
out[2,6] <- mb({df <- mstrsplit(readAsRaw(tf), type = "numeric")})
out[2,7] <- mb({df <- as.matrix(read_rds(tf_raw))}) + out[2,4]

# complex
mat <- matrix(complex(real = runif(n * p), imaginary = runif(n * p)), ncol = p)
mat <- as.data.frame(mat)
write.table(mat, tf <- tempfile(), sep = ",", col.names = FALSE, row.names = FALSE)
saveRDS(mat, tf_raw <- tempfile())
saveRDS(as.matrix(mat), tf_raw_mat <- tempfile())

#out[3,1] <- mb({df <- read_csv(tf, col_names = FALSE, col_types = paste(rep("d", p), collapse = ""))})
out[3,2] <- mb({df <- dstrsplit(readAsRaw(tf), col_types = rep("complex", p))})
out[3,3] <- mb({df <- readRDS(tf_raw)})
out[3,4] <- mb2({df <- read.csv(tf, header = FALSE, colClasses = "complex")})
out[3,5] <- mb({df <- read_rds(tf_raw_matrix)})
out[3,6] <- mb({df <- mstrsplit(readAsRaw(tf), type = "complex")})
out[3,7] <- mb({df <- as.matrix(read_rds(tf_raw))}) + out[3,4]

# logical
mat <- matrix(sample(c(FALSE, TRUE), n * p, replace = TRUE), ncol = p)
mat <- as_data_frame(mat)
write_csv(mat, tf <- tempfile(), col_names = FALSE)
write_rds(mat, tf_raw <- tempfile())
write_rds(as.matrix(mat), tf_raw_mat <- tempfile())

out[4,1] <- mb({df <- read_csv(tf, col_names = FALSE, col_types = paste(rep("l", p), collapse = ""))})
out[4,2] <- mb({df <- dstrsplit(readAsRaw(tf), col_types = rep("logical", p))})
out[4,3] <- mb({df <- read_rds(tf_raw)})
out[4,4] <- mb2({df <- read.csv(tf, header = FALSE, colClasses = "logical")})
out[4,5] <- mb({df <- read_rds(tf_raw_matrix)})
out[4,6] <- mb({df <- mstrsplit(readAsRaw(tf), type = "logical")})
out[4,7] <- mb({df <- as.matrix(read_rds(tf_raw))}) + out[4,4]

# character
mat <- matrix(sample(state.abb, n * p, replace = TRUE), ncol = p)
mat <- as_data_frame(mat)
write_csv(mat, tf <- tempfile(), col_names = FALSE)
write_rds(mat, tf_raw <- tempfile())
write_rds(as.matrix(mat), tf_raw_mat <- tempfile())

out[5,1] <- mb({df <- read_csv(tf, col_names = FALSE, col_types = paste(rep("c", p), collapse = ""))})
out[5,2] <- mb({df <- dstrsplit(readAsRaw(tf), col_types = rep("character", p))})
out[5,3] <- mb({df <- read_rds(tf_raw)})
out[5,4] <- mb2({df <- read.csv(tf, header = FALSE, colClasses = "character")})
out[5,5] <- mb({df <- read_rds(tf_raw_matrix)})
out[5,6] <- mb({df <- mstrsplit(readAsRaw(tf), type = "character")})
out[5,7] <- mb({df <- as.matrix(read_rds(tf_raw))}) + out[5,4]

# raw
mat <- matrix(as.raw(sample(1:250, n * p, replace = TRUE)), ncol = p)
mat <- as.data.frame(mat)
write.table(mat, tf <- tempfile(), sep = ",", col.names = FALSE, row.names = FALSE)
saveRDS(mat, tf_raw <- tempfile())
saveRDS(as.matrix(mat), tf_raw_mat <- tempfile())

#out[6,1] <- mb({df <- read_csv(tf, col_names = FALSE, col_types = paste(rep("c", p), collapse = ""))})
out[6,2] <- mb({df <- dstrsplit(readAsRaw(tf), col_types = rep("raw", p))})
out[6,3] <- mb({df <- read_rds(tf_raw)})
out[6,4] <- mb2({df <- read.csv(tf, header = FALSE, colClasses = "raw")})
out[6,5] <- mb({df <- read_rds(tf_raw_matrix)})
out[6,6] <- mb({df <- mstrsplit(readAsRaw(tf), type = "raw")})
out[6,7] <- mb({df <- as.matrix(read_rds(tf_raw))}) + out[6,4]

# POSIXct
mat <- data_frame(x = Sys.time() + sample(0:10000, n, replace = TRUE))
for (i in 2:p) {
  mat <- bind_cols(mat, data_frame(x = Sys.time() + sample(0:10000, n, replace = TRUE)))
}
write_csv(mat, tf <- tempfile(), col_names = FALSE)
write_rds(mat, tf_raw <- tempfile())
write_rds(as.matrix(mat), tf_raw_mat <- tempfile())

out[7,1] <- mb({df <- read_csv(tf, col_names = FALSE, col_types = paste(rep("T", p), collapse = ""))})
out[7,2] <- mb({df <- dstrsplit(readAsRaw(tf), col_types = rep("POSIXct", p))})
out[7,3] <- mb({df <- read_rds(tf_raw)})
out[7,4] <- mb2({df <- read.csv(tf, header = FALSE, colClasses = "POSIXct")})
out[7,5] <- mb({df <- read_rds(tf_raw_matrix)})
#out[7,6] <- mb({df <- mstrsplit(readAsRaw(tf), type = "POSIXct")})
out[7,7] <- mb({df <- as.matrix(read_rds(tf_raw))}) + out[7,4]

# Mixed
mat1 <- matrix(sample(state.abb, n * p, replace = TRUE), ncol = p)
mat1 <- as_data_frame(mat1)
mat2 <- matrix(runif(n * p), ncol = p)
mat2 <- as_data_frame(mat2)
mat <- bind_cols(mat1, mat2)
write_csv(mat, tf <- tempfile(), col_names = FALSE)
write_rds(mat, tf_raw <- tempfile())
#write_rds(as.matrix(mat), tf_raw_mat <- tempfile())

out[8,1] <- mb({df <- read_csv(tf, col_names = FALSE, col_types = paste(rep(c("c", "d"), each = p), collapse = ""))})
out[8,2] <- mb({df <- dstrsplit(readAsRaw(tf), col_types = rep(c("character", "numeric"), each = p))})
out[8,3] <- mb({df <- read_rds(tf_raw)})
out[8,4] <- mb2({df <- read.csv(tf, header = FALSE, colClasses = rep(c("character", "numeric"), each = p))})
#out[8,5] <- mb({df <- read_rds(tf_raw_matrix)})
#out[8,6] <- mb({df <- mstrsplit(readAsRaw(tf), type = "character")})
#out[8,7] <- mb({df <- as.matrix(read_rds(tf_raw))}) + out[5,4]

print(out)

# Uncomment out whichever one you are testing
# saveRDS(out, "timing_mac.Rds")
# saveRDS(out, "timing_windows.Rds")
# saveRDS(out, "timing_linux.Rds")
