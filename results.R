# Construct tables for the final paper
# Authors: Taylor Arnold and Mike Kane
# Date: 2017-03-14

library(dplyr)
library(readr)
library(iotools)
library(xtable)

out <- NULL

df <- as_data_frame(t(readRDS("timing_mac.Rds")))
colnames(df) <- c("int", "num", "cplx", "logical", "char", "raw", "time", "mix")
df$method <- c("read_csv", "dstrsplit", "readRDS", "read.csv", "readRDS", "mstrsplit", "as.matrix")
df$type <- c("dataframe", "dataframe", "dataframe", "dataframe", "matrix", "matrix", "matrix")
df$platform <- "mac"
out <- df

df <- as_data_frame(t(readRDS("timing_windows.Rds")))
colnames(df) <- c("int", "num", "cplx", "logical", "char", "raw", "time", "mix")
df$method <- c("read_csv", "dstrsplit", "readRDS", "read.csv", "readRDS", "mstrsplit", "as.matrix")
df$type <- c("dataframe", "dataframe", "dataframe", "dataframe", "matrix", "matrix", "matrix")
df$platform <- "windows"
out <- bind_rows(out, df)

df <- as_data_frame(t(readRDS("timing_linux.Rds")))
colnames(df) <- c("int", "num", "cplx", "logical", "char", "raw", "time", "mix")
df$method <- c("read_csv", "dstrsplit", "readRDS", "read.csv", "readRDS", "mstrsplit", "as.matrix")
df$type <- c("dataframe", "dataframe", "dataframe", "dataframe", "matrix", "matrix", "matrix")
df$platform <- "linux"
out <- bind_rows(out, df)

out %>%
  filter(type == "dataframe") %>%
  select(platform, method, int, logical, num, char, mix, cplx, raw) %>%
  arrange(factor(method, levels = c("readRDS", "dstrsplit", "read_csv", "read.csv"))) %>%
  arrange(factor(platform, levels = c("linux", "mac", "windows"))) %>%
  xtable(digits = 1) %>%
  print(include.rownames = FALSE, NA.string = "$\\cdot$")
