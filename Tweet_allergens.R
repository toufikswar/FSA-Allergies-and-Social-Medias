
### FSA base script

### saved .xlsx data as .csv, then can import

tmp1 <- read.csv("20161029-20171127.csv", header = TRUE)
tmp2 <- read.csv("20171128-20180926.csv", header = FALSE)
colnames(tmp2) <- colnames(tmp1)
data <- rbind(tmp1, tmp2)