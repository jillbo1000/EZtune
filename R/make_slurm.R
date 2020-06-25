ex <- readLines("example.slurm")

f <- list.files("./conf")

for(i in 1:length(f)) {
  tmp <- ex
  nm <- gsub(".conf", "", f[i])
  cnf <- read.table(paste("./conf/", f[i], sep = ""))
  tmp[2] <- gsub("X", nrow(cnf), tmp[2])
  tmp[6] <- gsub("X", nm, tmp[6])
  tmp[16] <- gsub("X", f[i], tmp[16])
  write.table(tmp, paste("./conf/", nm, ".slurm", sep = ""), quote = FALSE,
              row.names = FALSE, col.names = FALSE)
}
