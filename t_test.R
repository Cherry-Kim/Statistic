#One sample t-test
data2 <- read.table('data.txt', header=T)
head(data2)

data <- data2[,-1]
rownames(data) <- data2[,1]

#convert data frame to numeric
dat.log2 <- data
for (i in 1:ncol(data)){
        dat.log2[,i] <- log(data[,i]+1,2)
}
write.csv(dat.log2, "dat.log2.csv", quote=F)


group1_cols <- c("sample1")
group2_cols <- c("sample2", "sample3", )


# Calculate t-scores for each sample in Group2
group1_data <- as.matrix(dat.log2[, group1_cols])
group2_data <- as.matrix(dat.log2[, group2_cols])

results <- data.frame(t_score = numeric(), p_value = numeric())
for (i in 1:nrow(group2_data)) {
  result <- t.test(group2_data[i,], mu = group1_data[i])
  t_score <- result$statistic
  p_value <- result$p.value
  results <- rbind(results, c(t_score, p_value))
}
write.csv(results, paste0("t_test_", group1_cols, ".log2.csv"), row.names = FALSE)

