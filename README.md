# RunsCorrelation
Compute pairwise Kendall's Tau-b correlation between two runs

```{r }
getScoreRanksCorrelation <- function(runA, runB){
  # generate rank for scores with ties
  runA$scoreRank = unlist(with(runA, tapply(-score, queryNum, rank, ties.method='min')))
  runB$scoreRank = unlist(with(runB, tapply(-score, queryNum, rank, ties.method='min')))
  
  # get list of queryNum and Document Id pairs from the first run
  dfQueryDocs = unique(runA[, c("queryNum", "docId")])
  
  # append list of queryNum and Document Id pairs from the second run to the first
  dfQueryDocs = rbind(dfQueryDocs, runB[, c("queryNum", "docId")])
  
  
  # remove duplicates query-docId pairs
  dfQueryDocs = unique(dfQueryDocs)
  
  # generate document score rank for each run by LEFT JOINing the QueryDocs with the run
  docScoreRank_A = merge(x = dfQueryDocs, y = runA[,c("queryNum","docId", "scoreRank")], by = c("queryNum", "docId"), all.x = TRUE)
  docScoreRank_B = merge(x = dfQueryDocs, y = runB[,c("queryNum","docId", "scoreRank")], by = c("queryNum", "docId"), all.x = TRUE)
  
  # Sort both docScoreRank lists to ensure both are in the same comparable order
  docScoreRank_A = docScoreRank_A[order(docScoreRank_A$queryNum, docScoreRank_A$docId),]
  docScoreRank_B = docScoreRank_B[order(docScoreRank_B$queryNum, docScoreRank_B$docId),]
  
  # refresh the row names according to the new order
  rownames(docScoreRank_A) = NULL
  rownames(docScoreRank_B) = NULL
  
  # replace NA with 0
  docScoreRank_A[is.na(docScoreRank_A)] <- 0
  docScoreRank_B[is.na(docScoreRank_B)] <- 0
  
  # calculate correlaction between the two document score rank
  return(cor(docScoreRank_A$scoreRank, docScoreRank_B$scoreRank, method="kendall", use="pairwise"))
}
```
