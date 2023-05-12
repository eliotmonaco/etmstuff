get_qtr <- function(df, var) {

  m <- as.numeric(na.omit(unique(format(as.Date(df[[var]], tryFormats = c("%Y-%m-%d", "%Y%m%d")),
                                        format = "%m"))))

  q <- c()

  q[1] <- ifelse(any(1:3 %in% m), 1, 0)
  q[2] <- ifelse(any(4:6 %in% m), 2, 0)
  q[3] <- ifelse(any(7:9 %in% m), 3, 0)
  q[4] <- ifelse(any(10:12 %in% m), 4, 0)

  q[which(q != 0)]

}



