get_yr <- function(df, var) {

  y <- as.numeric(na.omit(unique(format(as.Date(df[[var]], tryFormats = c("%Y-%m-%d", "%Y%m%d")),
                                        format = "%Y"))))

  sort(y)

}



