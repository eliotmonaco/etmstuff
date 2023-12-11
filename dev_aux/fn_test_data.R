# Generate data to test functions



## Test lab_result functions

df_test_lrv <- data.frame(
  lab_result_value = c(
    "1", "2", "3", "14", "55",
    "1.0", "2.0", "3.0", "14.0", "55.0",
    ".11", ".999", "6.989", "333.333",
    " 4 . 6 ",
    "3.", "15.", ".67", ".9", "0.5",
    "< 1", "< 3.3", "> 2", ">3.5",
    "< 3.0.", "<3.0'", "< 3.o",
    "<0", "<99", "< word", "> word", "> 3 word", "> 4 word",
    "0.9.8", "10.4.2",
    "2.2`", "`2.2", "3.3'", "'3.3", "4,4",
    "4mcg/dL", "10 ug/dL", "mcg/dL 55.0",
    "= 9.0", "=9.0", "=9,0", "=9.0 ug/dL",
    "0", "0.0", "0.00",
    "none detected", "None Detected", "Not Detected",
    "words words words", "6 digit . period 0 digit", "word 8 word 9 word",
    "", NA
  )
)



