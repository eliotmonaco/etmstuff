# `regex_various` used in `clean_street_address()`

## Dataframe ####

rows <- c(
  "embed_punct",
  "house_frac",
  "nondigit",
  "nonres",
  "nth",
  "num_dir",
  "num_only",
  "num_word",
  "numsign",
  "phone",
  "symbol",
  "unit",
  "unknown"
)

regex_various <- as.data.frame(matrix(nrow = length(rows), ncol = 3))

regex_various <- as.data.frame(apply(regex_various, 2, as.character))

rownames(regex_various) <- rows
colnames(regex_various) <- c("pattern", "replacement", "exception")

# regex_various["", "pattern"]

## embed_punct ####

p <- paste0(
  "(?x) ", # Turn on free-spacing
  "(?<= [:alnum:]) ", # Preceded by an alphanumeric
  "[:punct:]+ ", # One or more punctuation
  "(?= [:alnum:])" # Followed by an alphanumeric
)

regex_various["embed_punct",] <- c(p, " ", NA)

## house_frac ####

p <- paste0(
  "(?x) ", # Turn on free-spacing
  "(?<= \\d) ", # Preceded by a digit
  "\\s+ (1 | ONE | ([:alpha:]* \\s* AND \\s+ [:alpha:]*))* \\s* HALF \\s* ",
  "(?= \\s [:alnum:]) ", # Followed by a space and alphanumeric
  "(?! \\s full)" # Not followed by " Full", as in the street name "Half Full"
)

regex_various["house_frac",] <- c(p, " 1/2 ", NA)

## nondigit ####

p <- "^\\D+(?=\\d*)"

regex_various["nondigit",] <- c(p, "", NA)

## nonres ####

p <- "(?x) ^ .* (9999.*address | 9999.*need | address.*need) .* $"

regex_various["nonres",] <- c(p, "", NA)

## nth ####

p <- "(?x) \\b Nth \\b"

regex_various["nth",] <- c(p, "N", NA)

## num_dir ####

p <- paste0(
  "(?x) ", # Turn on free-spacing
  "(?<= ^ \\d{1,1000}) ", # Initial digit(s)
  "[ENSW] \\b" # Concatenated with a direction letter, then a word boundary
)

regex_various["num_dir",] <- c(p, "", NA)

## num_only ####

p <- "^\\d+$"

regex_various["num_only",] <- c(p, "", NA)

## num_word ####

p <- paste0(
  "(?x) ", # Turn on free-spacing
  "(?<= ^ \\d{1,1000}) ", # Initial digit(s)
  "[:alpha:]{2,}" # Concatenated with a word
)

regex_various["num_word",] <- c(p, "", NA)

## numsign ####

p <- "#"

regex_various["numsign",] <- c(p, " ", NA)

## phone ####

p <- "\\d{7,}"

regex_various["phone",] <- c(p, "", NA)

## symbol ####

p <- "[:symbol:]"

regex_various["symbol",] <- c(p, "", NA)

## unit ####

p <- paste0(
  "(?x) ", # Turn on free-spacing
  "(?<= \\s) ", # Preceded by a space
  "(#|ap|apt|apartment|lot|no(?!rth)|num|number|rm|room|ste|suite|trlr|trailer|unit) ", # Unit prefix
  "(\\s | [:punct:])* ", # Spaces and/or punctuation marks
  "[:alnum:]+$" # Unit identifier, adjacent to end of string
)

regex_various["unit",] <- c(p, "", NA)

## unknown ####

p <- "(?x) ^ .* unknown .* $"

regex_various["unknown",] <- c(p, "", NA)



## Save ####

saveRDS(regex_various, "dev_aux/helpers/regex_various.rds")
