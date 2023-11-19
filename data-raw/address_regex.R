# `address_regex` used in `clean_street_address()`

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
  "pobox",
  "symbol",
  "unit",
  "unknown"
)

address_regex <- as.data.frame(matrix(nrow = length(rows), ncol = 3))

address_regex <- as.data.frame(apply(address_regex, 2, as.character))

rownames(address_regex) <- rows
colnames(address_regex) <- c("pattern", "pattern_replace", "replacement")



## embed_punct ####

# ### Original, with str_extract_all
# p <- paste0(
#   "(?x) ",           # Turn on free-spacing
#   "(?<=[:alnum:]) ", # Preceded by an alphanumeric
#   "[:punct:]+ ",     # One or more punctuation
#   "(?=[:alnum:])"    # Followed by an alphanumeric
# )

### Update, with str_match_all
p <- paste0(
  "(?x) ",                                    # Turn on free-spacing
  "(?:(?<=1)/(?=2)) | ",                      # Non-capturing group
  "((?<=[:alnum:]) [:punct:]+ (?=[:alnum:]))" # Capturing group
)

p2 <- paste0(
  "(?x) ",           # Turn on free-spacing
  "(\\s1/2)\\s | ",  # Capturing group 1
  "(?<=[:alnum:]) ", # Preceded by an alphanumeric
  "[:punct:]+ ",     # One or more punctuation
  "(?=[:alnum:])"    # Followed by an alphanumeric
)

r <- "\\1 "

address_regex["embed_punct",] <- c(p, p2, r)



## house_frac ####

p <- paste0(
  "(?x) ",               # Turn on free-spacing
  "(?<= \\d) ",          # Preceded by a digit
  "\\s+ (1 | ONE | ([:alpha:]* \\s* AND \\s+ [:alpha:]*))* \\s* HALF \\s* ",
  "(?= \\s [:alnum:]) ", # Followed by a space + alphanumeric
  "(?! \\s full)"        # Not followed by " Full" (as in the street name "Half Full")
)

address_regex["house_frac",] <- c(p, NA, " 1/2 ")



## nondigit ####

p <- "(?x) ^ \\D+ (?= \\d*)"

address_regex["nondigit",] <- c(p, NA, "")



## nonres ####

# p <- "(?x) ^ .* (9999.*address | 9999.*need | address.*need) .* $"
p <- "(?x) (9999.*address | 9999.*need | address.*need)"

address_regex["nonres",] <- c(p, NA, "")



## nth ####

p <- "(?x) \\b Nth \\b"

address_regex["nth",] <- c(p, NA, "N")



## num_dir ####

p <- paste0(
  "(?x) ",                # Turn on free-spacing
  "(?<= ^ \\d{1,1000}) ", # Preceded by initial digit(s)
  "[ENSW] \\b"            # Concatenated with a direction letter + word boundary
)

address_regex["num_dir",] <- c(p, NA, "")



## num_only ####

p <- "(?x) ^ \\d+ $"

address_regex["num_only",] <- c(p, NA, "")



## num_word ####

p <- paste0(
  "(?x) ",                # Turn on free-spacing
  "(?<= ^ \\d{1,1000}) ", # Preceded by initial digit(s)
  "[:alpha:]{2,}"         # Concatenated with a word
)

address_regex["num_word",] <- c(p, NA, "")



## numsign ####

p <- "#"

address_regex["numsign",] <- c(p, NA, " ")



## phone ####

p <- "\\d{7,}"

address_regex["phone",] <- c(p, NA, "")



## pobox ####

p. <- "P[[:punct:]\\s]*"
o. <- "O[[:punct:]\\s]*"
r. <- "R[[:punct:]\\s]*"
sub <- "[:alnum:][[:punct:]\\s]*"
boxnum1 <- "(#|NO)*[[:punct:]\\s]*\\d+"
boxnum2 <- "(#|NO)*[[:punct:]\\s]*\\d*"
boxnum3 <- "(#|NO)*[[:punct:]\\s]*\\d+[[:punct:]\\s]*"
designator <- paste0(
  "(", p., o., "BOX|",
  sub, o., "BOX|",
  p., sub, "BOX|",
  p., o., "[:alnum:]OX|",
  p., o., "B[:alnum:]X|",
  p., o., "BO[:alnum:])"
)

pbx1 <- paste0("((?<![:alnum:])", designator, "[[:punct:]\\s]*", boxnum1, ")")

pbx2 <- paste0("(", designator, "[[:punct:]\\s]*", boxnum1, "$)")

pbx3 <- paste0("(", designator, ")")

pbx4 <- paste0("((?<![:alnum:])", p., o., "B(?![:alpha:])", "[[:punct:]\\s]*", boxnum2, ")")

rbx5 <- paste0(
  "((?<![:alnum:])",
  "(", r., r., boxnum3, "BOX|",
  sub, r., boxnum3, "BOX|",
  r., sub, boxnum3, "BOX|",
  r., r., boxnum3, "[:alnum:]OX|",
  r., r., boxnum3, "B[:alnum:]X|",
  r., r., boxnum3, "BO[:alnum:])",
  "[[:punct:]\\s]*", boxnum1, ")"
)

bx6 <- paste0(
  "((?<![:alnum:])",
  "(BOX|",
  "[:alnum:]OX|",
  "B[:alnum:]X|",
  "BO[:alnum:])",
  "[[:punct:]\\s]*", boxnum1, ")"
)

p <- paste(c(pbx1, pbx2, pbx3, pbx4, rbx5, bx6), collapse = "|")

address_regex["pobox",] <- c(p, NA, "")



## symbol ####

p <- "[:symbol:]"

address_regex["symbol",] <- c(p, NA, "")



## unit ####

p <- paste0(
  "(?x) ",                    # Turn on free-spacing
  "(?<= \\s) ",               # Preceded by a space
  "(#|ap|apt|apartment|lot|", # Unit prefixes
  "no(?!rth)|num|number|rm|room|",
  "ste|suite|trlr|trailer|unit) ",
  "(\\s | [:punct:])* ",      # Spaces and/or punctuation marks
  "[:alnum:]+ $"              # Unit identifier, adjacent to end of string
)

address_regex["unit",] <- c(p, NA, "")



## unknown ####

# p <- "(?x) ^ .* unknown .* $"
p <- "unknown"

address_regex["unknown",] <- c(p, NA, "")



## Save ####

saveRDS(address_regex, "dev_aux/helpers/address_regex.rds")
