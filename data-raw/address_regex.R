# Generates `address_regex` used in `clean_street_address()`



## Dataframe ####

rows <- c(
  "digit_char",
  "dir_street",
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
  "rep_seg",
  "symbol",
  "unit",
  "unknown"
)

address_regex <- as.data.frame(
  list(
    pattern = rep(NA, length(rows)),
    n_cap_gps = rep(NA, length(rows)),
    pattern_replace = rep(NA, length(rows)),
    replacement = rep(NA, length(rows))
  ),
  row.names = rows
)



## digit_char ####

p <- paste0(
  "(?x) ",                              # Turn on free-spacing
  "(?<=\\d{1,10}.{1,50}\\b",            # Preceded by digit(s), other characters, a word boundary...
  "\\d{1,10}(?!st|nd|rd|th|\\d|\\b)) ", # ...digits not followed by expected characters or a word boundary
  "[:graph:]* ",                        # Letters/digits/punctuation
  "(?=\\b)"                             # Followed by a word boundary
)

address_regex["digit_char",] <- c(p, NA, NA, "")



## dir_street ####

p <- paste0(
  "(?x) ",          # Turn on free-spacing
  "(?:^\\d+\\s+) ", # Non-capturing group: initial digit group and space
  "([NSEW]+) ",     # Capturing group: direction
  "(?:\\d)"         # Non-capturing group: concatenated digit
)

p2 <- paste0(
  "(?x) ",               # Turn on free-spacing
  "(^\\d+\\s+[NSEW]+) ", # Capturing group 1: initial digit group, space, and direction
  "(\\d.*)"              # Capturing group 2: concatenated digit and everything else
)

r <- "\\1 \\2"

address_regex["dir_street",] <- c(p, 1, p2, r)



## embed_punct ####

p <- paste0(
  "(?x) ",                                  # Turn on free-spacing
  "(?:(?<=1)/(?=2)) | ",                    # Non-capturing group: "1/2"
  "((?<=[:alnum:])[:punct:]+(?=[:alnum:]))" # Capturing group
)

p2 <- paste0(
  "(?x) ",           # Turn on free-spacing
  "(\\s1/2)\\s | ",  # Capturing group 1
  "(?<=[:alnum:]) ", # Preceded by an alphanumeric
  "[:punct:]+ ",     # One or more punctuation
  "(?=[:alnum:])"    # Followed by an alphanumeric
)

r <- "\\1 "

address_regex["embed_punct",] <- c(p, 1, p2, r)



## house_frac ####

p <- paste0(
  "(?x) ",             # Turn on free-spacing
  "(?<=\\d) ",         # Preceded by a digit
  "\\s+ (1|ONE|([:alpha:]*\\s*AND\\s+[:alpha:]*))* \\s*HALF\\s* ",
  "(?=\\s[:alnum:]) ", # Followed by a space + alphanumeric
  "(?!\\sfull)"        # Not followed by " Full" (as in the street name "Half Full")
)

address_regex["house_frac",] <- c(p, NA, NA, " 1/2 ")



## nondigit ####

p <- "(?x) ^\\D+ (?=\\d*)"

address_regex["nondigit",] <- c(p, NA, NA, "")



## nonres ####

p <- "(?x) ^.* (9999.*address|9999.*need|address.*need) .*$"

address_regex["nonres",] <- c(p, NA, NA, "")



## nth ####

p <- "(?x) \\b Nth \\b"

address_regex["nth",] <- c(p, NA, NA, "N")



## num_dir ####

p <- paste0(
  "(?x) ",                # Turn on free-spacing
  "(?<=^\\d{1,1000}) ", # Preceded by initial digit(s)
  "[NSEW]\\b"            # Concatenated with a direction letter + word boundary
)

address_regex["num_dir",] <- c(p, NA, NA, "")



## num_only ####

p <- "(?x) ^\\d+$"

address_regex["num_only",] <- c(p, NA, NA, "")



## num_word ####

p <- paste0(
  "(?x) ",              # Turn on free-spacing
  "(?<=^\\d{1,1000}) ", # Preceded by initial digit(s)
  "[:alpha:]{2,}"       # Concatenated with a word
)

address_regex["num_word",] <- c(p, NA, NA, "")



## numsign ####

p <- "#"

address_regex["numsign",] <- c(p, NA, NA, " ")



## phone ####

p <- "\\d{7,}"

address_regex["phone",] <- c(p, NA, NA, "")



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

address_regex["pobox",] <- c(p, NA, NA, "")



## rep_seg ####

p <- paste0(
  "(?x) ",
  "\\b (\\w+\\s+\\w+\\s+\\w+\\s+\\w+\\s+\\w+) \\s+ (?:\\w+\\s+){0,3} \\1 \\b | ", # CG1: 5-word string. NCG: other word(s).
  "\\b (\\w+\\s+\\w+\\s+\\w+\\s+\\w+) \\s+ (?:\\w+\\s+){0,3} \\2 \\b | ",         # CG2: 4-word string. NCG: other word(s).
  "\\b (\\w+\\s+\\w+\\s+\\w+) \\s+ (?:\\w+\\s+){0,3} \\3 \\b | ",                 # CG3: 3-word string. NCG: other word(s).
  "\\b (\\w+\\s+\\w+) \\s+ (?:\\w+\\s+){0,3} \\4 \\b"                             # CG4: 2-word string. NCG: other word(s).
)

p2 <- paste0(
  "(?x) ",
  "\\b ((\\w+\\s+\\w+\\s+\\w+\\s+\\w+\\s+\\w+) \\s+ (?:\\w+\\s+){0,3}) \\2 \\b | ", # CG1: (CG2: 5-word string + NCG: other word(s))
  "\\b ((\\w+\\s+\\w+\\s+\\w+\\s+\\w+) \\s+ (?:\\w+\\s+){0,3}) \\4 \\b | ",         # CG3: (CG4: 4-word string + NCG: other word(s))
  "\\b ((\\w+\\s+\\w+\\s+\\w+) \\s+ (?:\\w+\\s+){0,3}) \\6 \\b | ",                 # CG5: (CG6: 3-word string + NCG: other word(s))
  "\\b ((\\w+\\s+\\w+) \\s+ (?:\\w+\\s+){0,3}) \\8 \\b"                             # CG7: (CG8: 2-word string + NCG: other word(s))
)

r <- "\\1\\3\\5\\7"

address_regex["rep_seg",] <- c(p, 4, p2, r)



## symbol ####

p <- "[:symbol:]"

address_regex["symbol",] <- c(p, NA, NA, "")



## unit ####

p <- paste0(
  # "(?x) ",                       # Turn on free-spacing
  "(?<=\\s)",                      # Preceded by a space
  "(#|ap|apt|apartment|lot|",      # Unit designator
  "no(?!rth)|num|number|rm|room|",
  "ste|suite|trlr|trailer|unit)",
  "(?![:alpha:]{2,}$)",            # Not followed by 2+ letters then end of string
  "(\\s|[:punct:])*",              # Spaces and/or punctuation marks
  "[:alnum:]+$"                    # Unit identifier, adjacent to end of string
)

address_regex["unit",] <- c(p, NA, NA, "")



## unknown ####

p <- "(?x) ^.*unknown.*$"

address_regex["unknown",] <- c(p, NA, NA, "")



## Save ####

address_regex$n_cap_gps <- as.numeric(address_regex$n_cap_gps)

# saveRDS(address_regex, "dev_aux/helpers/address_regex.rds")

usethis::use_data(address_regex, overwrite = T)
