# Generates `address_regex.rda` used in `clean_address()`



## Nomenclature ####

## - CG = capturing group
## - NCG = non-capturing group



## Dataframe ####

rows <- c(
  "digit_letter",
  "dir_street",
  "emb_punct",
  "extract_unit",
  "fractional",
  "no_letters",
  "no_spaces",
  "nondigit",
  "nonres",
  "nth",
  "num_dir1",
  "num_dir2",
  "num_sign",
  "num_word",
  "phone",
  "pobox",
  "rep_letter",
  "rep_segment",
  "six_words",
  "symbol",
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



## Types ####



### digit_letter ####

## Unexpected digit and letter combination
## - Final pattern is intended to target a numbered street name followed by non-space characters other than the correct ordinal suffix (e.g., to capture "1th St" but not "1st St" or "1 St")

sfx <- c("11th", "12th", "13th", "1st", "2nd", "3rd", "[4-9]th", "0th")
sfx <- paste0("\\d{0,5}", sfx, "\\b", collapse = "|")
sfx <- paste0("(?!", sfx, "|\\d{1,5}\\b", ")")

pat1 <- paste0(
  "[:alpha:]+\\d+[:alpha:]+|", # Digit(s) surrounded by letters
  "\\d+[:alpha:]+\\d+|",       # Letter(s) surrounded by digits
  paste0("\\s", sfx, "\\d+")   # Space (not followed by a correctly formatted ordinal number) + digit(s)
)

pat2 <- "(^.*$)" # Replace with original string

r <- "\\1"

address_regex["digit_letter",] <- c(pat1, NA, pat2, r)



### dir_street ####

pat1 <- paste0(
  "(?:^\\d+\\s+)", # NCG: initial digit group and space
  "([NSEW]+)",     # CG: direction letter(s)
  "(?:\\d)"        # NCG: concatenated digit
)

pat2 <- paste0(
  "(^\\d+\\s+[NSEW]+)", # CG1: initial digit group, space, and direction
  "(\\d.*)"             # CG2: concatenated digit and everything else
)

r <- "\\1 \\2"

address_regex["dir_street",] <- c(pat1, 1, pat2, r)



### emb_punct ####

# Punctuation, except `-` and `'`
x <- "[ \\. , : ; \\? \\! / * @ \\# _ \" \\[ \\] \\{ \\} \\( \\) ]"
x <- paste0("(?<=[:alnum:])", x, "+(?=[:alnum:])")

pat1 <- paste0(
  "(?xx)",
  "(?:(?<=1)/(?=2))|", # NCG: "1/2"
  paste0("(", x, ")")  # CG: Punctuation surrounded by alphanumeric characters
)

pat2 <- paste0(
  "(?xx)",
  "(\\s1/2)\\s|", # CG1: " 1/2"
  x
)

r <- "\\1 " # When " 1/2" is captured, replace with itself (?)

address_regex["emb_punct",] <- c(pat1, 1, pat2, r)



### extract_unit ####

unit <- paste0(
  "apartment|basement|floor|lot|number|room|suite|trailer|unit|",
  "apt|ap|bsmt|fl|num|no(?!rth)|rm|ste|trlr|",
  "#"
)

sfx <- paste0(c(etmstuff::street_sfx$full, etmstuff::street_sfx$abbr), collapse = "|")

pat <- paste0(
  "(?<=\\s|,|-)",                                # Preceded by a space, comma, or hyphen
  "(", unit, ")",                                # Unit designator
  "(?![:alpha:]{2,})",                           # Not followed by 2+ letters
  "(?![[:alpha:]\\s]{1,}(", sfx, "))",           # Not followed by additional letters/spaces and a street suffix
  "(\\s|[:punct:])*",                            # Spaces and/or punctuation
  "([:alnum:]+$|[:alnum:]+(-|/|\\s)[:alnum:]+$)" # Unit identifier, adjacent to end of string
)

address_regex["extract_unit",] <- c(pat, NA, NA, "")



### fractional ####

pat <- paste0(
  "(?<=\\d)",         # Preceded by a digit
  "\\s+(1|ONE|([:alpha:]*\\s*AND\\s+[:alpha:]*))*\\s*HALF\\s*",
  "(?=\\s[:alnum:])", # Followed by a space + alphanumeric
  "(?!\\sFULL)"       # Not followed by " FULL" (as in the street name "HALF FULL")
)

address_regex["fractional",] <- c(pat, NA, NA, " 1/2 ")



### no_letters ####

pat <- "^[\\d\\s[:punct:]]+$"

address_regex["no_letters",] <- c(pat, NA, NA, "")



### no_spaces ####

pat <- "^[:graph:]+$" # Character string with no spaces

address_regex["no_spaces",] <- c(pat, NA, NA, "")



### nondigit ####

pat <- "^\\D+(?=\\d*)"

address_regex["nondigit",] <- c(pat, NA, NA, "")



### nonres ####

pat <- "^.*(9999.*address|9999.*need|address.*need).*$"

address_regex["nonres",] <- c(pat, NA, NA, "")



### nth ####

pat <- "(?x) \\b Nth \\b"

address_regex["nth",] <- c(pat, NA, NA, "N")



### num_dir1 ####

pat <- paste0(
  "(?<=^\\d{1,10})",    # Preceded by initial digit(s)
  "([NSEW][:punct:]*)", # CG: a direction letter (punctuation optional)
  "(?=\\s\\1\\s)"       # Followed by a space, the same letter, and a space
)

address_regex["num_dir1",] <- c(pat, NA, NA, "")



### num_dir2 ####

pat1 <- paste0(
  "(?<=^\\d{1,10})",  # Preceded by initial digit(s)
  "[NSEW][:punct:]*", # A direction letter (punctuation optional)
  "(?=\\s\\w)"        # Followed by a space and a word character
)

pat2 <- paste0(
  "(^\\d{1,10})",           # CG1: initial digit(s)
  "([NSEW][:punct:]*\\s.*)" # CG2: concatenated letter and everything else
)

r <- "\\1 \\2"

address_regex["num_dir2",] <- c(pat1, NA, pat2, r)



### num_sign ####

pat <- "#"

address_regex["num_sign",] <- c(pat, NA, NA, " ")



### num_word ####

pat <- paste0(
  "(?<=^\\d{1,10})", # Preceded by initial digit(s)
  "[:alpha:]{2,}"    # Concatenated with a word
)

address_regex["num_word",] <- c(pat, NA, NA, "")



### phone ####

pat <- "\\d{7,}"

address_regex["phone",] <- c(pat, NA, NA, "")



### pobox ####

x <- "[[:punct:]\\s]*"
alt <- paste0("[:alnum:]", x)

POBOX <- paste0(
  "(P", x, "O", x, "BOX|",
  alt, "O", x, "BOX|",
  "P", x, alt, "BOX|",
  "P", x, "O", x, "[:alnum:]OX|",
  "P", x, "O", x, "B[:alnum:]X|",
  "P", x, "O", x, "BO[:alnum:])"
)
NO <- paste0("(#|NO)*", x, "\\d+")
NOx <- paste0("(#|NO)*", x, "\\d*")

pbx1 <- paste0("((?<![:alnum:])", POBOX, x, NO, ")")
pbx2 <- paste0("(", POBOX, x, NO, "$)")
pbx3 <- paste0("(", POBOX, ")")
pbx4 <- paste0("((?<![:alnum:])", "P", x, "O", x, "B(?![:alpha:])", x, NOx, ")")
rbx <- paste0(
  "((?<![:alnum:])",
  "(R", x, "R", x, NO, x, "BOX|",
  alt, "R", x, NO, x, "BOX|",
  "R", x, alt, NO, x, "BOX|",
  "R", x, "R", x, NO, x, "[:alnum:]OX|",
  "R", x, "R", x, NO, x, "B[:alnum:]X|",
  "R", x, "R", x, NO, x, "BO[:alnum:])",
  x, NO, ")"
)
bx <- paste0(
  "((?<![:alnum:])",
  "(BOX|",
  "[:alnum:]OX|",
  "B[:alnum:]X|",
  "BO[:alnum:])",
  x, NO, ")"
)

pat <- paste(c(pbx1, pbx2, pbx3, pbx4, rbx, bx), collapse = "|")

address_regex["pobox",] <- c(pat, NA, NA, "")



### rep_letter ####

pat <- "([:alpha:])\\1{2,}" # 3+ occurrences of the same letter

r <- "\\1\\1" # Replace with two occurrences

address_regex["rep_letter",] <- c(pat, NA, NA, r)



### rep_segment ####

w <- "\\w+"
s <- "\\s+"
ws <- "\\w+\\s+"
ncg <- "(?:\\w+\\s+){0,3}"

pat1 <- paste0(
  "\\b(", ws, ws, ws, ws, w, ")", s, ncg, "\\1\\b|", # CG1: 5-word string. NCG: other word(s).
  "\\b(", ws, ws, ws, w, ")", s, ncg, "\\2\\b|",     # CG2: 4-word string. NCG: other word(s).
  "\\b(", ws, ws, w, ")", s, ncg, "\\3\\b|",         # CG3: 3-word string. NCG: other word(s).
  "\\b(", ws, w, ")", s, ncg, "\\4\\b"               # CG4: 2-word string. NCG: other word(s).
)

pat2 <- paste0(
  "\\b((", ws, ws, ws, ws, w, ")", s, ncg, ")\\2\\b|", # CG1: (CG2: 5-word string + NCG: other word(s))
  "\\b((", ws, ws, ws, w, ")", s, ncg, ")\\4\\b|",     # CG3: (CG4: 4-word string + NCG: other word(s))
  "\\b((", ws, ws, w, ")", s, ncg, ")\\6\\b|",         # CG5: (CG6: 3-word string + NCG: other word(s))
  "\\b((", ws, w, ")", s, ncg, ")\\8\\b"               # CG7: (CG8: 2-word string + NCG: other word(s))
)

r <- "\\1\\3\\5\\7"

address_regex["rep_segment",] <- c(pat1, 4, pat2, r)



### six_words ####

pat1 <- "(?x) (\\S+\\s){5,} \\S+" # 6+ words

pat2 <- "(^.*$)" # Replace with original string

r <- "\\1"

address_regex["six_words",] <- c(pat1, NA, pat2, r)



### symbol ####

pat <- "[:symbol:]"

address_regex["symbol",] <- c(pat, NA, NA, "")



### unknown ####

# pat <- "^.*unknown.*$"
pat <- "unknown"

address_regex["unknown",] <- c(pat, NA, NA, "")



## Save ####

address_regex$n_cap_gps <- as.numeric(address_regex$n_cap_gps)

usethis::use_data(address_regex, overwrite = T)
