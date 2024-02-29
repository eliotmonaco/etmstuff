# Generates `address_regex.rda`


# Setup ####

## - CG = capturing group
## - NCG = non-capturing group

address_regex <- list()


# digit_letter ####
## Unexpected digit and letter combination
## - Final pattern is intended to target a numbered street name followed by non-space characters other than the correct ordinal suffix (e.g., to capture "1th St" but not "1st St" or "1 St")

type <- "digit_letter"

sfx <- c("11th", "12th", "13th", "1st", "2nd", "3rd", "[4-9]th", "0th")
sfx <- paste0("\\d{0,5}", sfx, "\\b", collapse = "|")
sfx <- paste0("(?!", sfx, "|\\d{1,5}\\b", ")")

p_search <- paste0(
  "[:alpha:]+\\d+[:alpha:]+|", # Digit(s) surrounded by letters
  "\\d+[:alpha:]+\\d+|",       # Letter(s) surrounded by digits
  paste0("\\s", sfx, "\\d+")   # Space (not followed by a correctly formatted ordinal number) + digit(s)
)

n_cap <- NA

p_replace <- "(^.*$)" # Replace with original string

repl <- "\\1"

address_regex[[type]] <- c(type, p_search, n_cap, p_replace, repl)


# dir_street ####
type <- "dir_street"

p_search <- paste0(
  "(?:^\\d+\\s+)", # NCG: initial digit group and space
  "([NSEW]+)",     # CG: direction letter(s)
  "(?:\\d)"        # NCG: concatenated digit
)

n_cap <- 1

p_replace <- paste0(
  "(^\\d+\\s+[NSEW]+)", # CG1: initial digit group, space, and direction
  "(\\d.*)"             # CG2: concatenated digit and everything else
)

repl <- "\\1 \\2"

address_regex[[type]] <- c(type, p_search, n_cap, p_replace, repl)


# emb_punct ####
type <- "emb_punct"

# Punctuation, except `-` and `'`
x <- "[ \\. , : ; \\? \\! / * @ \\# _ \" \\[ \\] \\{ \\} \\( \\) ]"
x <- paste0("(?<=[:alnum:])", x, "+(?=[:alnum:])")

p_search <- paste0(
  "(?xx)",
  "(?:(?<=1)/(?=2))|", # NCG: "1/2"
  paste0("(", x, ")")  # CG: Punctuation surrounded by alphanumeric characters
)

n_cap <- 1

p_replace <- paste0(
  "(?xx)",
  "(\\s1/2)\\s|", # CG1: " 1/2"
  x
)

repl <- "\\1 " # When " 1/2" is captured, replace with itself (?)

address_regex[[type]] <- c(type, p_search, n_cap, p_replace, repl)


# extract_unit ####
type <- "extract_unit"

# unit <- paste0(
#   "apartment|basement|building|floor|lot|number|room|suite|trailer|unit|",
#   "apt|ap|bsmt|bldg|fl|num|no(?!rth)|rm|ste|trlr|",
#   "#"
# )

unit <- na.omit(c(etmstuff::unit_pfx$full, etmstuff::unit_pfx$abbr, c("ap", "no(?!rth)")))
unit <- paste0(unit, collapse = "|")

sfx <- na.omit(c(etmstuff::street_sfx$full, etmstuff::street_sfx$abbr))
sfx <- paste0(sfx, collapse = "|")

p_search <- paste0(
  "(?<=\\s|,|-)",                                # Preceded by a space, comma, or hyphen
  "(", unit, ")",                                # Unit designator
  "(?![:alpha:]{2,})",                           # Not followed by 2+ letters
  "(?![[:alpha:]\\s]{1,}(", sfx, "))",           # Not followed by additional letters/spaces and a street suffix
  "(\\s|[:punct:])*",                            # Spaces and/or punctuation
  "([:alnum:]+$|[:alnum:]+(-|/|\\s)[:alnum:]+$)" # Unit identifier, adjacent to end of string
)

n_cap <- NA; p_replace <- NA; repl <- ""

address_regex[[type]] <- c(type, p_search, n_cap, p_replace, repl)

# extract_unit_current <- "(?<=\\s|,|-)(apartment|basement|building|floor|lot|number|room|suite|trailer|unit|#|apt|bsmt|bldg|fl|num|rm|ste|trlr|ap|no(?!rth))(?![:alpha:]{2,})(?![[:alpha:]\\s]{1,}(Avenue|Boulevard|Circle|Court|Crossing|Drive|Highway|Lane|Parkway|Place|Road|Street|Terrace|Trafficway|Trail|Way|Ave|Blvd|Cir|Ct|Xing|Dr|Hwy|Ln|Pkwy|Pl|Rd|St|Ter|Trfy|Trl))(\\s|[:punct:])*([:alnum:]+$|[:alnum:]+(-|/|\\s)[:alnum:]+$)"

# extract_unit_old <- "(?<=\\s|,|-)(apartment|basement|building|floor|lot|number|room|suite|trailer|unit|apt|ap|bsmt|bldg|fl|num|no(?!rth)|rm|ste|trlr|#)(?![:alpha:]{2,})(?![[:alpha:]\\s]{1,}(Avenue|Boulevard|Drive|Lane|Place|Road|Street|Terrace|Ave|Blvd|Dr|Ln|Pl|Rd|St|Ter))(\\s|[:punct:])*([:alnum:]+$|[:alnum:]+(-|/|\\s)[:alnum:]+$)"


# fractional ####
type <- "fractional"

p_search <- paste0(
  "(?<=\\d)",         # Preceded by a digit
  "\\s+(1|ONE|([:alpha:]*\\s*AND\\s+[:alpha:]*))*\\s*HALF\\s*",
  "(?=\\s[:alnum:])", # Followed by a space + alphanumeric
  "(?!\\sFULL)"       # Not followed by " FULL" (as in the street name "HALF FULL")
)

n_cap <- NA; p_replace <- NA

repl <- " 1/2 "

address_regex[[type]] <- c(type, p_search, n_cap, p_replace, repl)


# no_letters ####
type <- "no_letters"

p_search <- "^[\\d\\s[:punct:]]+$"

n_cap <- NA; p_replace <- NA; repl <- ""

address_regex[[type]] <- c(type, p_search, n_cap, p_replace, repl)


# no_spaces ####
type <- "no_spaces"

p_search <- "^[:graph:]+$" # Character string with no spaces

n_cap <- NA; p_replace <- NA; repl <- ""

address_regex[[type]] <- c(type, p_search, n_cap, p_replace, repl)


# nondigit ####
type <- "nondigit"

p_search <- "^\\D+(?=\\d*)"

n_cap <- NA; p_replace <- NA; repl <- ""

address_regex[[type]] <- c(type, p_search, n_cap, p_replace, repl)


# nonres ####
type <- "nonres"

p_search <- "^.*(9999.*address|9999.*need|address.*need).*$"

n_cap <- NA; p_replace <- NA; repl <- ""

address_regex[[type]] <- c(type, p_search, n_cap, p_replace, repl)


# nth ####
type <- "nth"

p_search <- "(?x) \\b Nth \\b"

n_cap <- NA; p_replace <- NA

repl <- "N"

address_regex[[type]] <- c(type, p_search, n_cap, p_replace, repl)


# num_dir1 ####
type <- "num_dir1"

p_search <- paste0(
  "(?<=^\\d{1,10})",    # Preceded by initial digit(s)
  "([NSEW][:punct:]*)", # CG: a direction letter (punctuation optional)
  "(?=\\s\\1\\s)"       # Followed by a space, the same letter, and a space
)

n_cap <- NA; p_replace <- NA; repl <- ""

address_regex[[type]] <- c(type, p_search, n_cap, p_replace, repl)


# num_dir2 ####
type <- "num_dir2"

p_search <- paste0(
  "(?<=^\\d{1,10})",  # Preceded by initial digit(s)
  "[NSEW][:punct:]*", # A direction letter (punctuation optional)
  "(?=\\s\\w)"        # Followed by a space and a word character
)

n_cap <- NA

p_replace <- paste0(
  "(^\\d{1,10})",           # CG1: initial digit(s)
  "([NSEW][:punct:]*\\s.*)" # CG2: concatenated letter and everything else
)

repl <- "\\1 \\2"

address_regex[[type]] <- c(type, p_search, n_cap, p_replace, repl)


# num_sign ####
type <- "num_sign"

p_search <- "#"

n_cap <- NA; p_replace <- NA

repl <- " "

address_regex[[type]] <- c(type, p_search, n_cap, p_replace, repl)


# num_word ####
type <- "num_word"

p_search <- paste0(
  "(?<=^\\d{1,10})", # Preceded by initial digit(s)
  "[:alpha:]{2,}"    # Concatenated with a word
)

n_cap <- NA; p_replace <- NA; repl <- ""

address_regex[[type]] <- c(type, p_search, n_cap, p_replace, repl)


# phone ####
type <- "phone"

p_search <- "\\d{7,}"

n_cap <- NA; p_replace <- NA; repl <- ""

address_regex[[type]] <- c(type, p_search, n_cap, p_replace, repl)


# pobox ####
type <- "pobox"

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

p_search <- paste(c(pbx1, pbx2, pbx3, pbx4, rbx, bx), collapse = "|")

n_cap <- NA; p_replace <- NA; repl <- ""

address_regex[[type]] <- c(type, p_search, n_cap, p_replace, repl)


# rep_letter ####
type <- "rep_letter"

p_search <- "([:alpha:])\\1{2,}" # 3+ occurrences of the same letter

n_cap <- NA; p_replace <- NA

repl <- "\\1\\1" # Replace with two occurrences

address_regex[[type]] <- c(type, p_search, n_cap, p_replace, repl)


# rep_segment ####
type <- "rep_segment"

w <- "\\w+"
s <- "\\s+"
ws <- "\\w+\\s+"
ncg <- "(?:\\w+\\s+){0,3}"

p_search <- paste0(
  "\\b(", ws, ws, ws, ws, w, ")", s, ncg, "\\1\\b|", # CG1: 5-word string. NCG: other word(s).
  "\\b(", ws, ws, ws, w, ")", s, ncg, "\\2\\b|",     # CG2: 4-word string. NCG: other word(s).
  "\\b(", ws, ws, w, ")", s, ncg, "\\3\\b|",         # CG3: 3-word string. NCG: other word(s).
  "\\b(", ws, w, ")", s, ncg, "\\4\\b"               # CG4: 2-word string. NCG: other word(s).
)

n_cap <- 4

p_replace <- paste0(
  "\\b((", ws, ws, ws, ws, w, ")", s, ncg, ")\\2\\b|", # CG1: (CG2: 5-word string + NCG: other word(s))
  "\\b((", ws, ws, ws, w, ")", s, ncg, ")\\4\\b|",     # CG3: (CG4: 4-word string + NCG: other word(s))
  "\\b((", ws, ws, w, ")", s, ncg, ")\\6\\b|",         # CG5: (CG6: 3-word string + NCG: other word(s))
  "\\b((", ws, w, ")", s, ncg, ")\\8\\b"               # CG7: (CG8: 2-word string + NCG: other word(s))
)

repl <- "\\1\\3\\5\\7"

address_regex[[type]] <- c(type, p_search, n_cap, p_replace, repl)


# six_words ####
type <- "six_words"

p_search <- "(?x) (\\S+\\s){5,} \\S+" # 6+ words

n_cap <- NA

p_replace <- "(^.*$)" # Replace with original string

repl <- "\\1"

address_regex[[type]] <- c(type, p_search, n_cap, p_replace, repl)


# symbol ####
type <- "symbol"

p_search <- "[:symbol:]"

n_cap <- NA; p_replace <- NA; repl <- ""

address_regex[[type]] <- c(type, p_search, n_cap, p_replace, repl)


# unknown ####
type <- "unknown"

p_search <- "unknown"

n_cap <- NA; p_replace <- NA; repl <- ""

address_regex[[type]] <- c(type, p_search, n_cap, p_replace, repl)


# Join as dataframe and save ####

address_regex <- tibble::as_tibble(address_regex)
address_regex <- tibble::as_tibble(t(address_regex))

colnames(address_regex) <- c("type", "search_pattern", "n_cap_gps", "replace_pattern", "replacement")

address_regex <- address_regex |>
  dplyr::arrange(type) |>
  dplyr::mutate(n_cap_gps = as.numeric(n_cap_gps))

usethis::use_data(address_regex, overwrite = T)
