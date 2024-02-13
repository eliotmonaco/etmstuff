# Explore and model the distribution of house numbers in addresses after real numbers taken from validated Melissa Data addresses in lead records from 2015-2022

library(tidyverse)

## Import addresses
df_md1 <- readRDS("~/r_projects/bl_2015-2022/data/addresses/df_md_val1_2015-2019.rds")
df_md2 <- readRDS("~/r_projects/bl_2015-2022/data/addresses/df_md_val1_2020-2022.rds")

df_md <- df_md1 %>%
  bind_rows(df_md2)

rm(df_md1, df_md2)

## Deduplicate addresses
df_md <- df_md %>%
  distinct(pick(AddressDeliveryInstallation:Suite), .keep_all = T)

## Pull house numbers
hn <- sort(as.numeric(stringr::str_extract(df_md$AddressLine1, "^\\d+")))

## Plot distribution of house numbers
plotly::plot_ly(
  x = hn,
  type = "histogram",
  xbins = list(size = 1)
) %>%
  plotly::layout(
    title = "Distribution of house numbers (actual)",
    xaxis = list(
      title = "House numbers"
    ),
    yaxis = list(
      title = "Frequency"
    )
  )

## Find outliers
sum(nchar(hn) == 6)
sum(hn >= 40000)

## Plot distribution of house numbers below 40000
hn40k <- hn[hn < 40000]

plotly::plot_ly(
  x = hn40k,
  type = "histogram",
  xbins = list(size = 1)
) %>%
  plotly::layout(
    title = "Distribution of house numbers < 40000 (actual)",
    xaxis = list(
      title = "House numbers",
      range = list(0, 1999)
    ),
    yaxis = list(
      title = "Frequency"
    )
  )

## Zoom in on distributions within intervals of 100
min <- 300
max <- min + 99

plotly::plot_ly(
  x = hn[hn >= min & hn <= max],
  type = "histogram",
  xbins = list(size = 1)
) %>%
  plotly::layout(
    title = paste("Distribution of house numbers:", min, "to", max, "(actual)")
  )

## Model the distribution of house numbers within intervals of 100
min <- 300
max <- min + 99
hn_size <- length(hn[hn >= min & hn <= max])

m <- 2.7
sd <- 1.2
p <- dlnorm(1:100, meanlog = m, sdlog = sd)

samp <- sample(min:max, hn_size, replace = T, prob = p)

plotly::plot_ly(
  x = samp,
  type = "histogram",
  xbins = list(size = 1)
) %>%
  plotly::layout(
    title = paste("Distribution of house numbers:", min, "to", max, "(modeled)"),
    xaxis = list(
      title = paste("meanlog = ", m, ", sdlog = ", sd)
    )
  )

## Get counts of house numbers within each interval of 100
int_seq <- seq(0, 40000, by = 100)

bincounts <- list()

for (i in 1:length(int_seq) - 1) {
  min <- int_seq[i]
  max <- int_seq[i + 1]
  x <- hn[hn > min & hn < max]
  bincounts[i] <- length(x)
}

bincounts <- unlist(bincounts)

plot(bincounts, main = "Counts of house numbers within each interval of 100 (actual)")

## Model bincounts
m <- 2.6
sd <- 1.2
p <- dlnorm(1:400, meanlog = m, sdlog = sd)

samp <- sample(1:400, length(hn40k), replace = T, prob = p)

bincounts <- tabulate(samp)

plot(bincounts, main = "Counts of house numbers within each interval of 100 (modeled)")

## Function to model the distribution of house numbers
model_house_num_dist <- function(sample_size) {
  # Create intervals of 100 for house numbers
  hn_max <- 40000
  int_size <- 100
  n_int <- hn_max / int_size
  int_seq1 <- 1:n_int
  int_seq2 <- seq(0, hn_max, by = int_size)

  # Create bin count for each house number interval
  m <- 2.7
  sd <- 1.2
  p <- dlnorm(int_seq1, meanlog = m, sdlog = sd)
  samp <- sample(int_seq1, sample_size, replace = TRUE, prob = p)
  bincounts <- tabulate(samp)

  # Add zero counts for any missing bins at the end of `bincounts`
  n_zeros <- 400 - length(bincounts)
  bincounts <- c(bincounts, rep(0, times = n_zeros))

  # Create distribution for each house number interval
  dist <- list()
  for (i in 1:(length(int_seq2) - 1)) {
    min <- int_seq2[i]
    max <- int_seq2[i + 1] - 1
    m <- 2.6
    sd <- 1.2
    p <- dlnorm(1:100, meanlog = m, sdlog = sd)
    dist[[i]] <- sample(min:max, size = bincounts[i], replace = TRUE, prob = p)
  }

  unlist(dist)
}

samp <- model_house_num_dist(length(hn40k))

plotly::plot_ly(
  x = samp,
  type = "histogram",
  xbins = list(size = 1)
) %>%
  plotly::layout(
    title = "Distribution of house numbers < 40000 (modeled)",
    xaxis = list(
      title = "House numbers"
    ),
    yaxis = list(
      title = "Frequency"
    )
  )

## Plot 0-1999 only
plotly::plot_ly(
  x = samp,
  type = "histogram",
  xbins = list(size = 1)
) %>%
  plotly::layout(
    title = "Distribution of house numbers < 40000 (modeled)",
    xaxis = list(
      title = "House numbers",
      range = list(0, 1999)
    ),
    yaxis = list(
      title = "Frequency"
    )
  )
