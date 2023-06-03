#### EpiTrax search and scrape functions ####

## Packages used: RSelenium, rvest, tcltk, svDialogs, stringr

# Open a browser and log into EpiTrax
epi_init <- function() {
  
  suppressMessages({
    library(RSelenium)
    library(rvest)
  })
  
  # browser()
  rD <- rsDriver(browser = "chrome", port = 1111L, chromever = "111.0.5563.64")
  # rD <- rsDriver(browser = "chrome", verbose = T, check = F)
  # rD <- rsDriver(browser = "firefox", check = F)
  remDr <- rD$client
  remDr$navigate("http://ksepitrax.org/nedss/login/")
  remDr$findElements(using = "class", value = "ui-button")[[1]]$clickElement()
  remDr$findElement(using = "id", value = "username")$sendKeysToElement(list("TPKHEETM"))
  remDr$findElement(using = "id", value = "password")$sendKeysToElement(list("NelP6IUN2e&@WSI^O#Ga"))
  remDr$findElements(using = "name", value = "submit")[[1]]$clickElement()
  
  assign("rD", rD, envir = .GlobalEnv)
  assign("remDr", remDr, envir = .GlobalEnv)
  
}

# Search CMR from each record (opens a new tab)
epi_search <- function(df, row, slp1=12, slp2=2) {
  
  # Search CMR
  remDr$findElements(using = "link text", value = "CMR SEARCH")[[1]]$clickElement()
  remDr$findElement(using = "id", value = "cmrForm:record_number")$sendKeysToElement(list(df$patient_record_number[row]))
  remDr$findElements(using = "id", value = "cmrForm:j_idt224")[[1]]$clickElement()
  Sys.sleep(1)
  remDr$findElements(using = "class", value = "app_link")[[1]]$clickElement()
  Sys.sleep(slp1)
  
  # Switch focus to the tab opened above
  qpath <- sprintf("%s/session/%s/window", remDr$serverURL, remDr$sessionInfo[["id"]])
  remDr$queryRD(qpath, "POST", qdata = list(handle = remDr$getWindowHandles()[[2]]))
  
  # Go to Edit Person page
  web_elem <- remDr$findElements(using = "css selector", value = ".ui-menu-list.ui-helper-reset > li > a")
  remDr$mouseMoveToLocation(webElement = web_elem[[1]])
  Sys.sleep(1)
  remDr$findElements(using = "css selector", value = ".ui-shadow :nth-child(8) .ui-menuitem-text")[[1]]$clickElement()
  Sys.sleep(slp2)
  remDr$acceptAlert()
  
}

# Prompt user for input
epi_prompt <- function(df_fr, row, var1, var2, df_to, nm_df_to) {
  
  recno <- df_fr$recno[row]
  address1 <- paste("Address 1:", df_fr[row, var1])
  address2 <- paste("Address 2:", df_fr[row, var2])
  
  response1 <- tcltk::tk_messageBox(message = paste0("Update the address in row ", row, "?\n\n",
                                                     address1, "\n",
                                                     address2),
                                    type = "yesnocancel")
  
  if (response1 == "yes") {
    response2 <- svDialogs::dlg_input("From which section?")$res
    section <- as.numeric(response2)
    return <- epi_pull(section)
    response3 <- svDialogs::dlg_input(paste0("All address components must be separated by semicolons. Leave a ",
                                             "blank space in between if a component is missing."),
                                      default = paste0(paste(return[1], ifelse(is.na(return[2]), "", return[2]), return[3],
                                                             return[4], return[5], return[6],
                                                             sep = " | "), " | "))$res
    input <- unlist(strsplit(response3, "|", fixed = T))
    df_to <- epi_add(df_to, recno, input)
    # nm <- deparse(substitute(df_to))
    assign(nm_df_to, df_to, envir = .GlobalEnv)
    assign("outcome", "save", envir = .GlobalEnv)
    epi_advance(row)
  } else if (response1 == "no") {
    assign("outcome", "continue", envir = .GlobalEnv)
    epi_advance(row)
  } else if (response1 == "cancel") {
    assign("outcome", "cancel", envir = .GlobalEnv)
    epi_advance(row)
    return(message("Search cancelled."))
  }
  
}

# Pull an address from EpiTrax
epi_pull <- function(section) {
  
  html <- read_html(remDr$getPageSource()[[1]])
  return <- ""
  
  for (i in c(1:3, 5)) {
    str_css <- paste0(".ui-tabs-panels > div:nth-child(1) > div:nth-child(7) > div:nth-child(",
                      section + 3, ") > ",
                      "table:nth-child(1) > tbody > tr > td > div > div > table:nth-child(",
                      i, ") > ",
                      "tbody > tr:nth-child(2) > td > input")
    addr_comp <- html %>%
      html_elements(str_css) %>%
      html_attr("value")
    return[i] <- addr_comp
  }
  
  for (i in c(4, 6)) {
    str_css <- paste0(".ui-tabs-panels > div:nth-child(1) > div:nth-child(7) > div:nth-child(",
                      section + 3, ") > ",
                      "table:nth-child(1) > tbody > tr > td > div > div > table:nth-child(",
                      i, ") > ",
                      "tbody > tr:nth-child(2) > td > div > label")
    addr_comp <- html %>%
      html_elements(str_css) %>%
      html_text2()
    return[i] <- addr_comp
  }
  
  return
  
}

# Add the pulled address to a DF
epi_add <- function(df, recno, input) {
  
  i <- nrow(df) + 1
  df[i, 1] <- recno
  
  for (j in 1:7) {
    if (!is.na(input[j])) {
      df[i, j + 1] <- stringr::str_trim(input[j])
    }
  }
  
  df
  
}

# Close the new tab and advance to the next row
epi_advance <- function(row) {
  
  remDr$closeWindow()
  qpath <- sprintf("%s/session/%s/window", remDr$serverURL, remDr$sessionInfo[["id"]])
  remDr$queryRD(qpath, "POST", qdata = list(handle = remDr$getWindowHandles()[[1]]))
  row <- row + 1
  assign("r", row, envir = .GlobalEnv)
  
}

# Close browser and server
epi_close <- function() {
  
  remDr$close()
  rD$server$stop()
  try(
    system("taskkill /im java.exe /f", intern = F, ignore.stdout = F),
    silent = T
  )
  
}

#### ~----



#### USPS search function ####

## Packages used: RSelenium, rvest

# Searches addresses supplied by DF and returns the top result
search_usps <- function(df) {
  
  df$usps_addr1 <- ""
  df$usps_addr2 <- ""
  df$usps_dpv <- ""
  
  pb <- txtProgressBar(1, nrow(df), width = 50, style = 3)
  
  rD <- rsDriver(browser = "chrome", port = 1111L, chromever = "104.0.5112.79")
  remDr <- rD$client
  
  for (i in 1:nrow(df)) {
    remDr$navigate("https://tools.usps.com/zip-code-lookup.htm?byaddress")
    remDr$findElement(using = "id", value = "tAddress")$sendKeysToElement(list(df$street[i]))
    remDr$findElement(using = "id", value = "tApt")$sendKeysToElement(list(df$unit[i]))
    remDr$findElement(using = "id", value = "tCity")$sendKeysToElement(list(df$city[i]))
    remDr$findElement(using = "id", value = "tState")$sendKeysToElement(list(df$state[i]))
    remDr$findElements(using = "id", value = "zip-by-address")[[1]]$clickElement()
    Sys.sleep(2)
    setTxtProgressBar(pb, i)
    html <- read_html(remDr$getPageSource()[[1]])
    # Pull street address result
    return1 <- html %>%
      html_elements("#zipByAddressDiv > ul > li:nth-child(1) > div.zipcode-result-address > p:nth-child(2)") %>%
      html_text2()
    if (length(return1) != 0) {df$usps_addr1[i] <- return1} else {next}
    # Pull city/state/zip result
    return2 <- html %>%
      html_elements("#zipByAddressDiv > ul > li:nth-child(1) > div > p:nth-child(3)") %>%
      html_text2()
    if (length(return2) != 0) {df$usps_addr2[i] <- return2} else {next}
    # Pull Delivery Point Validation indicator
    return3 <- html %>%
      html_elements("#zipByAddressDiv > ul > li:nth-child(1) > div.address-detail-info-wrapper > div:nth-child(5) > div:nth-child(2) > div:nth-child(2) > p") %>%
      html_text2()
    if (length(return3) != 0) {df$usps_dpv[i] <- return3} else {next}
  }
  
  remDr$close()
  rD$server$stop()
  rm(remDr, rD)
  
  system("taskkill /im java.exe /f", intern = F, ignore.stdout = F)
  
  df

}

#### ~----



#### Google Maps search and scrape functions ####

## Packages used: RSelenium, rvest

# Open a browser and go to Google Maps
gmaps_init <- function(start_row) {
  rD <- rsDriver(browser = "chrome", port = 1111L, chromever = "104.0.5112.79")
  remDr <- rD$client
  remDr$navigate("https://www.google.com/maps/@39.0463488,-95.6760064,6849m/data=!3m1!1e3")
  assign("row", start_row, envir=.GlobalEnv)
  assign("rD", rD, envir=.GlobalEnv)
  assign("remDr", remDr, envir=.GlobalEnv)
}

# Search the address from each record
gmaps_search <- function(df, row) {
  i <- row
  recno <- df$recno[i]
  assign("recno", recno, envir=.GlobalEnv)
  address1 <- paste0(df$st_Name[i], " ", df$addr_line2[i])
  assign("address1", address1, envir=.GlobalEnv)
  address2 <- paste0(df$city[i], ", ", df$state[i], " ", df$zip5[i])
  assign("address2", address2, envir=.GlobalEnv)
  remDr$findElement(using = "id", value = "searchboxinput")$clearElement()
  remDr$findElement(using = "id", value = "searchboxinput")$sendKeysToElement(list(df$st_Name[i], " ",
                                                                                   df$addr_line2[i], " ",
                                                                                   df$city[i], " ",
                                                                                   df$state[i]))
  # if (df$epi_street[i] != "") {
  #   remDr$findElement(using = "id", value = "searchboxinput")$clearElement()
  #   remDr$findElement(using = "id", value = "searchboxinput")$sendKeysToElement(list(df$epi_street[i], " ",
  #                                                                                    df$epi_unit[i], " ",
  #                                                                                    df$epi_city[i], " ",
  #                                                                                    df$epi_state[i]))
  #   address1 <- paste0(df$epi_street[i], " ", df$epi_unit[i])
  #   address2 <- paste0(df$epi_city[i], ", ", df$epi_state[i], " ", df$epi_zip[i])
  # } else {
  #   remDr$findElement(using = "id", value = "searchboxinput")$clearElement()
  #   remDr$findElement(using = "id", value = "searchboxinput")$sendKeysToElement(list(df$st_Name[i], " ",
  #                                                                                    df$addr_line2[i], " ",
  #                                                                                    df$city[i], " ",
  #                                                                                    df$state[i]))
  #   address1 <- paste0(df$st_Name[i], " ", df$addr_line2[i])
  #   address2 <- paste0(df$city[i], ", ", df$state[i], " ", df$zip5[i])
  # }
}

# Pull the address from the result page
gmaps_pull <- function() {
  html <- read_html(remDr$getPageSource()[[1]])
  return <- html %>%
    html_elements("div.aIFcqe div:nth-child(7) span.DkEaL") %>%
    html_text2()
  return
}

# Add the pulled address to a DF
gmaps_add <- function(df, row, input) {
  i <- nrow(df) + 1
  df[i, 1] <- recno
  for (j in 1:6) {
    if (!is.na(input[j])) {
      df[i, j + 1] <- str_trim(input[j])
    }
  }
  df
}

# Advance to the next row
gmaps_advance <- function(row) {
  remDr$navigate("https://www.google.com/maps/@39.0463488,-95.6760064,6849m/data=!3m1!1e3")
  row <- row + 1
  assign("row", row, envir=.GlobalEnv)
}

# Close browser and server
gmaps_close <- function(row) {
  remDr$close()
  rD$server$stop()
  system("taskkill /im java.exe /f", intern = F, ignore.stdout = F)
}

#### ~----



#### Google search function ####

## Packages used: RSelenium, rvest

# Searches addresses supplied by DF and returns the main result
search_google <- function(df_search) {
  
  df_search$google_addr1 <- ""
  df_search$google_addr2 <- ""
  
  pb <- txtProgressBar(1, nrow(df_search), width = 50, style = 3)
  
  rD <- rsDriver(browser = "chrome", port = 1111L, chromever = "104.0.5112.79")
  remDr <- rD$client
  
  for (i in 1:nrow(df_search)) {
    remDr$navigate(df_search$google_url[i])
    Sys.sleep(2)
    setTxtProgressBar(pb, i)
    html <- read_html(remDr$getPageSource()[[1]])
    return1 <- html %>%
      html_elements(".desktop-title-content") %>%
      html_text2()
    if (length(return1) != 0) {df_search$google_addr1[i] <- return1} else {next}
    return2 <- html %>%
      html_elements(".desktop-title-subcontent") %>%
      html_text2()
    if (length(return2) != 0) {df_search$google_addr2[i] <- return2} else {next}
  }
  
  remDr$close()
  rD$server$stop()
  rm(remDr, rD)
  
  system("taskkill /im java.exe /f", intern = F, ignore.stdout = F)
  
  return(df_search)
  # assign("df_google_results", df_search, envir=.GlobalEnv)
  
}

# References
# https://levelup.gitconnected.com/web-scraping-with-r-part-2-dynamic-webpages-de620a161671
# https://resulumit.com/teaching/scrp_workshop.html#73
# https://stackoverflow.com/questions/43991498/rselenium-server-signals-port-is-already-in-use

#### ~----



