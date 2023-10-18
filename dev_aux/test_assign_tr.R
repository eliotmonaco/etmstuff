
data_core_2015_2019 <- readRDS("../BL_2015-2022/data/final/data_core_2015-2019.rds")
data_core_2020_2022 <- readRDS("../BL_2015-2022/data/final/data_core_2020-2022.rds")

data2015 <- etmstuff::subset_date_range(data_core_2015_2019, "lab_collection_date", "2015")
data2022 <- etmstuff::subset_date_range(data_core_2020_2022, "lab_collection_date", "2022")



library(dplyr)
library(etmstuff)



profvis::profvis(
  data2022_tr6 <- classify_test_reason(
    data2022,
    # data2022 %>%
    #   slice(1:1000),
    blrv = 3.5
  )
)



inspect_test_reason <- function(df, type) {
  if (type == "unknown") {
    ids <- df %>%
      filter(test_reason == "unknown") %>%
      pull(patient_id)
  } else if (type == "alert") {
    ids <- df %>%
      filter(!is.na(test_seq_alert) | !is.na(test_followup_alert)) %>%
      pull(patient_id)
  }

  df %>%
    filter(patient_id %in% ids) %>%
    select(
      patient_id, lab_collection_date, lab_result_number,
      lab_specimen_source, test_reason, test_seq_alert, test_followup_alert
    ) %>%
    arrange(patient_id, lab_collection_date, lab_result_number)
}



# To invoke browser() after 5 iterations of loop: browser(expr = {i == 5})

devtools::load_all()

# debugonce(classify_test_reason)

df <- classify_test_reason(data2022, blrv = 3.5)



# 2015

data2015_tr2 <- classify_test_reason(data2015, blrv = 3.5)

data2015_tr %>%
  group_by(test_reason) %>%
  count()

df15_unk1 <- inspect_unknowns(data2015_tr)

data2015_tr2 %>%
  group_by(test_reason) %>%
  count() %>%
  arrange(desc(n))

df15_unk2 <- inspect_unknowns(data2015_tr2)



# 2022

data2022_tr5 <- classify_test_reason(data2022, blrv = 3.5)

data2022_tr %>%
  group_by(test_reason) %>%
  count()

df22_unk1 <- inspect_unknowns(data2022_tr)

data2022_tr2 %>%
  group_by(test_reason) %>%
  count()

df22_unk2 <- inspect_unknowns(data2022_tr2)

data2022_tr4 %>%
  group_by(test_reason) %>%
  count()

df22_unk4 <- inspect_test_reason(data2022_tr4, type = "unknown")

data2022_tr5 %>%
  group_by(test_reason) %>%
  count()

data2022_tr5 %>%
  filter(lab_collection_date < "2022-10-01") %>%
  group_by(test_followup_alert) %>%
  count()

df22_unk5 <- inspect_test_reason(data2022_tr5, type = "alert")










all.equal(data2022_tr5, data2022_tr6)






devtools::load_all()

df <- classify_test_reason(data2022 %>% slice(1:1000), blrv = 3.5)

dfA <- inspect_test_reason(df, type = "alert")



all.equal(df, data2022_tr5 %>% slice(1:1000))



df <- data2022_tr2 %>%
  select(
    dupe_id2 = dupe_id,
    patient_id2 = patient_id,
    test_reason2 = test_reason
  ) %>%
  bind_cols(
    data2022_tr3 %>%
      select(
        patient_id3 = patient_id,
        test_reason3 = test_reason
      )
  ) %>%
  filter(test_reason2 == "unknown", test_reason3 == "ven_cfm_nonelev")

df2 <- data2022_tr2 %>%
  filter(patient_id == df$patient_id2) %>%
  select(dupe_id, patient_id, lab_collection_date, lab_result_number, lab_specimen_source, test_reason) %>%
  arrange(patient_id, lab_collection_date, lab_result_number)






data2022_tr2 %>%
  group_by(test_reason) %>%
  count()

df %>%
  group_by(test_reason) %>%
  count()







