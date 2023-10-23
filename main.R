if (!require("pacman")) install.packages("pacman")

pkgs =
  c("here",
    "tidyverse",
    "readxl"
    )
pacman::p_load(pkgs, character.only = T)

sero_data <- read_xls(path = here("data", "PEER_HealthData_Public.xls"))

sero <- sero_data %>%
  mutate(village = as_factor(village),
         chiefdom = as_factor(chiefdom),
         district = as_factor(district),
         sex = as_factor(sex),
         iggfinal = as_factor(iggfinal),
         age_cat = cut(age_en, breaks = c(0, 9, 19, 29, 39, 49, 59, 69, 79, 89, 99, 109),
                       labels = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90-99", "100-109"))) %>%
  select(ID, village, chiefdom, district, sex, age_en, iggfinal, age_cat) %>%
  drop_na(age_en)

ggplot(sero, aes(x = village, fill = iggfinal)) +
  geom_bar() +
  facet_wrap(~ district, scales = "free")

ggplot(sero, aes(x = age_cat, fill = iggfinal)) +
  geom_bar(position = "fill") +
  facet_wrap(~ district, scales = "free")

ggplot(sero %>%
         filter(district == "Kenema"), aes(x = age_cat, fill = iggfinal)) +
  geom_bar(position = "fill") +
  facet_wrap(~ chiefdom, scales = "free")

ggplot(sero %>%
         filter(district == "Kenema") %>%
         group_by(village) %>%
         mutate(n = n(),
                village = paste0(village, ": N =", n)),
       aes(x = age_cat, fill = iggfinal)) +
  geom_bar(position = "fill") +
  facet_wrap(~ village, scales = "free")
