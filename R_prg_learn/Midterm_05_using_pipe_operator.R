# 期中作業：pipe operator之改寫
load(url("https://storage.googleapis.com/r_rookies/straw_hat_df.RData"))
this_year <- as.numeric(format(Sys.Date(), '%Y'))
birth_year <- this_year - straw_hat_df$age
birth_date_chr <- paste(birth_year, straw_hat_df$birthday, sep = "-")
straw_hat_df$birth_date <- as.Date(birth_date_chr)

load(url("https://storage.googleapis.com/r_rookies/straw_hat_df.RData"))
straw_hat_df$birth_date <- Sys.Date() %>%
  format(.,"%Y") %>%
  # 一旦()中有其他arguments，就得使用"."
  as.numeric() %>%
  '-'(straw_hat_df$age) %>%
  paste(., straw_hat_df$birthday, sep = "-") %>%
  as.Date()
