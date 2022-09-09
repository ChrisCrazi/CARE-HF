library(tidyverse)
library(tidylog)
library(tableone)
library(MatchIt)
library(survival)
library(lubridate)
library(ggpubr)
library(SCCS)

load("C:/Users/LabPC14CSMPR/Desktop/Chris/HOPE/booster dose and COVID-19 mortality in multimorbidity/RDS/DX.RData")
load("C:/Users/LabPC14CSMPR/Desktop/Chris/HOPE/booster dose and COVID-19 mortality in multimorbidity/RDS/A10X.RData")

cohort <- read_rds("C:/Users/LabPC14CSMPR/Desktop/Chris/HOPE/booster dose and COVID-19 mortality in multimorbidity/RDS/4.cohort_full.RDS")
cohort <- cohort %>% filter(!is.na(patient_pssn)) %>% as_tibble()
cohort <- cohort %>% rename(vaccine.brand.1st = `Vaccine Brand.1st`, vaccine.brand.2nd = `Vaccine Brand.2nd`, 
                      vaccine.brand.3rd = `Vaccine Brand.3rd`, vaccine.brand.4th = `Vaccine Brand.4th`, 
                      date.of.vaccination.1st = `Date of vaccination.1st`, date.of.vaccination.2nd = `Date of vaccination.2nd`, 
                      date.of.vaccination.3rd = `Date of vaccination.3rd`, date.of.vaccination.4th = `Date of vaccination.4th`)

dx_clean
dx_latest

# HF
dx_hf <- rbind(dx_clean, dx_latest) %>% as_tibble() %>% filter(str_detect(code, "^428")) %>% mutate(date = as_date(date))

dx_hf_3weeksbefore <- dx_hf %>% filter(date > date("2021-02-01") & date < date("2021-02-23"))

dx_pre <- dx_hf %>%
  filter(date < date("2021-02-23")|is.na(date)) %>%
  group_by(patient_pssn) %>%
  slice(1)

heart_tran_pts <- filter(dx_latest, str_detect(code, "^37.5"))$patient_pssn

# primary outcome hospitalization for heart failure
dx_after <- dx_hf %>%
  filter(!is.na(date)) %>%
  # filter(str_detect(Source, "1.IP")) %>%
  select(patient_pssn, date) %>%
  filter(date >= date("2021-02-23")) %>%
  group_by(patient_pssn) %>% # not recurrent event to be consider
  arrange(date) %>%
  slice(1) %>%
  ungroup() %>% 
  mutate(outcome = "HF hospitalization")


# # secondary outcome mace
# dx_after <- rbind(dx_clean, dx_latest) %>% as_tibble() %>%
#   # MI or IS or carditis
#   filter(str_detect(code, "^433.[012389]1|^43[46]|^437.[01]|^410|^420.9|^422|^423.9|^429.0")) %>%
#   mutate(outcome = if_else(str_detect(code, "^433.[012389]1|^43[46]|^437.[01]"), "stroke", "")) %>%
#   mutate(outcome = if_else(str_detect(code, "^410"), "MI", outcome)) %>%
#   mutate(outcome = if_else(str_detect(code, "^420.9|^422|^423.9|^429.0"), "carditis", outcome)) %>%
#   select(patient_pssn, date, outcome) %>%
#   rbind(select(mutate(rename(filter(cohort, str_detect(death_diag_cd, "^I")), date = death_date_ymd), outcome = "CV death"), patient_pssn, date, outcome)) %>% # add cardiovascular death into the definition of MACE
#   mutate(date = as_date(date)) %>%
#   filter(!is.na(date)) %>%
#   filter(date >= date("2021-02-23")) %>%
#   group_by(patient_pssn) %>% # not recurrent event to be consider
#   arrange(date) %>%
#   slice(1) %>%
#   ungroup()

# # secondary outcome all hospitalization
# dx_after <- admin_latest %>%
#   select(patient_pssn, date, Source) %>%
#   rbind(select(dx_latest, patient_pssn, date, Source)) %>%
#   filter(str_detect(Source, "1.IP")) %>%
#   as_tibble() %>%
#   mutate(date = as_date(date)) %>%
#   filter(!is.na(date)) %>%
#   select(patient_pssn, date) %>%
#   filter(date >= date("2021-02-23")) %>%
#   group_by(patient_pssn) %>% # not recurrent event to be consider
#   arrange(date) %>%
#   slice(1) %>%
#   ungroup() %>%
#   mutate(outcome = "all hospitalization")

# # sen5 eTable7
# dx_after <- dx_latest %>%
#   as_tibble() %>% filter(str_detect(code, "^8[0-2][0-9]")) %>% mutate(date = as_date(date)) %>%
#   filter(!is.na(date)) %>%
#   select(patient_pssn, date) %>%
#   filter(date >= date("2021-02-23")) %>%
#   group_by(patient_pssn) %>% # not recurrent event to be consider
#   arrange(date) %>%
#   slice(1) %>%
#   ungroup()

# flow chart
# cohort %>% filter(patient_pssn %in% dx_pre$patient_pssn) %>% .$vaccine.brand.1st %>% table
# cohort %>% filter(patient_pssn %in% dx_pre$patient_pssn) %>% filter(vaccine.brand.1st == "BioNTech/Fosun") %>% filter(is.na(vaccine.brand.2nd))
# cohort %>% filter(patient_pssn %in% dx_pre$patient_pssn) %>% filter(vaccine.brand.1st == "BioNTech/Fosun") %>% filter(!is.na(vaccine.brand.2nd)) %>% filter(is.na(vaccine.brand.3rd))
# cohort %>% filter(patient_pssn %in% dx_pre$patient_pssn) %>% filter(vaccine.brand.1st == "BioNTech/Fosun") %>% filter(!is.na(vaccine.brand.2nd))  %>% .$vaccine.brand.3rd %>% table
# 
# cohort %>% filter(patient_pssn %in% dx_pre$patient_pssn) %>% filter(vaccine.brand.1st == "Sinovac") %>% filter(is.na(vaccine.brand.2nd))
# cohort %>% filter(patient_pssn %in% dx_pre$patient_pssn) %>% filter(vaccine.brand.1st == "Sinovac") %>% filter(!is.na(vaccine.brand.2nd)) %>% filter(is.na(vaccine.brand.3rd))
# cohort %>% filter(patient_pssn %in% dx_pre$patient_pssn) %>% filter(vaccine.brand.1st == "Sinovac") %>% filter(!is.na(vaccine.brand.2nd))  %>% .$vaccine.brand.3rd %>% table
# 
# cohort %>% filter(patient_pssn %in% dx_pre$patient_pssn) %>% filter(patient_pssn %in% dx_after$patient_pssn) %>% .$vaccine.brand.1st %>% table
# cohort %>% filter(patient_pssn %in% dx_pre$patient_pssn) %>% filter(patient_pssn %in% dx_after$patient_pssn) %>% filter(vaccine.brand.1st == "BioNTech/Fosun") %>% filter(is.na(vaccine.brand.2nd))
# cohort %>% filter(patient_pssn %in% dx_pre$patient_pssn) %>% filter(patient_pssn %in% dx_after$patient_pssn) %>% filter(vaccine.brand.1st == "BioNTech/Fosun") %>% filter(!is.na(vaccine.brand.2nd)) %>% filter(is.na(vaccine.brand.3rd))
# cohort %>% filter(patient_pssn %in% dx_pre$patient_pssn) %>% filter(patient_pssn %in% dx_after$patient_pssn) %>% filter(vaccine.brand.1st == "BioNTech/Fosun") %>% filter(!is.na(vaccine.brand.2nd))  %>% .$vaccine.brand.3rd %>% table
# 
# cohort %>% filter(patient_pssn %in% dx_pre$patient_pssn) %>% filter(patient_pssn %in% dx_after$patient_pssn) %>% filter(vaccine.brand.1st == "Sinovac") %>% filter(is.na(vaccine.brand.2nd))
# cohort %>% filter(patient_pssn %in% dx_pre$patient_pssn) %>% filter(patient_pssn %in% dx_after$patient_pssn) %>% filter(vaccine.brand.1st == "Sinovac") %>% filter(!is.na(vaccine.brand.2nd)) %>% filter(is.na(vaccine.brand.3rd))
# cohort %>% filter(patient_pssn %in% dx_pre$patient_pssn) %>% filter(patient_pssn %in% dx_after$patient_pssn) %>% filter(vaccine.brand.1st == "Sinovac") %>% filter(!is.na(vaccine.brand.2nd))  %>% .$vaccine.brand.3rd %>% table

LAB_ALL_COVID <- readRDS("C:/Users/LabPC14CSMPR/Desktop/Chris/HOPE/booster dose and COVID-19 mortality in multimorbidity/RDS/LAB_ALL_COVID.RDS")
hx_covid <- LAB_ALL_COVID[grepl("^21[3567]$",T_NUM) & result=="detected", unique(patient_pssn)]

df2 <- cohort %>% 
  # filter(!patient_pssn %in% dx_hf_3weeksbefore$patient_pssn) %>% # sen3 eTable6
  mutate(death_date_ymd = as_date(death_date_ymd)) %>% 
  filter(patient_pssn %in% dx_pre$patient_pssn) %>%
  filter(patient_pssn %in% dx_after$patient_pssn) %>%
  mutate(outcome = dx_after$outcome[match(patient_pssn, dx_after$patient_pssn)]) %>% 
  mutate(eventdate = dx_after$date[match(patient_pssn, dx_after$patient_pssn)]) %>% 
  # filter(is.na(death_date_ymd)) %>% # sen1 eTable3
  # filter(!is.na(date.of.vaccination.1st)) %>%
  group_by(patient_pssn) %>% 
  slice(1) %>% 
  ungroup() %>% 
  filter((vaccine.brand.1st == "BioNTech/Fosun" & (vaccine.brand.3rd == "BioNTech/Fosun" | is.na(vaccine.brand.3rd))) | (vaccine.brand.1st == "Sinovac" & (vaccine.brand.3rd == "Sinovac" | is.na(vaccine.brand.3rd))) | is.na(vaccine.brand.1st)) %>% 
  mutate(type = if_else(vaccine.brand.1st == "BioNTech/Fosun", "BNT162b2", "")) %>% 
  mutate(type = if_else(vaccine.brand.1st == "Sinovac", "CoronaVac", type)) %>% 
  mutate(type = if_else(is.na(vaccine.brand.1st), "unvaccinated", type)) %>% 
  mutate(hx.covid = if_else(patient_pssn %in% hx_covid, 1, 0))

# df2 %>% filter(outcome == "carditis") %>% filter(!is.na(vaccine.brand.1st)) %>% select(starts_with("date"), eventdate, everything()) %>% View()

# final_cohort <- df2 %>% filter((vaccine.brand.1st == "BioNTech/Fosun" & (vaccine.brand.3rd == "BioNTech/Fosun" | is.na(vaccine.brand.3rd))) | (vaccine.brand.1st == "Sinovac" & (vaccine.brand.3rd == "Sinovac" | is.na(vaccine.brand.3rd))) | is.na(vaccine.brand.1st))
# write_rds(final_cohort, "HF result/final_cohort.rds")

# df2 %>% filter(patient_pssn %in% heart_tran_pts) %>% View()

df2 %>% filter(death_date_ymd != "") %>% filter(type == "BNT162b2") %>% .$death_diag_cd %>% table()
df2 %>% filter(death_date_ymd != "") %>% filter(type == "CoronaVac") %>% .$death_diag_cd %>% table()

df2 %>% filter(type == "BNT162b2") %>% .$hx.covid %>% table()
df2 %>% filter(type == "CoronaVac") %>% .$hx.covid %>% table()

df3 <- df2 %>% 
  # filter(Age >= 80) %>%
  # filter(Age < 80) %>%
  # filter(sex == "M") %>%
  # filter(sex == "F") %>%
  # filter(hx.covid == 0) %>% # sen2 eTable5
  mutate(nid = seq_along(patient_pssn)) %>%
  # filter(vaccine.brand.1st == "BioNTech/Fosun" & (vaccine.brand.3rd == "BioNTech/Fosun" | is.na(vaccine.brand.3rd)) | is.na(vaccine.brand.1st)) %>% # main analysis: include BNT and unvaccinated
  filter(vaccine.brand.1st == "Sinovac" & (vaccine.brand.3rd == "Sinovac" | is.na(vaccine.brand.3rd)) | is.na(vaccine.brand.1st)) %>% # main analysis: include sinovac and unvaccinated
  # mutate(dob = as_date(paste0(dob_y, "-01-01"))) %>% 
  mutate(dob = as_date("2021-01-01")) %>% 
  mutate(eventdate = as.integer(as_date(eventdate) - dob)) %>% 
  mutate(obs_start = as.integer(as_date("2021-02-23")-dob)) %>% 
  mutate(vaccdate1 = as.integer(as_date(date.of.vaccination.1st)-dob)) %>% 
  mutate(vaccdate2 = as.integer(as_date(date.of.vaccination.2nd)-dob)) %>% 
  mutate(vaccdate3 = as.integer(as_date(date.of.vaccination.3rd)-dob)) %>% 
  mutate(vaccdate1_p14 = as.integer(vaccdate1 + 14)) %>% 
  mutate(vaccdate2_p14 = as.integer(vaccdate2 + 14)) %>% 
  mutate(vaccdate3_p14 = as.integer(vaccdate3 + 14)) %>% 
  mutate(obs_end = as.integer(pmin(as_date("2022-03-31"), as_date(death_date_ymd), na.rm = T)-dob)) %>% 
  select(nid, starts_with("obs"), starts_with("vaccdate"), starts_with("eventdate"), vaccine.brand.1st)

df3$vaccine.brand.1st %>% table()
df3 %>% filter(is.na(vaccine.brand.1st)) %>% table()

# df3 <- df3 %>% mutate(eventdate = if_else(nid == 3174, 357, as.double(eventdate))) %>% mutate(eventdate = if_else(nid == 3174, 357, as.double(eventdate)))
# df3 %>% filter(nid %in% c(1295,1916,2916,6888))
# df3 %>% filter(!is.na(vaccdate3)) %>% filter(eventdate > 320)
# df3 %>% filter(nid %in% c(1334,7007,8430))
# df3 %>% filter(!is.na(vaccdate3)) %>% filter(eventdate > 320)

# ageq <- floor(quantile(df3$eventdate, seq(0.1,0.9,0.1),
#                        names=F, na.rm = T))

# ageq <- cumsum(c(90, 61, 61, 61, 61, 61))
ageq <- cumsum(c(59, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, 31, 28))

gc()
(eventde_result <- eventdepenexp(indiv = nid,
                                 astart = obs_start,
                                 aend = obs_end,
                                 aevent = eventdate,
                                 adrug = cbind(vaccdate1, vaccdate1_p14, vaccdate2, vaccdate2_p14, vaccdate3, vaccdate3_p14),
                                 aedrug = cbind(vaccdate1+13, vaccdate1_p14 +13, vaccdate2+13, vaccdate2_p14+13, vaccdate3+13, vaccdate3_p14+13),
                                 sameexpopar = F,
                                 agegrp=ageq,
                                 dataformat = "multi",
                                 data = df3))

# # sen2 eTable4
# (eventde_result <- standardsccs(event ~ vaccdate1+age,
#                              indiv = nid,
#                              astart = obs_start,
#                              aend = obs_end,
#                              aevent = eventdate,
#                              adrug = cbind(vaccdate1, vaccdate1_p14, vaccdate2, vaccdate2_p14, vaccdate3, vaccdate3_p14),
#                              aedrug = cbind(vaccdate1+13, vaccdate1_p14 +13, vaccdate2+13, vaccdate2_p14+13, vaccdate3+13, vaccdate3_p14+13),
#                              sameexpopar = F,
#                              agegrp=ageq,
#                              dataformat = "multi",
#                              data = df3))

str_c(round(eventde_result$conf.int[seq(1,6),], 2)[,1], "(",
      round(eventde_result$conf.int[seq(1,6),], 2)[,3], "-",
      round(eventde_result$conf.int[seq(1,6),], 2)[,4], ")")


mace_data <- formatdata(indiv = nid,
                        astart = obs_start,
                        aend = obs_end,
                        aevent = eventdate,
                        adrug = cbind(vaccdate1, vaccdate1_p14, vaccdate2, vaccdate2_p14, vaccdate3, vaccdate3_p14),
                        aedrug = cbind(vaccdate1+13, vaccdate1_p14 +13, vaccdate2+13, vaccdate2_p14+13, vaccdate3+13, vaccdate3_p14+13),
                        sameexpopar = F,
                        agegrp=ageq,
                        dataformat = "multi",
                        data = df3)
tapply(mace_data$event, mace_data$vaccdate1, sum)
tapply(mace_data$interval, mace_data$vaccdate1, sum)/365

result <- rbind(cbind(eventde_result$conf.int, eventde_result$coefficients)[, c(1,3,4,9)][seq(1,6),]) %>% 
  as_tibble() %>% 
  mutate_if(is.numeric, round, digits = 2) %>% 
  mutate_all(format, nsmall = 2)
colnames(result) <- c("exp", "lower", "upper", "p")
result <- result %>%
  mutate(IRR = str_c(exp, " (", lower, "-", upper, ")", sep = "")) %>% 
  mutate(p = if_else(p == "0.00", "<.01", p)) %>% 
  select(IRR, p)
result <- rbind(rep("", 2), result[seq(1,6),])
result <- result %>% mutate(No.events = c(tapply(mace_data$event, mace_data$vaccdate1, sum))) %>% 
  mutate(follow.up = c(tapply(mace_data$interval, mace_data$vaccdate1, sum))) %>% 
  mutate(No.events = as.integer(No.events)) %>% 
  mutate(follow.up = as.integer(follow.up)) %>% 
  mutate(absolute.rate = format(round(No.events*1000/follow.up, digits = 1), nsmall = 1)) %>% 
  select(No.events, follow.up, absolute.rate, IRR, p)
result <- rbind(rep("", 5), result[1,], rep("", 5), result[seq(2,3),], rep("", 5), result[seq(4,5),], rep("", 5), result[seq(6,7),])
# write_csv(result, "HF result/HF biontech.csv")
# write_csv(result, "HF result/HF sinovac.csv")
# write_csv(result, "HF result/all hospitalization biontech.csv")
# write_csv(result, "HF result/all hospitalization sinovac.csv")

# # sen4 eTable 7
# (eventde_result <- eventdepenexp(indiv = nid,
#                                  astart = obs_start,
#                                  aend = obs_end,
#                                  aevent = eventdate,
#                                  adrug = cbind(vaccdate1, vaccdate1+1, vaccdate1_p14,
#                                                vaccdate2, vaccdate2+1, vaccdate2_p14,
#                                                vaccdate3, vaccdate3+1, vaccdate3_p14),
#                                  aedrug = cbind(vaccdate1, vaccdate1+13, vaccdate1_p14+13,
#                                                 vaccdate2, vaccdate2+13, vaccdate2_p14+13,
#                                                 vaccdate3, vaccdate3+13, vaccdate3_p14+13),
#                                  sameexpopar = F,
#                                  agegrp=ageq,
#                                  dataformat = "multi",
#                                  data = df3))
#
# str_c(round(eventde_result$conf.int[seq(1,9),], 2)[,1], "(",
#       round(eventde_result$conf.int[seq(1,9),], 2)[,3], "-",
#       round(eventde_result$conf.int[seq(1,9),], 2)[,4], ")")
# 
# 
# mace_data <- formatdata(indiv = nid,
#                         astart = obs_start,
#                         aend = obs_end,
#                         aevent = eventdate,
#                         adrug = cbind(vaccdate1, vaccdate1+1, vaccdate1_p14, 
#                                       vaccdate2, vaccdate2+1, vaccdate2_p14, 
#                                       vaccdate3, vaccdate3+1, vaccdate3_p14),
#                         aedrug = cbind(vaccdate1, vaccdate1+13, vaccdate1_p14+13, 
#                                        vaccdate2, vaccdate2+13, vaccdate2_p14+13, 
#                                        vaccdate3, vaccdate3+13, vaccdate3_p14+13),
#                         sameexpopar = F,
#                         agegrp=ageq,
#                         dataformat = "multi",
#                         data = df3)
# tapply(mace_data$event, mace_data$vaccdate1, sum)
# tapply(mace_data$interval, mace_data$vaccdate1, sum)/365
# 
# result <- rbind(cbind(eventde_result$conf.int, eventde_result$coefficients)[, c(1,3,4,9)][seq(1,9),]) %>% 
#   as_tibble() %>% 
#   mutate_if(is.numeric, round, digits = 2) %>% 
#   mutate_all(format, nsmall = 2)
# colnames(result) <- c("exp", "lower", "upper", "p")
# result <- result %>%
#   mutate(IRR = str_c(exp, " (", lower, "-", upper, ")", sep = "")) %>% 
#   mutate(p = if_else(p == "0.00", "<.01", p)) %>% 
#   select(IRR, p)
# result <- rbind(rep("", 2), result[seq(1,9),])
# result <- result %>% mutate(No.events = c(tapply(mace_data$event, mace_data$vaccdate1, sum))) %>% 
#   mutate(follow.up = c(tapply(mace_data$interval, mace_data$vaccdate1, sum))) %>% 
#   mutate(No.events = as.integer(No.events)) %>% 
#   mutate(follow.up = as.integer(follow.up)) %>% 
#   mutate(absolute.rate = format(round(No.events*1000/follow.up, digits = 1), nsmall = 1)) %>% 
#   select(No.events, follow.up, absolute.rate, IRR, p)
# result <- rbind(rep("", 5), result[1,], rep("", 5), result[seq(2,4),], rep("", 5), result[seq(5,7),], rep("", 5), result[seq(8,10),])
# # write_csv(result, "HF result/sen 4 all hospitalization biontech.csv")
# # write_csv(result, "HF result/sen 4 all hospitalization sinovac.csv")

# # sen5 eTable 8
# (eventde_result <- eventdepenexp(indiv = nid,
#                                  astart = obs_start,
#                                  aend = obs_end,
#                                  aevent = eventdate,
#                                  adrug = cbind(vaccdate1, vaccdate2, vaccdate3),
#                                  aedrug = cbind(vaccdate1+27, vaccdate2+27, vaccdate3+27),
#                                  sameexpopar = F,
#                                  agegrp=ageq,
#                                  dataformat = "multi",
#                                  data = df3))
# 
# str_c(round(eventde_result$conf.int[seq(1,3),], 2)[,1], "(",
#       round(eventde_result$conf.int[seq(1,3),], 2)[,3], "-",
#       round(eventde_result$conf.int[seq(1,3),], 2)[,4], ")")
# 
# 
# mace_data <- formatdata(indiv = nid,
#                         astart = obs_start,
#                         aend = obs_end,
#                         aevent = eventdate,
#                         adrug = cbind(vaccdate1, vaccdate2, vaccdate3),
#                         aedrug = cbind(vaccdate1+27, vaccdate2+27, vaccdate3+27),
#                         sameexpopar = F,
#                         agegrp=ageq,
#                         dataformat = "multi",
#                         data = df3)
# tapply(mace_data$event, mace_data$vaccdate1, sum)
# tapply(mace_data$interval, mace_data$vaccdate1, sum)/365
# 
# result <- rbind(cbind(eventde_result$conf.int, eventde_result$coefficients)[, c(1,3,4,9)][seq(1,9),]) %>% 
#   as_tibble() %>% 
#   mutate_if(is.numeric, round, digits = 2) %>% 
#   mutate_all(format, nsmall = 2)
# colnames(result) <- c("exp", "lower", "upper", "p")
# result <- result %>%
#   mutate(IRR = str_c(exp, " (", lower, "-", upper, ")", sep = "")) %>% 
#   mutate(p = if_else(p == "0.00", "<.01", p)) %>% 
#   select(IRR, p)
# result <- rbind(rep("", 2), result[seq(1,3),])
# result <- result %>% mutate(No.events = c(tapply(mace_data$event, mace_data$vaccdate1, sum))) %>% 
#   mutate(follow.up = c(tapply(mace_data$interval, mace_data$vaccdate1, sum))) %>% 
#   mutate(No.events = as.integer(No.events)) %>% 
#   mutate(follow.up = as.integer(follow.up)) %>% 
#   mutate(absolute.rate = format(round(No.events*1000/follow.up, digits = 1), nsmall = 1)) %>% 
#   select(No.events, follow.up, absolute.rate, IRR, p)
# result <- rbind(rep("", 5), result[1,], rep("", 5), result[2,], rep("", 5), result[3,], rep("", 5), result[4,])
# # write_csv(result, "HF result/sen 5 all hospitalization biontech.csv")
# # write_csv(result, "HF result/sen 5 all hospitalization sinovac.csv")
# 
# # sen6 eTable 9
# (eventde_result <- eventdepenexp(indiv = nid,
#                                  astart = obs_start,
#                                  aend = obs_end,
#                                  aevent = eventdate,
#                                  adrug = cbind(vaccdate1, vaccdate2, vaccdate3),
#                                  aedrug = cbind(vaccdate1+20, vaccdate2+20, vaccdate3+20),
#                                  sameexpopar = F,
#                                  agegrp=ageq,
#                                  dataformat = "multi",
#                                  data = df3))
# 
# str_c(round(eventde_result$conf.int[seq(1,3),], 2)[,1], "(",
#       round(eventde_result$conf.int[seq(1,3),], 2)[,3], "-",
#       round(eventde_result$conf.int[seq(1,3),], 2)[,4], ")")
# 
# 
# mace_data <- formatdata(indiv = nid,
#                         astart = obs_start,
#                         aend = obs_end,
#                         aevent = eventdate,
#                         adrug = cbind(vaccdate1, vaccdate2, vaccdate3),
#                         aedrug = cbind(vaccdate1+20, vaccdate2+20, vaccdate3+20),
#                         sameexpopar = F,
#                         agegrp=ageq,
#                         dataformat = "multi",
#                         data = df3)
# tapply(mace_data$event, mace_data$vaccdate1, sum)
# tapply(mace_data$interval, mace_data$vaccdate1, sum)/365
# 
# result <- rbind(cbind(eventde_result$conf.int, eventde_result$coefficients)[, c(1,3,4,9)][seq(1,9),]) %>% 
#   as_tibble() %>% 
#   mutate_if(is.numeric, round, digits = 2) %>% 
#   mutate_all(format, nsmall = 2)
# colnames(result) <- c("exp", "lower", "upper", "p")
# result <- result %>%
#   mutate(IRR = str_c(exp, " (", lower, "-", upper, ")", sep = "")) %>% 
#   mutate(p = if_else(p == "0.00", "<.01", p)) %>% 
#   select(IRR, p)
# result <- rbind(rep("", 2), result[seq(1,3),])
# result <- result %>% mutate(No.events = c(tapply(mace_data$event, mace_data$vaccdate1, sum))) %>% 
#   mutate(follow.up = c(tapply(mace_data$interval, mace_data$vaccdate1, sum))) %>% 
#   mutate(No.events = as.integer(No.events)) %>% 
#   mutate(follow.up = as.integer(follow.up)) %>% 
#   mutate(absolute.rate = format(round(No.events*1000/follow.up, digits = 1), nsmall = 1)) %>% 
#   select(No.events, follow.up, absolute.rate, IRR, p)
# result <- rbind(rep("", 5), result[1,], rep("", 5), result[2,], rep("", 5), result[3,], rep("", 5), result[4,])
# # write_csv(result, "HF result/sen 6 all hospitalization biontech.csv")
# # write_csv(result, "HF result/sen 6 all hospitalization sinovac.csv")