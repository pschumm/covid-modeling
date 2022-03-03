suppressMessages(library(data.table))
suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
suppressMessages(library(repr))
options(dplyr.summarise.inform = FALSE)

# Do not execute this. Kernel will be aborted due to larget data size
# cdc.source.csv <- "COVID-19_Case_Surveillance_Public_Use_Data_with_Geography.csv.gz"
# 5/10/2021 updates https://data.cdc.gov/api/views/n8mc-b4w4/rows.csv?accessType=DOWNLOAD
# Dictionary https://data.cdc.gov/Case-Surveillance/COVID-19-Case-Surveillance-Public-Use-Data-with-Ge/n8mc-b4w4
# Variable summary https://data.cdc.gov/Case-Surveillance/COVID-19-Case-Surveillance-Public-Use-Data-with-Ge/ynhu-f2s2

# read CDC 19 column dataset
data <- fread(cdc.source.csv, 
          colClasses = list(factor = c( "case_month", 
                                        "res_state", 
                                        "state_fips_code", 
                                        "age_group", 
                                        "sex", 
                                        "race", 
                                        "ethnicity", 
                                        "process", 
                                        "exposure_yn", 
                                        "current_status", 
                                        "symptom_status", 
                                        "hosp_yn", 
                                        "icu_yn", 
                                        "death_yn", 
                                        "underlying_conditions_yn"), 
                            character = c("res_county", 
                                          "county_fips_code"),
                            integer = c("case_positive_specimen_interval", 
                                        "case_onset_interval")))


# remove unconfirmed cases and recode ethnicity as "Hispanic White", "Non-His White", "Non-His Asian", "Non-His Black"
# this removed 10+% of the records
data <- data %>%  
          filter(current_status == "Laboratory-confirmed case") %>%
          rename(original_ethnicity = ethnicity) %>%
          mutate(ethnicity = case_when(
                                  race == "White" & original_ethnicity == "Non-Hispanic/Latino" ~ "Non-His White",
                                  race == "Asian" & original_ethnicity == "Non-Hispanic/Latino" ~ "Non-His Asian",
                                  race == "Black" & original_ethnicity == "Non-Hispanic/Latino" ~ "Non-His Black",
                                  race == "White" & original_ethnicity == "Hispanic/Latino" ~ "Hispanic White")) %>%
          select(-c(current_status, race, original_ethnicity, exposure_yn)) %>%
          setDT()
saveRDS(data, file="cdc_May102021_22M_trimmed_record.rds")


# Remove records with incomplete geo information, unknown age_group, sex, mortality, unconfirmed cases, and 
# ethnicity not in the 4 categories above. This left us with about 20% of the total records.
data2 <- data %>%
             filter(!is.na(res_state) & 
                    !is.na(state_fips_code) & 
                    !is.na(county_fips_code) &
                    !is.na(res_county) &
                    !is.na(age_group) & 
                    age_group != "Missing" & 
                    sex %in% c("Female", "Male") & 
                    !is.na(ethnicity) & 
                    death_yn %in% c("Yes", "No")) %>%
             filter(!is.na(ethnicity)) %>%  
             mutate(sex = factor(sex),
                    age_group = factor(age_group), 
                    ethnicity = factor(ethnicity, levels = c("Non-His Asian", "Non-His Black", "Non-His White", "Hispanic White"))) %>%
             setDT()             
saveRDS(data2, file="cdc_May102021_5M_trimmed_record.rds")
data3 <- data2[case_onset_interval==0]         
saveRDS(data3, file="cdc_May102021_3M_trimmed_record.rds")

data3 <- readRDS("cdc_May102021_3M_trimmed_record.rds")

ethnicity_sex_prop <- data3 %>% 
        group_by(ethnicity) %>% 
        summarise(male = sum(sex=="Male"), 
                  female = sum(sex=="Female"), 
                  proportion_female = female / (male + female), 
                  percentage_female = paste0(round( proportion_female * 100, digit=2), "%"), 
                  percentage_male = paste0(round( (1 - proportion_female) * 100, digit=2), "%")) %>%
        setDT()
options(repr.plot.width=8, repr.plot.height=8)
ggplot(data3) + 
        geom_bar(aes(x=ethnicity, fill=sex)) + 
        geom_text(data = ethnicity_sex_prop, aes(x=ethnicity, y=male, label=percentage_female, vjust=-0.3), size=6) + 
        geom_text(data = ethnicity_sex_prop, aes(x=ethnicity, y=male, label=percentage_male, vjust=1.2), size=6) + 
        ggtitle("Case Count By Ethnicity & Sex") + 
        theme(legend.position = c(0.1, 0.9)) + 
        theme(text = element_text(size=12))

data3 %>% 
        group_by(ethnicity) %>% 
        summarise(fatality_count = sum(death_yn == "Yes"), 
                  case_count = n(),
                  fatality_rate = paste0(round(fatality_count/case_count * 100, digit=2), "%")) 

data3 %>% 
        group_by(ethnicity) %>% 
        summarise(male = sum(sex=="Male"), 
                  female = sum(sex=="Female"), 
                  proportion_female = female / (male + female), 
                  percentage_female = paste0(round( proportion_female * 100, digit=2), "%"))

data3 %>% 
          group_by(ethnicity, age_group) %>% 
          summarise(fatality_ratio = sum(death_yn == "Yes" & sex == "Female")/ sum(sex == "Female") / sum(death_yn == "Yes" & sex == "Male") * sum(sex == "Male")) %>%
          filter(!is.nan(fatality_ratio))

fatality_rate_age_sex <- data3 %>% 
          filter(!case_month %in% c("2020-01", "2020-02")) %>%
          group_by(ethnicity, age_group, sex) %>% 
          summarise(fatality_rate = sum(death_yn == "Yes")/ n()) 
fatality_rate_age <- data3 %>% 
          filter(!case_month %in% c("2020-01", "2020-02")) %>%
          group_by(ethnicity, age_group) %>% 
          summarise(fatality_rate = sum(death_yn == "Yes")/ n()) %>%
          mutate(sex = "Both Male & Female") 
fatality_rate_sex <- data3 %>% 
          filter(!case_month %in% c("2020-01", "2020-02")) %>%
          group_by(ethnicity, sex) %>% 
          summarise(fatality_rate = sum(death_yn == "Yes")/ n()) %>%
          mutate(age_group = "All Ages") 
fatality_rate <- data3 %>% 
          filter(!case_month %in% c("2020-01", "2020-02")) %>%
          group_by(ethnicity) %>% 
          summarise(fatality_rate = sum(death_yn == "Yes")/ n()) %>%
          mutate(age_group = "All Ages",
                 sex = "Both Male & Female")
d1 <- rbind(fatality_rate_age_sex, fatality_rate_age, fatality_rate_sex, fatality_rate)

options(repr.plot.width=15, repr.plot.height=12)
ggplot(d1, aes(ethnicity, fatality_rate, fill=ethnicity)) + 
       geom_bar(stat="identity") + 
       theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1)) +
       facet_grid(age_group ~ sex, scales = "free_y")

monthly_fatality_rate_age_sex <- data3 %>% 
          filter(!case_month %in% c("2020-01", "2020-02")) %>%
          group_by(case_month, ethnicity, age_group, sex) %>% 
          summarise(fatality_rate = sum(death_yn == "Yes")/ n()) 
monthly_fatality_rate_age <- data3 %>% 
          filter(!case_month %in% c("2020-01", "2020-02")) %>%
          group_by(case_month, ethnicity, age_group) %>% 
          summarise(fatality_rate = sum(death_yn == "Yes")/ n()) %>%
          mutate(sex = "Both Male & Female") 
monthly_fatality_rate_sex <- data3 %>% 
          filter(!case_month %in% c("2020-01", "2020-02")) %>%
          group_by(case_month, ethnicity, sex) %>% 
          summarise(fatality_rate = sum(death_yn == "Yes")/ n()) %>%
          mutate(age_group = "All Ages") 
monthly_fatality_rate <- data3 %>% 
          filter(!case_month %in% c("2020-01", "2020-02")) %>%
          group_by(case_month, ethnicity) %>% 
          summarise(fatality_rate = sum(death_yn == "Yes")/ n()) %>%
          mutate(age_group = "All Ages",
                 sex = "Both Male & Female") 
d2 <- rbind(monthly_fatality_rate_age_sex, monthly_fatality_rate_age, monthly_fatality_rate_sex, monthly_fatality_rate)
ggplot(d2, aes(case_month, fatality_rate, group=ethnicity, col=ethnicity)) + 
       geom_line() + 
       theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1)) +
       facet_grid(age_group ~ sex, scales = "free_y")

phase_fatality_rate <- data3 %>% 
          filter(!case_month %in% c("2020-01", "2020-02")) %>%
          group_by(case_month, ethnicity, age_group, sex) %>% 
          summarise(case_count = n(),
                    fatality_count = sum(death_yn == "Yes"),
                    fatality_rate = fatality_count/ case_count) %>%
          filter(fatality_count >= 10) %>%
          setDT()
print(phase_fatality_rate)

l1 <- lm(log10(fatality_rate) ~ sex + case_month + ethnicity + age_group, data = phase_fatality_rate)
summary(l1)

coef <- data.table(variable = names(l1$coefficients), beta = l1$coefficients) %>%
          filter(grepl("case_month", variable)) %>%
          rename(month = "variable") %>%
          mutate(month = gsub("case_month", "", month)) %>% 
          add_row(month = "2020-03", beta = 0) %>%
          mutate(fatality_rate_change = 10^beta) %>%
          setDT() 

options(repr.plot.width=10, repr.plot.height=8)
ggplot(coef, aes(month, fatality_rate_change, group=1)) + 
    geom_line() +
    theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1)) + 
    scale_y_continuous(labels = scales::percent)
