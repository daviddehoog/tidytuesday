# Tidy Tuesday - Patient Risk Profiles (2023, Week 44)
David de Hoog
2023-10-22

### Data and setup

``` r
library(tidytuesdayR)
library(tidyverse)
library(janitor)
```

``` r
tuesdata <- tidytuesdayR::tt_load('2023-10-24')
```


        Downloading file 1 of 1: `patient_risk_profiles.csv`

``` r
patient_risk_profiles <- tuesdata$patient_risk_profiles
```

### Cleaning

``` r
# Create a single, age group column in ten-year groupings, as a factor.
patient_risk_profiles %>%
  # Replace the 0s with NAs
  mutate(across(starts_with("age group: "), ~ na_if(.x, 0))) %>% 
  # Collapse age group columns into one age_group column, with the age ranges as values and dropping any rows with NA values.
  pivot_longer(
    cols = starts_with("age group: "),
    names_prefix = "age group: ",
    names_to = "age_group",
    values_drop_na = TRUE,
  ) %>% 
  # Clean up the age group values by removing spaces and convert to factors.
  mutate(age_group = str_replace_all(age_group, "\\s", ""), age_group = as.factor(age_group)) %>%
  # Collapse the five-year groups into ten year groups.
  mutate(age_group = 
           fct_collapse(age_group,
                `0 - 9` = c("0-4", "5-9"),
                `10 - 19` = c("10-14", "15-19"),
                `20 - 29` = c("20-24", "25-29"),
                `30 - 39` = c("30-34", "35-39"),
                `40 - 49` = c("40-44", "45-49"),
                `50 - 59` = c("50-54", "55-59"),
                `60 - 69` = c("60-64", "65-69"),
                `70 - 79` = c("70-74", "75-79"),
                `80 - 89` = c("80-84", "85-89"),
                `over 90` = c("90-94"),
                )
         ) %>% 
  # Filter entries where both sexes are selected (for plotting purposes only).
  filter(!(`Sex = FEMALE` == 1 & `Sex = MALE` == 1)) %>%
  # Effectively combine the two sex columns into one.
  mutate(sex = case_when(
    `Sex = FEMALE` == 1 ~ "Female",
    `Sex = MALE` == 1 ~ "Male"
    ) 
  ) %>%
  # Clean up and rationalise the columns.
  select(personId, sex, age_group, everything(), -`Sex = FEMALE`, -`Sex = MALE`) %>% 
  clean_names() -> patient_risk_clean
```

## Transforming

### Risk factor occurence

``` r
# Create a tibble of each risk, its occurence and percentage.  
patient_risk_clean %>%
  # Add up each of the columns of previous year diagnoses.
  summarize(across(ends_with("in_prior_year"), ~ sum(.x))) %>%
  # Convert the wide table into a longer one, with one row per risk factor.
  pivot_longer(
    cols = everything(),
    names_to = "in_prior_year",
    names_pattern = "(.*)_in_prior_year",
    values_to = "count"
    ) %>% 
  # Create a percentage column.
  mutate(percentage = count / nrow(patient_risk_clean) * 100) %>% 
  # Sort in descending order by the number of times a risk factor occurs.
  arrange(desc(count))
```

    # A tibble: 64 × 3
       in_prior_year                   count percentage
       <chr>                           <dbl>      <dbl>
     1 heart_failure                      23       25.3
     2 gastroesophageal_reflux_disease    19       20.9
     3 inflammatory_bowel_disease         19       20.9
     4 psychotic_disorder                 19       20.9
     5 atrial_fibrillation_incident       18       19.8
     6 heart_valve_disorder               18       19.8
     7 low_back_pain                      18       19.8
     8 occurrence_of_neuropathy           18       19.8
     9 peripheral_vascular_disease        18       19.8
    10 seizure                            18       19.8
    # ℹ 54 more rows

### Predicted risks

``` r
patient_risk_clean %>% 
  # Add up each of the columns of future risk.
  summarize(across(starts_with("predicted_risk_of_"), ~ sum(.x))) %>%
  # Convert the wide table into a longer one, with one row per risk.
  pivot_longer(
    cols = everything(),
    names_to = "predicted_risk_of",
    names_pattern = "predicted_risk_of_(.*)",
    values_to = "count"
    ) %>% 
  # Create a percentage column.
  mutate(percentage = count / nrow(patient_risk_clean) * 100) %>% 
  # Sort in descending order by the number of times a risk factor occurs.
  arrange(desc(count))
```

    # A tibble: 14 × 3
       predicted_risk_of                                            count percentage
       <chr>                                                        <dbl>      <dbl>
     1 muscle_weakness_or_injury                                   3.41       3.75  
     2 dementia                                                    2.81       3.09  
     3 sudden_hearing_loss_no_congenital_anomaly_or_middle_or_inn… 0.834      0.916 
     4 migraine                                                    0.787      0.864 
     5 ulcerative_colitis                                          0.467      0.513 
     6 pulmonary_embolism                                          0.354      0.389 
     7 treatment_resistant_depression_trd                          0.212      0.233 
     8 restless_leg_syndrome                                       0.184      0.203 
     9 parkinsons_disease_inpatient_or_with_2nd_diagnosis          0.115      0.127 
    10 acute_pancreatitis_with_no_chronic_or_hereditary_or_common… 0.0408     0.0448
    11 ankylosing_spondylitis                                      0.0406     0.0447
    12 multiple_sclerosis                                          0.0334     0.0368
    13 sudden_vision_loss_with_no_eye_pathology_causes             0.0245     0.0270
    14 autoimmune_hepatitis                                        0.0140     0.0154

``` r
rm(tuesdata)
rm(patient_risk_profiles)
rm(patient_risk_clean)
```

## References
