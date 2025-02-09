# Loaded necessary packages.
library(dplyr)
library(tidyr)
library(tidyverse)
library(data.table)
library(ggplot2)
library(haven)
library(kableExtra)
library(sjlabelled)
library(xtable)
library(ggpubr)
library(extrafont)
library(labelled)
library(stargazer)
library(networkD3)
library(webshot)
library(DescTools)
library(plm)
library(readr)
library(olsrr)
library(lmtest)
library(nlme)
library(fixest)
library(scales)
library(modelsummary)
library(tables)
library(tableone)
library(scales) 
library(fixest)
### Data preparation (this part and data filtration: courtesy of Simon Badertscher, mis WG-Gspänli)

# Load fonts (CMU Serif for closest match to LaTeX)
extrafont::loadfonts(device="all")
font_import("~/Library/CloudStorage/OneDrive-UniversitaetBern/UniBe/7 Semester/BA/cmu-serif (2)")
loadfonts()
font_import() # in case CMU Serif doesnt work for the graphs and plots

# Set working directory (the SHP data should be in this folder).
wd <- ("~/Library/CloudStorage/OneDrive-UniversitaetBern/UniBe/7 Semester/BA/Data")
setwd(wd)

# Load Covid Data
covid_data <- read.csv("~/Library/CloudStorage/OneDrive-UniversitaetBern/UniBe/7 Semester/BA/Data/OxCGRT_compact_national_v1.csv")
covid_data <- covid_data[32882:33977, ]

# check correlation between stringency index and covid cases
cor(covid_data$StringencyIndex_Average, covid_data$ConfirmedCases)


# Convert the 'date' column to a character vector and covert from yyyymmdd format to Date
covid_data$Date <- as.Date(as.character(covid_data$Date), format = "%Y%m%d")

# Create a "year" column from the Date column
covid_data$year <- format(covid_data$Date, "%Y")


# Aggregate data to yearly values
# Calculate the mean of the stringency index and the total cases for each year
yearly_covid_data <- covid_data %>%
  group_by(year) %>%
  summarize(
    avg_stringency = mean(StringencyIndex_Average, na.rm = TRUE),
    total_cases = sum(ConfirmedCases, na.rm = TRUE),
    yearly_new_cases = max(ConfirmedCases, na.rm = TRUE) - min(ConfirmedCases, na.rm = TRUE)  # Yearly cases
  )

# Create data frame to fill in all years prior to the pandemic with values 0
years_2000_to_2019 <- data.frame(
  year = as.character(2000:2019),   # Create a 'year' column
  avg_stringency = 0,               # Set avg_stringency to 0
  total_cases = 0,                   # Set total_cases to 0
  yearly_new_cases = 0              # Set yearly_new cases to 0
)

# Combine the new data frame (years 2000-2019) with the existing yearly_covid_data (2020-2022)
full_covid_data <- rbind(years_2000_to_2019, yearly_covid_data)

# Optional: Convert year column to a numeric type for easier sorting later (if needed)
full_covid_data$year <- as.numeric(full_covid_data$year)

# Sort by year (optional, if you want the data to be ordered)
full_covid_data <- full_covid_data[order(full_covid_data$year), ]






# Setting up the years and variables to then combine the annual personal and 
# annual household data.

year <- c("00","01","02", "03", "04", "05", "06", "07", "08", "09", 10:22)

setnames_variables_p <- c("person_id","househ_id","interview_type","age","sex",
                          "civstat","first_nat","sec_nat","no_kids","work_stat",
                          "priv_publ","part_fullt","eduyear","edulvl","workincgr_an","fin_sat","depr_freq","life_sat","contrWH")

life_sat_data <- read_dta("shp00_p_user.dta")

for (y in year) {
  # Loaded the data sets individually.
  file_path <- paste0("shp", y,"_p_user.dta")
  df <- read_dta(file_path)
  select_variables_p <- c("idpers",paste0("idhous",y),paste0("status",y),paste0("age",y),paste0("sex",y),
                          paste0("civsta",y),paste0("reg_1_",y),paste0("reg_2_",y),paste0("ownkid",y),
                          paste0("wstat",y),paste0("p",y,"w32"),paste0("p",y,"w39"),paste0("edyear",y),paste0("isced",y),
                          paste0("i",y,"wyg"),paste0("p",y,"i01"),paste0("p",y,"c17"),paste0("p",y,"c44"),paste0("p",y,"w74"))
  
  df <- df[select_variables_p]
  # Rename the chosen variables.
  setnames(df, select_variables_p,setnames_variables_p)
  # Keep only personal interviews.
  df <- df %>% filter(get(paste0("interview_type")) == 0)
  df <- df[, -which(names(df) == "interview_type")]
  # Add year as a variable.
  df$year <- as.numeric(paste0("20",y))
  
  # Save the annual adjusted dataset.
  write_dta(df, file.path(wd, paste0("temp_p", y, ".dta")))
}

# Load all datasets and combine them.
datasets <- list()
for (i in 1:length(year)){
  y <- year[i]
  datasets[[i]] <- read_dta(paste0("temp_p",y,".dta"))
}

temp_data <- bind_rows(datasets)
# Remove the original sets.
remove(datasets)

# Save the combined personal dataset.
write_dta(temp_data, "data_p.dta")


# Setting up the variables to then combine the annual household data.

setnames_variables_h <- c("househ_id","househ_type","new_born","hh_n_kids","h_you_kid","h_old_kid")

for (y in year) {
  file_path <- paste0("shp", y,"_h_user.dta")
  df <- read_dta(file_path)
  select_variables_h <- c(paste0("idhous",y),paste0("hldcen",y),paste0("nbb_",y),paste0("nbkid",y),paste0("ayouki",y),paste0("aoldki",y))
  # Select the chosen variables.
  df <- df[select_variables_h]
  # Rename the chosen variables.
  setnames(df, select_variables_h,setnames_variables_h)
  # Add year as a variable.
  df$year <- as.numeric(paste0("20",y))
  
  # Save the annual adjusted dataset.
  write_dta(df, file.path(wd, paste0("temp_h", y, ".dta")))
}


# Load all datasets and combine them.
datasets <- list()
for (i in 1:length(year)){
  y <- year[i]
  datasets[[i]] <- read_dta(paste0("temp_h",y,".dta"))
}
temp_data <- bind_rows(datasets)
# Remove the original sets.
remove(datasets)

write_dta(temp_data, "data_h.dta")
# Save the combined household dataset.


# Load both datasets.
data_p <- read_dta(paste0(wd,"/data_p.dta"))
data_h <- read_dta(paste0(wd,"/data_h.dta"))

# Merge the personal and household datasets.
data <- merge(data_p,data_h,by = c("househ_id","year"))

# Write and load the data (necessary only if the code is not run consecutively.)
write_dta(data, "raw_data.dta")

data <- read_dta("raw_data.dta")
#attach(data)


### Data filtration

# keep only positive values
data <- data[which(data$depr_freq >= 0),]
data <- data[which(data$workincgr_an >= 0),]
data <- data[which(data$civstat >= 0),]
data <- data[which(data$sex >= 0),]
data <- data[which(data$first_nat >= 0),]
data <- data[which(data$work_stat >= 0),]
data <- data[which(data$edulvl >= 0),]
data <- data[which(data$life_sat >= 0),]
data <- data[which(data$no_kids >= 0),]



data$fin_cris <- 0
data[data$year %in% c(2009),"fin_cris"] <- 1
data$covid_cris <- 0
data[data$year %in% c(2020:2022),"covid_cris"] <-1
data$covid_cris20 <- 0
data[data$year %in% c(2020),"covid_cris20"] <-1
data$covid_cris21 <- 0
data[data$year %in% c(2021),"covid_cris21"]<-1
data$covid_cris22 <- 0
data[data$year %in% c(2022),"covid_cris22"] <-1


# Recode age to age groups 16-24, 25-34, 35-44, 45-54, 55-69 and ???70
data$age_group <- cut(
  data$age,
  breaks = c(16, 24, 34, 44, 54, 69, Inf),  # Define age group boundaries
  labels = c("16-24", "25-34", "35-44", "45-54", "55-69", "???70"),  # Age group labels
  right = FALSE  # Include the left boundary, exclude the right boundary
)

data$age_group <- relevel(data$age_group, ref = "35-44")

# Recode nationality

data$first_nat[data$first_nat == 10] <- 0
data$first_nat[data$first_nat %in% c(11:17,31)] <- 1
data$first_nat[data$first_nat %in% c(20,30,40,50,60)] <- 2

attr(data[["first_nat"]], "labels") <- c("Switzerland" = 0, "Europe or North America" = 1
                                         ,"Rest of the world" = 2)
data <- mutate(data, first_nat = set_label(first_nat, label = "First nationality"))
data <- data[which(data$first_nat  %in% c(0:2)),]

# Recode gender.
data$sex[data$sex == 1] <- 0
data$sex[data$sex == 2] <- 1
attr(data[["sex"]], "labels") <- c("Male" = 0, "Female" = 1)
data <- mutate(data, sex = set_label(sex, label = "Gender"))
data <- data[which(data$sex < 3),]

# Recode marital status (only married/registered partnership or single remain).
data$civstat[data$civstat %in% c(1,3,4,5,7)] <- 0
data$civstat[data$civstat %in% c(2,6)] <- 1
attr(data[["civstat"]], "labels") <- c("Single" = 0, "Married or registered partnership" = 1)
data <- mutate(data, civstat = set_label(civstat, label = "Marital status"))
data <- data[which(data$civstat >= 0),]

# Recode employment status
data$work_stat[data$work_stat == 1] <- 0
data$work_stat[data$work_stat %in% c(2,3)] <- 1
attr(data[["work_stat"]], "labels") <- c("Employed" = 0, "Unemployed or not in labour force" = 1)
data <- mutate(data, work_stat = set_label(work_stat, label = "Employment Status"))

# Recode life satisfaction
attr(data[["life_sat"]], "labels") <- c("Not at all satisfied" = 0, "Completely satisfied" = 10)

# Recode Highest level of education
data$edulvl[data$edulvl ==  10] <- 0
data$edulvl[data$edulvl == 20] <- 1
data$edulvl[data$edulvl == 31] <- 2
data$edulvl[data$edulvl == 32] <- 3
data$edulvl[data$edulvl == 33] <- 4
data$edulvl[data$edulvl == 41] <- 5
data$edulvl[data$edulvl %in% c(51,52,60)] <- 6

attr(data$edulvl,"labels") <- c(
  "Primary or first stage of basic education" = 0,
  "Lower secondary or Second stage of basic education" = 1,
  "Upper secondary education (preparation for tertiary education"= 2,
  "Upper secondary education (preparation for further prof. education)" = 3,
  "Upper secondary education (entrance into the labor market)" = 4,
  "Post-secondary education non tertiary (preparation for an institution for higher education)" = 5,
  "First or second stage of tertiary education"= 6)
data$edulvl <- set_label(data$edulvl, label = "Highest level of education")
data$year <- as.numeric(data$year)


# Merge the datasets by 'year' column
combined_data <- merge(data, full_covid_data, by = "year", all.x = TRUE)

# Filter for entries from 2018 onward
eligible_individuals <- combined_data %>%
  filter(year >= 2018) %>%
  distinct(person_id)  # Keep only unique IDs

# Determine all years from 2018 onward in your dataset
required_years <- seq(2018, max(combined_data$year))

# Identify individuals who have entries in all required years
eligible_individuals <- combined_data %>%
  filter(year %in% required_years) %>%     # Keep only records from 2018 onward
  group_by(person_id) %>%
  filter(all(required_years %in% year)) %>% # Check if each ID has all required years
  distinct(person_id)                              # Keep only the unique IDs

# Keep only rows for individuals in eligible_individuals
filtered_data <- combined_data %>%
  semi_join(eligible_individuals, by = "person_id")


# Balance the panel

# Define the required years
required_years <- 2018:2022

# Count the number of unique years each individual appears in
balanced_ids <- combined_data %>%
  filter(year %in% required_years) %>%
  group_by(person_id) %>%
  summarize(year_count = n_distinct(year)) %>%
  filter(year_count == length(required_years)) %>%
  pull(person_id)

# Keep only rows corresponding to individuals who are in all required years
balanced_panel <- combined_data %>%
  filter(person_id %in% balanced_ids, year %in% required_years)

#adjust income for inflation
#create cpi_dataframe with data from the BFS
cpi_data <- data.frame(
  year = c(2018,2019,2020,2021,2022),
  cpi = c(101.7,102,101.3,101.9,104.8) 
)

#merge income data with cpi data
balanced_panel <- merge(balanced_panel, cpi_data, by = "year")

# Reference CPI for the base year (2018)
reference_cpi <- balanced_panel$cpi[balanced_panel$year == 2018] [1]

# Adjusting the income for inflation
balanced_panel$real_income <- balanced_panel$workincgr_an * reference_cpi / balanced_panel$cpi

#create full time equivalent salaries
#balanced_panel$real_fte_salaries <- (balanced_panel$real_income/balanced_panel$contrWH)*40

# Convert to pdata.frame for plm package
pdata <- pdata.frame(balanced_panel, index = c("person_id", "year"))

# Check if the panel is balanced
panel_info <- pdim(pdata)
print(panel_info)


# quality of ggsave
dpi_ggs <- 1000

### Check whether balanced panel is still representative
# Keep only years 2018-2022 for the original set, only those are of relevance for comparison

data_18to22 <- data[data$year >= 2018, ]
data_18to22 <- merge(data_18to22, cpi_data, by = "year")
data_18to22$real_income <- data_18to22$workincgr_an * reference_cpi / data_18to22$cpi

pdata <- pdata %>% mutate(dataset = "pdata")
data_18to22 <- data_18to22 %>% mutate(dataset = "data_18to22")

max_age_data <- data.frame(
  value = c(balanced_panel$age, as.numeric(data_18to22$age)),
  Panel = c(rep("Balanced", length(balanced_panel$age)), rep("Original", length(data_18to22$age)))
)

gg_density_comp_age <- ggplot(max_age_data, aes(x = value, color = Panel, fill = Panel)) +
  geom_density(alpha = 0.3) +
  labs(
    title = "Age density comparison between both datasets",
    x = "Age",
    y = "Density"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("#5F9EA0", "red")) +
  scale_fill_manual(values = c("#5F9EA0", "red")) +
  theme(text = element_text(family = "CMU Serif"),
        axis.title = element_text(size=18),
        axis.text = element_text(size=18),         
        plot.title = element_text(size = 20, hjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10))) +
  theme(legend.position = "top")
gg_density_comp_age

#ggsave("gg_density_comp_age.png", gg_density_comp_age,dpi = dpi_ggs, width = 8, height = 5)


#density comparison for real_income, remove outliers so that the plot becomes meaningful

# Compute the 99.9th percentile cutoff for real_income
cutoff_income <- quantile(data_18to22$real_income, 0.995, na.rm = TRUE)

# Remove outliers (top 0.1%)
data_18to22_filtered <- data_18to22 %>%
  filter(real_income <= cutoff_income)
balanced_panel_filtered <- balanced_panel %>%
  filter(real_income <= cutoff_income)

max_data_inc <- data.frame(
  value = c(balanced_panel_filtered$real_income, as.numeric(data_18to22_filtered$real_income)),
  Region = c(rep("Balanced Panel", length(balanced_panel_filtered$real_income)), rep("Original Panel", length(data_18to22_filtered$real_income)))
)

gg_density_comp_inc <- ggplot(max_data_inc, aes(x = value, color = Region, fill = Region)) +
  geom_density(alpha = 0.3) +
  labs(
    title = "Income density comparison between both datasets",
    x = "Real Income",
    y = "Density"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("#5F9EA0", "red")) +
  scale_fill_manual(values = c("#5F9EA0", "red")) +
  theme(text = element_text(family = "CMU Serif"),
        axis.title = element_text(size=18),
        axis.text = element_text(size=18),         
        plot.title = element_text(size = 20, hjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10))) +
  theme(legend.position = "top")

# Needed for comma formatting

gg_density_comp_inc <- gg_density_comp_inc +
  scale_x_continuous(labels = comma) # Converts scientific notation to normal numbers
gg_density_comp_inc

#ggsave("gg_density_comp_inc.png", gg_density_comp_inc,dpi = dpi_ggs, width = 8, height = 5)



# Add a new column to label the dataset source
data_18to22 <- data_18to22 %>% mutate(dataset = "Original")
balanced_panel <- balanced_panel %>% mutate(dataset = "Balanced")

# combine original and balanced panel
combined_data <- bind_rows(data_18to22, balanced_panel)

# specify variables for comparison
covariates <- c("sex", "civstat", "first_nat", "no_kids", "work_stat",
                "edulvl", "life_sat", "age", "real_income")

# Create a TableOne object, stratifying by dataset
cov_balance_table <- CreateTableOne(vars = covariates,
                                    strata = "dataset",
                                    data = combined_data,
                                    test = FALSE)  # set test = TRUE if you want p-values

# Print table with standardized mean differences (SMD)
# SMDs are useful for assessing whether the balancing has reduced differences between groups. 
# Typically, an SMD of less than 0.1 or 0.2 is considered acceptable.
print(cov_balance_table, smd = TRUE)
# Capture the TableOne output (with SMDs) as a character matrix
tex_table <- print(cov_balance_table, smd = TRUE, printToggle = FALSE)

# Convert the character matrix to an xtable object
latex_table <- xtable(tex_table)

# Print the xtable object with LaTeX output
print(latex_table, type = "latex", include.rownames = TRUE)


#proportions of first nationality

# Function to calculate proportions of a categorical variable
calculate_proportions <- function(data, variable) {
  prop_table <- table(data[[variable]]) %>%
    prop.table() %>%
    round(2)
  return(prop_table)
}
# Calculate proportions for balanced_panel
prop_first_nat_balanced_panel <- calculate_proportions(balanced_panel, "first_nat")

# Calculate proportions for data_18to22
prop_first_nat_data_18to22 <- calculate_proportions(data_18to22, "first_nat")

# Print the results
print("Proportions of first nationality for balanced_panel:")
print(prop_first_nat_balanced_panel)

print("Proportions of first nationality for data_18to22:")
print(prop_first_nat_data_18to22)




# time dynamics
# observations over the years

year_counts_comp <- combined_data %>%
  group_by(dataset, year) %>%
  summarise(n = n(), .groups = "drop")

print(year_counts_comp)

year_age_comp <- combined_data %>%
  group_by(dataset, year) %>%
  summarise(mean_age = mean(age, na.rm = TRUE), .groups = "drop")

print(year_age_comp)

# Plot the counts per year for each dataset
gg_year_counts_comp <- ggplot(year_counts_comp, aes(x = year, y = n, fill = dataset)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Distribution of Observations Over Years",
       x = "Year",
       y = "Number of Observations") +
  theme_minimal() +
  scale_color_manual(values = c("#5F9EA0", "red")) +
  scale_fill_manual(values = c("#5F9EA0", "red"))
gg_year_counts_comp
#ggsave("gg_year_counts_comp.png", gg_year_counts_comp,dpi = dpi_ggs, width = 8, height = 5)

# plot the mean age per year for each dataset
gg_year_age_comp <- ggplot(year_age_comp, aes(x = year, y = mean_age, color = dataset, group = dataset)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(title = "Mean Age per Year by Dataset",
       x = "Year",
       y = "Mean Age") +
  theme_minimal() +
  scale_color_manual(values = c("Original" = "#5F9EA0", "Balanced" = "red"))
gg_year_age_comp
#ggsave("gg_year_age_comp.png", gg_year_age_comp,dpi = dpi_ggs, width = 8, height = 5)

#plot average stringency index over the years

stringency_trend <- balanced_panel %>%
  select(year, avg_stringency) %>%
  distinct()

gg_stringency <- ggplot(stringency_trend, aes(x = year, y = avg_stringency)) +
  geom_line(size = 1, color = "#5F9EA0") +
  geom_point(size = 3, color = "#5F9EA0") +
  labs(title = "Average Stringency per Year",
       x = "Year",
       y = "Average Stringency") +
  theme_minimal()
gg_stringency
#ggsave("gg_stringency.png", gg_stringency,dpi = dpi_ggs, width = 8, height = 5)


# (b) Summarize key continuous covariates ( age and real_income) by year.
#     verify that the means and variances remain comparable over
#     the years between the original and balanced sample

year_summary <- combined_data %>%
  group_by(dataset, year) %>%
  summarise(
    mean_age    = mean(age, na.rm = TRUE),
    sd_age      = sd(age, na.rm = TRUE),
    mean_income = mean(real_income, na.rm = TRUE),
    sd_income   = sd(real_income, na.rm = TRUE),
    .groups     = "drop"
  )

print(year_summary)



# plot the time trends for real_income by year for both datasets
gg_year_inc_comp <- ggplot(year_summary, aes(x = year, y = mean_income, color = dataset, group = dataset)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Mean Real Income Over Years",
       x = "Year",
       y = "Mean Real Income (inflation-adjusted)") +
  theme_minimal() +
  scale_color_manual(values = c("#5F9EA0", "red")) +
  scale_fill_manual(values = c("#5F9EA0", "red"))
gg_year_inc_comp
#ggsave("gg_year_inc_comp.png", gg_year_inc_comp,dpi = dpi_ggs, width = 8, height = 5)
















### Descriptive Analysis



#Age distribution bar plot

# Step 1: Filter data for the year 2018
balpan_2018 <- balanced_panel %>% filter(year == 2018)

mean(balpan_2018$age)

# Step 2: Create a bar plot of age distribution
gg_age_distr._2018_barplot <- ggplot(balpan_2018, aes(x = age)) +
  geom_bar(fill = "#5F9EA0", color = "black") +
  labs(
    title = "Age Distribution for 2018",
    x = "Age",
    y = "Frequency"
  ) +
  theme(text = element_text(family = "CMU Serif"),
        axis.title = element_text(size=18),
        axis.text = element_text(size=18),         
        plot.title = element_text(size = 20, hjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)))


#ggsave("gg_age_distr._2018_barplot.png", gg_age_distr._2018_barplot,dpi = dpi_ggs, width = 8, height = 5)

gg_age_distr._2018_barplot

#generate annual mean life satisfaction

annual_mean_life_sat <- balanced_panel %>%
  group_by(year) %>%
  summarize(mean_value = mean(life_sat))

# Create bar plot of mean life satisfaction per year
gg_mean_life_sat_annual <- ggplot(data= annual_mean_life_sat, aes(x = factor(year), y = mean_value)) +
  geom_col(fill = "#5F9EA0", color = "black") +
  labs(
    title = "Mean life satisfaction per year",
    x = "Year",
    y = "Mean life satisfaction"
  ) +
  coord_cartesian(ylim = c(7.75,8.25)) +
  theme(text = element_text(family = "CMUSerif-Roman"),
        axis.title = element_text(size=18),
        axis.text = element_text(size=18),         
        plot.title = element_text(size = 20, hjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10))
  )

gg_mean_life_sat_annual
#ggsave("gg_mean_life_sat_annual.png", gg_mean_life_sat_annual,dpi = dpi_ggs, width = 8, height = 5)


# Create boxplot for income by year

income_data <- data.frame(pdata$real_income, pdata$year)
colnames(income_data) <- c("real_income", "year")

# Compute the 99.9th percentile cutoff for real_income
cutoff_income_boxplot <- quantile(income_data$real_income, 0.999, na.rm = TRUE)

# Remove outliers (top 0.1%)
income_data_filtered <- income_data %>%
  filter(real_income <= cutoff_income)

boxplot_real_income <- ggplot(income_data_filtered, aes(x = as.factor(year), y = real_income)) +
  geom_boxplot(fill = "#5F9EA0", color = "black") +
  scale_y_continuous(labels = label_number(big.mark = ))+
  #coord_cartesian(ylim = c(0,3e5))+
  labs(x = "Year",
       y = "Real income",
       fill = "Year", title = "Boxplots of real income by year") +
  theme(text = element_text(family = "CMU Serif"),
        plot.title = element_text(size=20,hjust = 0.5),
        axis.title = element_text(size=18),        
        axis.text = element_text(size=18),         
        legend.text = element_text(size=14),       
        legend.title = element_text(size=14),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)))

boxplot_real_income
#ggsave("boxplot_real_income.png", boxplot_real_income,dpi = dpi_ggs, width = 8, height = 5)


# summary statistic

# Create a quintile-based factor for real_income
balanced_panel <- balanced_panel %>%
  mutate(
    real_income_quintile = ntile(real_income, 5)  # Divide into 5 quintiles
  )

# Ensure categorical variables are factors
balanced_panel <- balanced_panel %>%
  mutate(
    first_nat = factor(first_nat),
    age_group = factor(age_group),
    sex = factor(sex),
    civstat = factor(civstat),
    work_stat = factor(work_stat),
    edulvl = factor(edulvl),
  )

# Calculate summary stats for sex
sex_summary <- balanced_panel %>%
  group_by(sex) %>%
  summarise(
    n = n(),
    mean_life_sat = mean(life_sat, na.rm = TRUE),
    sd_life_sat = sd(life_sat, na.rm = TRUE),
    ci_lower = mean_life_sat - qt(0.975, df = n - 1) * sd_life_sat / sqrt(n),
    ci_upper = mean_life_sat + qt(0.975, df = n - 1) * sd_life_sat / sqrt(n),
    .groups = 'drop'
  )

# Calculate summary stats for age_group
age_group_summary <- balanced_panel %>%
  group_by(age_group) %>%
  summarise(
    n = n(),
    mean_life_sat = mean(life_sat, na.rm = TRUE),
    sd_life_sat = sd(life_sat, na.rm = TRUE),
    ci_lower = mean_life_sat - qt(0.975, df = n - 1) * sd_life_sat / sqrt(n),
    ci_upper = mean_life_sat + qt(0.975, df = n - 1) * sd_life_sat / sqrt(n),
    .groups = 'drop'
  )

# Calculate summary stats for civstat
civstat_summary <- balanced_panel %>%
  group_by(civstat) %>%
  summarise(
    n = n(),
    mean_life_sat = mean(life_sat, na.rm = TRUE),
    sd_life_sat = sd(life_sat, na.rm = TRUE),
    ci_lower = mean_life_sat - qt(0.975, df = n - 1) * sd_life_sat / sqrt(n),
    ci_upper = mean_life_sat + qt(0.975, df = n - 1) * sd_life_sat / sqrt(n),
    .groups = 'drop'
  )

# Calculate summary stats for real income quantile
real_income_quintile_summary <- balanced_panel %>%
  group_by(real_income_quintile) %>%
  summarise(
    n = n(),
    mean_life_sat = mean(life_sat, na.rm = TRUE),
    sd_life_sat = sd(life_sat, na.rm = TRUE),
    ci_lower = mean_life_sat - qt(0.975, df = n - 1) * sd_life_sat / sqrt(n),
    ci_upper = mean_life_sat + qt(0.975, df = n - 1) * sd_life_sat / sqrt(n),
    .groups = 'drop'
  )
# Calculate summary stats for first nationality
first_nat_summary <- balanced_panel %>%
  group_by(first_nat) %>%
  summarise(
    n = n(),
    mean_life_sat = mean(life_sat, na.rm = TRUE),
    sd_life_sat = sd(life_sat, na.rm = TRUE),
    ci_lower = mean_life_sat - qt(0.975, df = n - 1) * sd_life_sat / sqrt(n),
    ci_upper = mean_life_sat + qt(0.975, df = n - 1) * sd_life_sat / sqrt(n),
    .groups = 'drop'
  )

# Calculate summary stats for working status
work_stat_summary <- balanced_panel %>%
  group_by(work_stat) %>%
  summarise(
    n = n(),
    mean_life_sat = mean(life_sat, na.rm = TRUE),
    sd_life_sat = sd(life_sat, na.rm = TRUE),
    ci_lower = mean_life_sat - qt(0.975, df = n - 1) * sd_life_sat / sqrt(n),
    ci_upper = mean_life_sat + qt(0.975, df = n - 1) * sd_life_sat / sqrt(n),
    .groups = 'drop'
  )
# Calculate summary stats for education level
edulvl_summary <- balanced_panel %>%
  group_by(edulvl) %>%
  summarise(
    n = n(),
    mean_life_sat = mean(life_sat, na.rm = TRUE),
    sd_life_sat = sd(life_sat, na.rm = TRUE),
    ci_lower = mean_life_sat - qt(0.975, df = n - 1) * sd_life_sat / sqrt(n),
    ci_upper = mean_life_sat + qt(0.975, df = n - 1) * sd_life_sat / sqrt(n),
    .groups = 'drop'
  )

# Combine all summary statistics into a single data frame
combined_summary <- bind_rows(
  mutate(sex_summary, factor_name = "Sex"),
  mutate(age_group_summary, factor_name = "Age Group"),
  mutate(edulvl_summary, factor_name = "Education Level"),
  mutate(real_income_quintile_summary, factor_name = "Real income quintile"),
  mutate(work_stat_summary, factor_name = "Working Status"),
  mutate(first_nat_summary, factor_name = "First Nationality"),
  mutate(civstat_summary, factor_name = "Civil status")
)


# Reshape data so each factor's statistics are in columns
stargazer_data <- combined_summary %>%
  select(factor_name, mean_life_sat, ci_lower, ci_upper) %>%
  arrange(factor_name)

# Add column names to match the format
colnames(stargazer_data) <- c("", "Mean Life Sat", "Lower CI", "Upper CI")

# Round the numeric columns manually
stargazer_data$`Mean Life Sat` <- round(stargazer_data$`Mean Life Sat`, 2)
stargazer_data$`Lower CI` <- round(stargazer_data$`Lower CI`, 2)
stargazer_data$`Upper CI` <- round(stargazer_data$`Upper CI`, 2)


# Use stargazer to display summary statistics
stargazer(
  stargazer_data,
  type = "text",   
  summary = FALSE, 
  digits = 2,      # Round the numbers to two decimal places
  rownames = FALSE, # already have factor names in the first column
  # column.labels = c("Mean Life Sat", "Lower CI", "Upper CI"),  # Column titles
  covariate.labels = stargazer_data$Factor,  # Factor labels will be shown as row labels
  table.layout = "=c"  # Keep a clean layout (no extra lines or features)
  #,out = "summary_table.tex"
)


# now the same just for 2018

#create income quantiles
balpan_2018 <- balpan_2018 %>%
  mutate(
    real_income_quintile = ntile(real_income, 5)  # Divide into 5 quintiles
  )

# Ensure categorical variables are factors
balpan_2018 <- balpan_2018 %>%
  mutate(
    first_nat = factor(first_nat),
    age_group = factor(age_group),
    sex = factor(sex),
    civstat = factor(civstat),
    work_stat = factor(work_stat),
    edulvl = factor(edulvl),
  )

# Calculate summary stats for sex
sex_summary18 <- balpan_2018 %>%
  group_by(sex) %>%
  summarise(
    n = n(),
    mean_life_sat = mean(life_sat, na.rm = TRUE),
    sd_life_sat = sd(life_sat, na.rm = TRUE),
    ci_lower = mean_life_sat - qt(0.975, df = n - 1) * sd_life_sat / sqrt(n),
    ci_upper = mean_life_sat + qt(0.975, df = n - 1) * sd_life_sat / sqrt(n),
    .groups = 'drop'
  )

# Calculate summary stats for age_group
age_group_summary18 <- balpan_2018 %>%
  group_by(age_group) %>%
  summarise(
    n = n(),
    mean_life_sat = mean(life_sat, na.rm = TRUE),
    sd_life_sat = sd(life_sat, na.rm = TRUE),
    ci_lower = mean_life_sat - qt(0.975, df = n - 1) * sd_life_sat / sqrt(n),
    ci_upper = mean_life_sat + qt(0.975, df = n - 1) * sd_life_sat / sqrt(n),
    .groups = 'drop'
  )

# Calculate summary stats for civstat
civstat_summary18 <- balpan_2018 %>%
  group_by(civstat) %>%
  summarise(
    n = n(),
    mean_life_sat = mean(life_sat, na.rm = TRUE),
    sd_life_sat = sd(life_sat, na.rm = TRUE),
    ci_lower = mean_life_sat - qt(0.975, df = n - 1) * sd_life_sat / sqrt(n),
    ci_upper = mean_life_sat + qt(0.975, df = n - 1) * sd_life_sat / sqrt(n),
    .groups = 'drop'
  )

# Calculate summary stats for real income quantile
real_income_quintile_summary18 <- balpan_2018 %>%
  group_by(real_income_quintile) %>%
  summarise(
    n = n(),
    mean_life_sat = mean(life_sat, na.rm = TRUE),
    sd_life_sat = sd(life_sat, na.rm = TRUE),
    ci_lower = mean_life_sat - qt(0.975, df = n - 1) * sd_life_sat / sqrt(n),
    ci_upper = mean_life_sat + qt(0.975, df = n - 1) * sd_life_sat / sqrt(n),
    .groups = 'drop'
  )
# Calculate summary stats for first nationality
first_nat_summary18 <- balpan_2018 %>%
  group_by(first_nat) %>%
  summarise(
    n = n(),
    mean_life_sat = mean(life_sat, na.rm = TRUE),
    sd_life_sat = sd(life_sat, na.rm = TRUE),
    ci_lower = mean_life_sat - qt(0.975, df = n - 1) * sd_life_sat / sqrt(n),
    ci_upper = mean_life_sat + qt(0.975, df = n - 1) * sd_life_sat / sqrt(n),
    .groups = 'drop'
  )

# Calculate summary stats for working status
work_stat_summary18 <- balpan_2018 %>%
  group_by(work_stat) %>%
  summarise(
    n = n(),
    mean_life_sat = mean(life_sat, na.rm = TRUE),
    sd_life_sat = sd(life_sat, na.rm = TRUE),
    ci_lower = mean_life_sat - qt(0.975, df = n - 1) * sd_life_sat / sqrt(n),
    ci_upper = mean_life_sat + qt(0.975, df = n - 1) * sd_life_sat / sqrt(n),
    .groups = 'drop'
  )
# Calculate summary stats for education level
edulvl_summary18 <- balpan_2018 %>%
  group_by(edulvl) %>%
  summarise(
    n = n(),
    mean_life_sat = mean(life_sat, na.rm = TRUE),
    sd_life_sat = sd(life_sat, na.rm = TRUE),
    ci_lower = mean_life_sat - qt(0.975, df = n - 1) * sd_life_sat / sqrt(n),
    ci_upper = mean_life_sat + qt(0.975, df = n - 1) * sd_life_sat / sqrt(n),
    .groups = 'drop'
  )
balpan_2018$has_kids <- ifelse(balpan_2018$no_kids > 0, 1, 0)
# Calculate summary stats for having kids
has_kids_summary18 <- balpan_2018 %>%
  group_by(has_kids) %>%
  summarise(
    n = n(),
    mean_life_sat = mean(life_sat, na.rm = TRUE),
    sd_life_sat = sd(life_sat, na.rm = TRUE),
    ci_lower = mean_life_sat - qt(0.975, df = n - 1) * sd_life_sat / sqrt(n),
    ci_upper = mean_life_sat + qt(0.975, df = n - 1) * sd_life_sat / sqrt(n),
    .groups = 'drop'
  )

# Combine all summary statistics into a single data frame
combined_summary18 <- bind_rows(
  mutate(sex_summary18, factor_name = "Sex"),
  mutate(age_group_summary18, factor_name = "Age Group"),
  mutate(edulvl_summary18, factor_name = "Education Level"),
  mutate(real_income_quintile_summary18, factor_name = "Real income quintile"),
  mutate(work_stat_summary18, factor_name = "Working Status"),
  mutate(first_nat_summary18, factor_name = "First Nationality"),
  mutate(civstat_summary18, factor_name = "Civil status"),
  mutate(has_kids_summary18, factor_name = "Kids in household")
)

interesting_data <- combined_summary18 %>%
  select(factor_name, mean_life_sat, ci_lower, ci_upper) %>%
  arrange(factor_name)
# Reshape data so each factor's statistics are in columns
stargazer_data18 <- combined_summary18 %>%
  select(factor_name, mean_life_sat, ci_lower, ci_upper) %>%
  arrange(factor_name)

# Add column names to match the format
colnames(stargazer_data18) <- c("", "Mean Life Sat", "Lower CI", "Upper CI")

#adjust row labelling, double and triple because either the stargazer package is buggy, or i simply don't get it
rownames(stargazer_data18)<- c("35-44", "16-24", "25-34", "45-54", "55-69", "70 and above", "Single", "In partnership",
                               "Edulvl 0", "Edulvl 1", "Edulvl 2", "Edulvl 3", "Edulvl 4", 
                               "Edulvl 5", "Edulvl 6", "Swiss", "Europe or NA", "Rest of the world", "No kids",
                               "One or more kids", "lowest inc qt", 
                               "2nd inc qt", "3rd inc qt", "4th inc qt", "Highest inc qt", "Male", "Female", 
                               "Employed", "Unemployed")
stargazer_data18 <- stargazer_data18[c("16-24", "25-34", "35-44", "45-54", "55-69", "70 and above", "Single", "In partnership",
                                       "Edulvl 0", "Edulvl 1", "Edulvl 2", "Edulvl 3", "Edulvl 4", 
                                       "Edulvl 5", "Edulvl 6", "Swiss", "Europe or NA", "Rest of the world", "No kids",
                                       "One or more kids", "lowest inc qt", 
                                       "2nd inc qt", "3rd inc qt", "4th inc qt", "Highest inc qt", "Male", "Female", 
                                       "Employed", "Unemployed"), ]
rownames(stargazer_data18) <- c("16-24", "25-34", "35-44", "45-54", "55-69", "70 and above", "Single", "In partnership",
                                "Edulvl 0", "Edulvl 1", "Edulvl 2", "Edulvl 3", "Edulvl 4", 
                                "Edulvl 5", "Edulvl 6", "Swiss", "Europe or NA", "Rest of the world", "No kids",
                                "One or more kids", "lowest inc qt", 
                                "2nd inc qt", "3rd inc qt", "4th inc qt", "Highest inc qt", "Male", "Female", 
                                "Employed", "Unemployed")

# Convert row names into a separate column in stargazer_data
stargazer_data18$Group <- rownames(stargazer_data18)

# Reorder columns so "Group" is the first column
stargazer_data18 <- stargazer_data18[, c("Group", "Mean Life Sat", "Lower CI", "Upper CI")]

# Round the numeric columns manually
stargazer_data18$`Mean Life Sat` <- round(stargazer_data18$`Mean Life Sat`, 2)
stargazer_data18$`Lower CI` <- round(stargazer_data18$`Lower CI`, 2)
stargazer_data18$`Upper CI` <- round(stargazer_data18$`Upper CI`, 2)

#reassign column names because stargazer be stargazing

colnames(stargazer_data18) <- c("", "Mean Life Sat", "Lower CI", "Upper CI")

# Use stargazer to display summary statistics
stargazer(
  stargazer_data18,
  type = "text",   
  summary = FALSE, 
  digits = 2, # Round the numbers to two decimal places
  rownames = FALSE,
  # column.labels = c("", "Mean Life Sat", "Lower CI", "Upper CI"),
  covariate.labels = stargazer_data18$Group, # We already have factor names in the first column
  # covariate.labels = c("16-24", "25-34", "35-44", "45-54", "55-69", "70 and above", "Single", "In partnership",
  #"Edulvl 0", "Edulvl 1", "Edulvl 2", "Edulvl 3", "Edulvl 4", 
  #"Edulvl 5", "Edulvl 6", "Swiss", "Europe or NA", "Rest of the world", "lowest inc qt", 
  #"2nd inc qt", "3rd inc qt", "4th inc qt", "Highest inc qt", "Male", "Female", 
  #"Employed", "Unemployed"),  # row names as labels
  table.layout = "=c"  # Keep a clean layout (no extra lines or features)
  #out = "summary_table18_rownames_ordered_kids.tex"
)




















### Main regressions
## assign individuals according to their age groups in the base year 2018

# Define age groups
age_groups <- c("16-24", "25-34", "35-44", "45-54", "55-69", "???70")

# Create empty lists to store results
valid_ids_age_list <- list()
pdata_age_list <- list()

# Loop through each age group and fix classification based on 2018
for (age in age_groups) {
  # Step 1: Identify individuals who were in this age group in 2018
  valid_ids_age_list[[age]] <- unique(pdata$person_id[pdata$age_group == age & pdata$year == 2018])
  
  # Step 2: Keep all their data across all years
  pdata_age_list[[age]] <- pdata[pdata$person_id %in% valid_ids_age_list[[age]], ]
}

# Assign individual variables for each age group
valid_ids_1624 <- valid_ids_age_list[["16-24"]]
pdata_1624 <- pdata_age_list[["16-24"]]

valid_ids_2534 <- valid_ids_age_list[["25-34"]]
pdata_2534 <- pdata_age_list[["25-34"]]

valid_ids_3544 <- valid_ids_age_list[["35-44"]]
pdata_3544 <- pdata_age_list[["35-44"]]

valid_ids_4554 <- valid_ids_age_list[["45-54"]]
pdata_4554 <- pdata_age_list[["45-54"]]

valid_ids_5569 <- valid_ids_age_list[["55-69"]]
pdata_5569 <- pdata_age_list[["55-69"]]

valid_ids_70 <- valid_ids_age_list[["???70"]]
pdata_70 <- pdata_age_list[["???70"]]


# FE model for each age group separately for heterogeneity analysis

FE_age_1624 <- plm(life_sat ~ avg_stringency + factor(first_nat) +
                     factor(sex) + factor(civstat) + no_kids + factor(work_stat) + edulvl +
                     log(real_income) + factor(year),
                   data=pdata_1624, model = "within", index = c("person_id"), effect = "individual")

FE_age_1624_feols <- feols(life_sat ~ avg_stringency + factor(first_nat) +
                             factor(sex) + factor(civstat) + no_kids + factor(work_stat) + edulvl +
                             log(real_income) + factor(year) | person_id, data = pdata_1624)

FE_age_2534 <- plm(life_sat ~ avg_stringency + factor(first_nat) +
                     factor(sex) + factor(civstat) + no_kids + factor(work_stat) + edulvl +
                     log(real_income) + factor(year),
                   data=pdata_2534, model = "within", index = c("person_id"), effect = "individual")

FE_age_3544 <- plm(life_sat ~ avg_stringency + factor(first_nat) +
                     factor(sex) + factor(civstat) + no_kids + factor(work_stat) + edulvl +
                     log(real_income) + factor(year),
                   data=pdata_3544, model = "within", index = c("person_id"), effect = "individual")

FE_age_3544_feols <- feols(life_sat ~ avg_stringency + factor(first_nat) +
                             factor(sex) + factor(civstat) + no_kids + factor(work_stat) + edulvl +
                             log(real_income) + factor(year) | person_id, data = pdata_3544)

FE_age_4554 <- plm(life_sat ~ avg_stringency + factor(first_nat) +
                     factor(sex) + factor(civstat) + no_kids + factor(work_stat) + edulvl +
                     log(real_income) + factor(year),
                   data=pdata_4554, model = "within", index = c("person_id"), effect = "individual")

FE_age_5569 <- plm(life_sat ~ avg_stringency + factor(first_nat) +
                     factor(sex) + factor(civstat) + no_kids + factor(work_stat) + edulvl +
                     log(real_income) + factor(year),
                   data=pdata_5569, model = "within", index = c("person_id"), effect = "individual")

FE_age_70 <- plm(life_sat ~ avg_stringency + factor(first_nat) +
                   factor(sex) + factor(civstat) + no_kids + factor(work_stat) + edulvl +
                   log(real_income) + factor(year),
                 data=pdata_70, model = "within", index = c("person_id"), effect = "individual")

summary(FE_age_1624)
summary(FE_age_2534)
summary(FE_age_3544)
summary(FE_age_4554)
summary(FE_age_5569)
summary(FE_age_70)

bptest(FE_age_1624)
bptest(FE_age_2534)
bptest(FE_age_3544)
bptest(FE_age_4554)
bptest(FE_age_5569)
bptest(FE_age_70)

library(sandwich)

# Function to determine the type that gives the largest robust SE
get_largest_vcovHC <- function(model) {
  hc_types <- c("HC0", "sss", "HC1", "HC2", "HC3", "HC4")  # Available types
  
  # Compute standard errors for each type
  se_list <- lapply(hc_types, function(type) {
    return(sqrt(diag(vcovHC(model, type = type))))
  })
  
  names(se_list) <- hc_types  # Assign names to list
  
  # Identify the type that produces the largest SE for each coefficient
  max_se_per_coef <- sapply(1:length(se_list[[1]]), function(i) {
    hc_type_max <- names(se_list)[which.max(sapply(se_list, function(x) x[i]))]
    return(list(se = max(sapply(se_list, function(x) x[i])), type = hc_type_max))
  })
  
  return(max_se_per_coef)  # Return largest SE and its type
}

largest_se_FE_age_1624 <- get_largest_vcovHC(FE_age_1624)
largest_se_FE_age_2534 <- get_largest_vcovHC(FE_age_2534)
largest_se_FE_age_3544 <- get_largest_vcovHC(FE_age_3544)
largest_se_FE_age_4554 <- get_largest_vcovHC(FE_age_4554)
largest_se_FE_age_5569 <- get_largest_vcovHC(FE_age_5569)
largest_se_FE_age_70 <- get_largest_vcovHC(FE_age_70)

# Print results
print(largest_se_FE_age_1624)
print(largest_se_FE_age_2534)
print(largest_se_FE_age_3544)
print(largest_se_FE_age_4554)
print(largest_se_FE_age_5569)
print(largest_se_FE_age_70)



robust_se_age_1624 <- sqrt(diag(vcovHC(FE_age_1624, type = "HC3",cluster = "group")))
robust_se_age_2534 <- sqrt(diag(vcovHC(FE_age_2534, type = "HC4",cluster = "group")))
robust_se_age_3544 <- sqrt(diag(vcovHC(FE_age_3544, type = "HC4",cluster = "group")))
robust_se_age_4554 <- sqrt(diag(vcovHC(FE_age_4554, type = "HC4",cluster = "group")))
robust_se_age_5569 <- sqrt(diag(vcovHC(FE_age_5569, type = "HC4",cluster = "group")))
robust_se_age_70   <- sqrt(diag(vcovHC(FE_age_70, type = "HC3",cluster = "group")))

robust_se_age_list <- list(
  robust_se_age_1624,
  robust_se_age_2534,
  robust_se_age_3544,
  robust_se_age_4554,
  robust_se_age_5569,
  robust_se_age_70
)



stargazer(FE_age_1624, FE_age_2534, FE_age_3544, FE_age_4554, FE_age_5569, FE_age_70,
          type = "text", 
          se = robust_se_age_list, 
          #dep.var.caption = "",
          covariate.labels = c("Average Stringency Index"),
          dep.var.labels = "Life Satisfaction",   
          column.labels = c("16-24", "25-34", "35-44", "45-54", "55-69", "70+"), # Column names
          keep = "avg_stringency",                  # Keep only avg_stringency coefficient
          keep.stat = c("n", "rsq"),
          model.numbers = FALSE,
          notes = "",
          notes.align = "r",
          notes.label = "",
          dep.var.caption = ""
          #out = "subgroup_age.tex"
)

## same logic for income quintiles, sort according to income quintile in 2018

# Create the variable "real_income_quintile_18" based on 2018 real_income, income quintile is thusly
# determined by pre pandemic income. This makes sure, that all quintiles have equal observations
pdata <- pdata %>%
  group_by(year) %>%
  mutate(real_income_quintile_18 = ifelse(year == 2018, ntile(real_income, 5), NA)) %>%
  ungroup()

# Forward fill "real_income_quintile_18" to keep 2018 classification for all years
pdata <- pdata %>%
  group_by(person_id) %>%
  mutate(real_income_quintile_18 = max(real_income_quintile_18, na.rm = TRUE)) %>%
  ungroup()

# Identify which income quintile each person belonged to in 2018
income_quintiles <- 1:5 

# Create empty lists to store results
valid_ids_income_list <- list()
pdata_income_list <- list()

# Loop through each income quintile and fix classification based on 2018
for (quintile in income_quintiles) {
  # Identify individuals in this income quintile in 2018
  valid_ids_income_list[[as.character(quintile)]] <- unique(pdata$person_id[pdata$real_income_quintile_18 == quintile & pdata$year == 2018])
  
  # Keep all their data across all years
  pdata_income_list[[as.character(quintile)]] <- pdata[pdata$person_id %in% valid_ids_income_list[[as.character(quintile)]], ]
}

# Assign individual variables for each income quintile
valid_ids_q1 <- valid_ids_income_list[["1"]]
pdata_q1 <- pdata_income_list[["1"]]

valid_ids_q2 <- valid_ids_income_list[["2"]]
pdata_q2 <- pdata_income_list[["2"]]

valid_ids_q3 <- valid_ids_income_list[["3"]]
pdata_q3 <- pdata_income_list[["3"]]

valid_ids_q4 <- valid_ids_income_list[["4"]]
pdata_q4 <- pdata_income_list[["4"]]

valid_ids_q5 <- valid_ids_income_list[["5"]]
pdata_q5 <- pdata_income_list[["5"]]

# Check results: Number of unique individuals in each 2018 income quintile
cat("Number of unique individuals in each income quintile (2018 classification):\n")
print(sapply(valid_ids_income_list, length))

# FE model for each income qt for heterogeneity analysis

FE_inc_1 <- plm(life_sat ~ avg_stringency + factor(first_nat) + factor(age_group) +
                  factor(sex) + factor(civstat) + no_kids + factor(work_stat) + 
                  edulvl + factor(year),
                data=pdata_q1, model = "within", index = c("person_id"), effect = "individual")

FE_inc_2 <- plm(life_sat ~ avg_stringency + factor(first_nat) + factor(age_group) +
                  factor(sex) + factor(civstat) + no_kids + factor(work_stat) + 
                  edulvl + factor(year),
                data=pdata_q2, model = "within", index = c("person_id"), effect = "individual")

FE_inc_3 <- plm(life_sat ~ avg_stringency + factor(first_nat) + factor(age_group) +
                  factor(sex) + factor(civstat) + no_kids + factor(work_stat) + 
                  edulvl + factor(year),
                data=pdata_q3, model = "within", index = c("person_id"), effect = "individual")

FE_inc_4 <- plm(life_sat ~ avg_stringency + factor(first_nat) + factor(age_group) +
                  factor(sex) + factor(civstat) + no_kids + factor(work_stat) + 
                  edulvl + factor(year),
                data=pdata_q4, model = "within", index = c("person_id"), effect = "individual")

FE_inc_5 <- plm(life_sat ~ avg_stringency + factor(first_nat) + factor(age_group) +
                  factor(sex) + factor(civstat) + no_kids + factor(work_stat) + 
                  edulvl + factor(year),
                data=pdata_q5, model = "within", index = c("person_id"), effect = "individual")

summary(FE_inc_1)
summary(FE_inc_2)
summary(FE_inc_3)
summary(FE_inc_4)
summary(FE_inc_5)

bptest(FE_inc_1)
bptest(FE_inc_2)
bptest(FE_inc_3)
bptest(FE_inc_4)
bptest(FE_inc_5)

# Function to determine the type that gives the largest robust SE

largest_se_FE_inc_1 <- get_largest_vcovHC(FE_inc_1)
largest_se_FE_inc_2 <- get_largest_vcovHC(FE_inc_2)
largest_se_FE_inc_3 <- get_largest_vcovHC(FE_inc_3)
largest_se_FE_inc_4 <- get_largest_vcovHC(FE_inc_4)
largest_se_FE_inc_5 <- get_largest_vcovHC(FE_inc_5)

# Print results
print(largest_se_FE_inc_1)
print(largest_se_FE_inc_2)
print(largest_se_FE_inc_3)
print(largest_se_FE_inc_4)
print(largest_se_FE_inc_5)


robust_se_inc_1 <- sqrt(diag(vcovHC(FE_inc_1, type = "HC3",cluster = "group")))
robust_se_inc_2 <- sqrt(diag(vcovHC(FE_inc_2, type = "HC3",cluster = "group")))
robust_se_inc_3 <- sqrt(diag(vcovHC(FE_inc_3, type = "HC3",cluster = "group")))
robust_se_inc_4 <- sqrt(diag(vcovHC(FE_inc_4, type = "HC4",cluster = "group")))
robust_se_inc_5 <- sqrt(diag(vcovHC(FE_inc_5, type = "HC4",cluster = "group")))

robust_se_inc_list <- list(
  robust_se_inc_1,
  robust_se_inc_2,
  robust_se_inc_3,
  robust_se_inc_4,
  robust_se_inc_5
)

stargazer(FE_inc_1, FE_inc_2, FE_inc_3, FE_inc_4, FE_inc_5,
          type = "text", 
          se = robust_se_inc_list, 
          #dep.var.caption = "",
          covariate.labels = c("Average Stringency Index"),
          dep.var.labels = "Life Satisfaction",   
          column.labels = c("lowest", "2nd", "3rd", "4th", "highest"), # Column names
          keep = "avg_stringency",                  # Keep only avg_stringency coefficient
          keep.stat = c("n", "rsq"),
          model.numbers = FALSE,
          notes = "",
          notes.align = "r",
          notes.label = "",
          dep.var.caption = ""
          #out = "subgroup_income.tex"
)


## Same for education levels. Again with pre pandemic educational levels, so that results remain consistent and individuals dont jump groups. 

# check how many individuals chaange their educational level in the pdata dataset. 
# Count how many distinct education levels each person has across all years
edu_changes <- pdata %>%
  group_by(person_id) %>%
  summarize(unique_edulvl = n_distinct(edulvl)) %>%
  ungroup()

# Count individuals with more than 1 unique education level
num_people_changed_edu <- sum(edu_changes$unique_edulvl > 1)

# Print result
cat("Number of individuals who changed their education level over time:", num_people_changed_edu, "\n")

# Define education levels
education_levels <- 0:6  

# Create empty lists to store results
valid_ids_edu_list <- list()
pdata_edu_list <- list()

# Loop through each education level and fix classification based on 2020
for (edu in education_levels) {
  # Step 1: Identify individuals who were in this education level in 2020
  valid_ids_edu_list[[as.character(edu)]] <- unique(pdata$person_id[pdata$edulvl == edu & pdata$year == 2020])
  
  # Step 2: Keep all their data across all years
  pdata_edu_list[[as.character(edu)]] <- pdata[pdata$person_id %in% valid_ids_edu_list[[as.character(edu)]], ]
}

# Assign individual variables for each education level
valid_ids_edu_0 <- valid_ids_edu_list[["0"]]
pdata_edu_0 <- pdata_edu_list[["0"]]

valid_ids_edu_1 <- valid_ids_edu_list[["1"]]
pdata_edu_1 <- pdata_edu_list[["1"]]

valid_ids_edu_2 <- valid_ids_edu_list[["2"]]
pdata_edu_2 <- pdata_edu_list[["2"]]

valid_ids_edu_3 <- valid_ids_edu_list[["3"]]
pdata_edu_3 <- pdata_edu_list[["3"]]

valid_ids_edu_4 <- valid_ids_edu_list[["4"]]
pdata_edu_4 <- pdata_edu_list[["4"]]

valid_ids_edu_5 <- valid_ids_edu_list[["5"]]
pdata_edu_5 <- pdata_edu_list[["5"]]

valid_ids_edu_6 <- valid_ids_edu_list[["6"]]
pdata_edu_6 <- pdata_edu_list[["6"]]

# Check results: Number of unique individuals in each education level (2020 classification)
cat("Number of unique individuals in each education level (2020 classification):\n")
print(sapply(valid_ids_edu_list, length))
# 
table(pdata$edulvl)

FE_edu_0 <- plm(life_sat ~ avg_stringency + factor(first_nat) + factor(age_group) +
                  factor(sex) + factor(civstat) + no_kids + factor(work_stat) + 
                  log(real_income) + factor(year),
                data=pdata_edu_0, model = "within", index = c("person_id"), effect = "individual")

FE_edu_1 <- plm(life_sat ~ avg_stringency + factor(first_nat) + factor(age_group) +
                  factor(sex) + factor(civstat) + no_kids + factor(work_stat) + 
                  log(real_income) + factor(year),
                data=pdata_edu_1, model = "within", index = c("person_id"), effect = "individual")

FE_edu_2 <- plm(life_sat ~ avg_stringency + factor(first_nat) + factor(age_group) +
                  factor(sex) + factor(civstat) + no_kids + factor(work_stat) + 
                  log(real_income) + factor(year),
                data=pdata_edu_2, model = "within", index = c("person_id"), effect = "individual")

FE_edu_3 <- plm(life_sat ~ avg_stringency + factor(first_nat) + factor(age_group) +
                  factor(sex) + factor(civstat) + no_kids + factor(work_stat) + 
                  log(real_income) + factor(year),
                data=pdata_edu_3, model = "within", index = c("person_id"), effect = "individual")

FE_edu_4 <- plm(life_sat ~ avg_stringency + factor(first_nat) + factor(age_group) +
                  factor(sex) + factor(civstat) + no_kids + factor(work_stat) + 
                  log(real_income) + factor(year),
                data=pdata_edu_4, model = "within", index = c("person_id"), effect = "individual")

FE_edu_5 <- plm(life_sat ~ avg_stringency + factor(first_nat) + factor(age_group) +
                  factor(sex) + factor(civstat) + no_kids + factor(work_stat) + 
                  log(real_income) + factor(year),
                data=pdata_edu_5, model = "within", index = c("person_id"), effect = "individual")

FE_edu_6 <- plm(life_sat ~ avg_stringency + factor(first_nat) + factor(age_group) +
                  factor(sex) + factor(civstat) + no_kids + factor(work_stat) + 
                  log(real_income) + factor(year),
                data=pdata_edu_6, model = "within", index = c("person_id"), effect = "individual")


# test for heteroskedasticity
bptest(FE_edu_0)
bptest(FE_edu_1)
bptest(FE_edu_2)
bptest(FE_edu_3)
bptest(FE_edu_4)
bptest(FE_edu_5)
bptest(FE_edu_6)

# Function to determine the type that gives the largest robust SE

largest_se_FE_edu_0 <- get_largest_vcovHC(FE_edu_0)
largest_se_FE_edu_1 <- get_largest_vcovHC(FE_edu_1)
largest_se_FE_edu_2 <- get_largest_vcovHC(FE_edu_2)
largest_se_FE_edu_3 <- get_largest_vcovHC(FE_edu_3)
largest_se_FE_edu_4 <- get_largest_vcovHC(FE_edu_4)
largest_se_FE_edu_5 <- get_largest_vcovHC(FE_edu_5)
largest_se_FE_edu_6 <- get_largest_vcovHC(FE_edu_6)


# Print results
print(largest_se_FE_edu_0)
print(largest_se_FE_edu_1)
print(largest_se_FE_edu_2)
print(largest_se_FE_edu_3)
print(largest_se_FE_edu_4)
print(largest_se_FE_edu_5)
print(largest_se_FE_edu_6)


robust_se_edu_0 <- sqrt(diag(vcovHC(FE_edu_0, type = "HC3",cluster = "group")))
robust_se_edu_1 <- sqrt(diag(vcovHC(FE_edu_1, type = "HC3",cluster = "group")))
robust_se_edu_2 <- sqrt(diag(vcovHC(FE_edu_2, type = "HC3",cluster = "group")))
robust_se_edu_3 <- sqrt(diag(vcovHC(FE_edu_3, type = "HC3",cluster = "group")))
robust_se_edu_4 <- sqrt(diag(vcovHC(FE_edu_4, type = "HC3",cluster = "group")))
robust_se_edu_5 <- sqrt(diag(vcovHC(FE_edu_5, type = "HC3",cluster = "group")))
robust_se_edu_6 <- sqrt(diag(vcovHC(FE_edu_6, type = "HC4",cluster = "group")))


robust_se_edu_list <- list(
  robust_se_edu_0,
  robust_se_edu_1,
  robust_se_edu_2,
  robust_se_edu_3,
  robust_se_edu_4,
  robust_se_edu_5,
  robust_se_edu_6
)

stargazer(FE_edu_0, FE_edu_1, FE_edu_2, FE_edu_3, FE_edu_4, FE_edu_5, FE_edu_6,
          type = "text", 
          se = robust_se_edu_list, 
          #dep.var.caption = "",
          covariate.labels = c("Average Stringency Index"),
          dep.var.labels = "Life Satisfaction",   
          column.labels = c("0", "1", "2", "3", "4", "5", "6"), # Column names
          keep = "avg_stringency",                  # Keep only avg_stringency coefficient
          keep.stat = c("n", "rsq"),
          model.numbers = FALSE,
          notes = "",
          notes.align = "r",
          notes.label = "",
          dep.var.caption = ""
          #out = "subgroup_edulvl.tex"
)

## heterogeneity analysis on the basis of sex. testing that sex does not change during the observation period. 
# Count distinct sex' individuals "levels"
sex_changes <- pdata %>%
  group_by(person_id) %>%
  summarize(unique_sex = n_distinct(sex)) %>%
  ungroup()

# Count individuals with more than 1 unique sex
num_people_changed_sex <- sum(sex_changes$unique_sex > 1)

# Print result
cat("Number of individuals who changed their sex over time:", num_people_changed_sex, "\n")

# fixed effects models for each sex

# male
FE_sex_0 <- plm(life_sat ~ avg_stringency + factor(first_nat) + factor(age_group) +
                  edulvl + factor(civstat) + no_kids + factor(work_stat) + 
                  log(real_income) + factor(year),
                data=pdata[which(pdata$sex == 0),], model = "within", index = c("person_id"), effect = "individual")

# female
FE_sex_1 <- plm(life_sat ~ avg_stringency + factor(first_nat) + factor(age_group) +
                  edulvl + factor(civstat) + no_kids + factor(work_stat) + 
                  log(real_income) + factor(year),
                data=pdata[which(pdata$sex == 1),], model = "within", index = c("person_id"), effect = "individual")

# test for heteroskedasticity
bptest(FE_sex_0)
bptest(FE_sex_1)


# Function to determine the type that gives the largest robust SE

largest_se_FE_sex_0 <- get_largest_vcovHC(FE_sex_0)
largest_se_FE_sex_1 <- get_largest_vcovHC(FE_sex_1)


# Print results
print(largest_se_FE_sex_0)
print(largest_se_FE_sex_1)

robust_se_sex_0 <- sqrt(diag(vcovHC(FE_sex_0, type = "HC4",cluster = "group")))
robust_se_sex_1 <- sqrt(diag(vcovHC(FE_sex_1, type = "HC3",cluster = "group")))


robust_se_sex_list <- list(
  robust_se_sex_0,
  robust_se_sex_1
)

stargazer(FE_sex_0, FE_sex_1,
          type = "text", 
          se = robust_se_sex_list, 
          #dep.var.caption = "",
          covariate.labels = c("Average Stringency Index"),
          dep.var.labels = "Life Satisfaction",   
          column.labels = c("Male", "Female"), # Column names
          keep = "avg_stringency",                  # Keep only avg_stringency coefficient
          keep.stat = c("n", "rsq"),
          model.numbers = FALSE,
          notes = "",
          notes.align = "r",
          notes.label = "",
          dep.var.caption = ""
          #out = "subgroup_sex.tex"
)


## heterogeneity analysis for first nationality

FE_nat_0 <- plm(life_sat ~ avg_stringency + factor(sex) + factor(age_group) +
                  edulvl + factor(civstat) + no_kids + factor(work_stat) + 
                  log(real_income) + factor(year),
                data=pdata[which(pdata$first_nat == 0),], model = "within", index = c("person_id"), effect = "individual")

FE_nat_1 <- plm(life_sat ~ avg_stringency + factor(sex) + factor(age_group) +
                  edulvl + factor(civstat) + no_kids + factor(work_stat) + 
                  log(real_income) + factor(year),
                data=pdata[which(pdata$first_nat == 1),], model = "within", index = c("person_id"), effect = "individual")

FE_nat_2 <- plm(life_sat ~ avg_stringency + factor(sex) + factor(age_group) +
                  edulvl + factor(civstat) + no_kids + 
                  log(real_income) + factor(year),
                data=pdata[which(pdata$first_nat == 2),], model = "within", index = c("person_id"), effect = "individual")

# factor work_stat is removed, since there are no unemployed individuals in the pdata dataset with first nationality = 2
table(pdata[which(pdata$first_nat == 2),]$work_stat)


# test for heteroskedasticity
bptest(FE_nat_0)
bptest(FE_nat_1)
bptest(FE_nat_2)


# Function to determine the type that gives the largest robust SE

largest_se_FE_nat_0 <- get_largest_vcovHC(FE_nat_0)
largest_se_FE_nat_1 <- get_largest_vcovHC(FE_nat_1)
largest_se_FE_nat_2 <- get_largest_vcovHC(FE_nat_2)



# Print results
print(largest_se_FE_nat_0)
print(largest_se_FE_nat_1)
print(largest_se_FE_nat_2)

robust_se_nat_0 <- sqrt(diag(vcovHC(FE_nat_0, type = "HC4",cluster = "group")))
robust_se_nat_1 <- sqrt(diag(vcovHC(FE_nat_1, type = "HC4",cluster = "group")))
robust_se_nat_2 <- sqrt(diag(vcovHC(FE_nat_2, type = "sss",cluster = "group")))


robust_se_nat_list <- list(
  robust_se_nat_0,
  robust_se_nat_1,
  robust_se_nat_2
)

stargazer(FE_nat_0, FE_nat_1, FE_nat_2,
          type = "text", 
          se = robust_se_nat_list, 
          #dep.var.caption = "",
          covariate.labels = c("Average Stringency Index"),
          dep.var.labels = "Life Satisfaction",   
          column.labels = c("Swiss", "Europe or NA", "Rest of the world"), # Column names
          keep = "avg_stringency",                  # Keep only avg_stringency coefficient
          keep.stat = c("n", "rsq"),
          model.numbers = FALSE,
          notes = "",
          notes.align = "r",
          notes.label = "",
          dep.var.caption = ""
          #out = "subgroup_firstnat.tex"
)

## heterogeneity analysis for civil status
# 

valid_ids_civstat_list <- list()
pdata_civstat_list <- list()

civstat_groups <- c(0, 1)  # 0 = Single, 1 = In Partnership

for (civ in civstat_groups) {
  valid_ids_civstat_list[[as.character(civ)]] <- unique(pdata$person_id[pdata$civstat == civ & pdata$year == 2020])
  pdata_civstat_list[[as.character(civ)]] <- pdata[pdata$person_id %in% valid_ids_civstat_list[[as.character(civ)]], ]
}

valid_ids_single <- valid_ids_civstat_list[["0"]]
pdata_single <- pdata_civstat_list[["0"]]

valid_ids_partnership <- valid_ids_civstat_list[["1"]]
pdata_partnership <- pdata_civstat_list[["1"]]

cat("\nCivstat (0=Single, 1=Partnership):\n")
print(sapply(valid_ids_civstat_list, length))

# build FE model for civil status

FE_civ_0 <- plm(life_sat ~ avg_stringency + factor(sex) + factor(age_group) +
                  edulvl + factor(first_nat) + no_kids + factor(work_stat) + 
                  log(real_income) + factor(year),
                data=pdata_single, model = "within", index = c("person_id"), effect = "individual")

FE_civ_1 <- plm(life_sat ~ avg_stringency + factor(sex) + factor(age_group) +
                  edulvl + factor(first_nat) + no_kids + factor(work_stat) + 
                  log(real_income) + factor(year),
                data=pdata_partnership, model = "within", index = c("person_id"), effect = "individual")

# test for heteroskedasticity
bptest(FE_civ_0)
bptest(FE_civ_1)


# Function to determine the type that gives the largest robust SE

largest_se_FE_civ_0 <- get_largest_vcovHC(FE_civ_0)
largest_se_FE_civ_1 <- get_largest_vcovHC(FE_civ_1)


# Print results
print(largest_se_FE_civ_0)
print(largest_se_FE_civ_1)

robust_se_civ_0 <- sqrt(diag(vcovHC(FE_civ_0, type = "HC3",cluster = "group")))
robust_se_civ_1 <- sqrt(diag(vcovHC(FE_civ_1, type = "HC3",cluster = "group")))

robust_se_civ_list <- list(
  robust_se_civ_0,
  robust_se_civ_1
)

stargazer(FE_civ_0, FE_civ_1,
          type = "text", 
          se = robust_se_civ_list, 
          #dep.var.caption = "",
          covariate.labels = c("Average Stringency Index"),
          dep.var.labels = "Life Satisfaction",   
          column.labels = c("Single", "In partnership"), # Column names
          keep = "avg_stringency",                  # Keep only avg_stringency coefficient
          keep.stat = c("n", "rsq"),
          model.numbers = FALSE,
          notes = "",
          notes.align = "r",
          notes.label = "",
          dep.var.caption = ""
          #out = "subgroup_civstat.tex"
)

## heterogeneity analysis for number of kids
# 

valid_ids_kids_list <- list()
pdata_kids_list <- list()

valid_ids_kids_list[["no_kids"]] <- unique(pdata$person_id[pdata$no_kids == 0 & pdata$year == 2020])
valid_ids_kids_list[["one_or_more_kids"]] <- unique(pdata$person_id[pdata$no_kids >= 1 & pdata$year == 2020])

pdata_kids_list[["no_kids"]] <- pdata[pdata$person_id %in% valid_ids_kids_list[["no_kids"]], ]
pdata_kids_list[["one_or_more_kids"]] <- pdata[pdata$person_id %in% valid_ids_kids_list[["one_or_more_kids"]], ]

valid_ids_no_kids <- valid_ids_kids_list[["no_kids"]]
pdata_no_kids <- pdata_kids_list[["no_kids"]]

valid_ids_one_or_more_kids <- valid_ids_kids_list[["one_or_more_kids"]]
pdata_one_or_more_kids <- pdata_kids_list[["one_or_more_kids"]]

cat("\nNo Kids vs. One or More Kids:\n")
print(sapply(valid_ids_kids_list, length))

# build FE model for having kids

FE_kids_0 <- plm(life_sat ~ avg_stringency + factor(sex) + factor(age_group) +
                   edulvl + factor(first_nat) + factor(civstat) + factor(work_stat) + 
                   log(real_income) + factor(year),
                 data=pdata_no_kids, model = "within", index = c("person_id"), effect = "individual")

FE_kids_1 <- plm(life_sat ~ avg_stringency + factor(sex) + factor(age_group) +
                   edulvl + factor(first_nat) + factor(civstat) + factor(work_stat) + 
                   log(real_income) + factor(year),
                 data=pdata_one_or_more_kids, model = "within", index = c("person_id"), effect = "individual")

# test for heteroskedasticity
bptest(FE_kids_0)
bptest(FE_kids_1)


# Function to determine the type that gives the largest robust SE

largest_se_FE_kids_0 <- get_largest_vcovHC(FE_kids_0)
largest_se_FE_kids_1 <- get_largest_vcovHC(FE_kids_1)


# Print results
print(largest_se_FE_kids_0)
print(largest_se_FE_kids_1)

robust_se_kids_0 <- sqrt(diag(vcovHC(FE_kids_0, type = "HC4",cluster = "group")))
robust_se_kids_1 <- sqrt(diag(vcovHC(FE_kids_1, type = "HC3",cluster = "group")))

robust_se_kids_list <- list(
  robust_se_kids_0,
  robust_se_kids_1
)

stargazer(FE_kids_0, FE_kids_1,
          type = "text", 
          se = robust_se_kids_list, 
          #dep.var.caption = "",
          covariate.labels = c("Average Stringency Index"),
          dep.var.labels = "Life Satisfaction",   
          column.labels = c("No kids", "One or more kids"), # Column names
          keep = "avg_stringency",                  # Keep only avg_stringency coefficient
          keep.stat = c("n", "rsq"),
          model.numbers = FALSE,
          notes = "",
          notes.align = "r",
          notes.label = "",
          dep.var.caption = ""
          #out = "subgroup_no_kids.tex"
)



## heterogeneity analysis for employment status
# 

valid_ids_work_list <- list()
pdata_work_list <- list()

work_groups <- c(0, 1)  # 0 = Employed, 1 = Unemployed

for (work in work_groups) {
  valid_ids_work_list[[as.character(work)]] <- unique(pdata$person_id[pdata$work_stat == work & pdata$year == 2020])
  pdata_work_list[[as.character(work)]] <- pdata[pdata$person_id %in% valid_ids_work_list[[as.character(work)]], ]
}

valid_ids_employed <- valid_ids_work_list[["0"]]
pdata_employed <- pdata_work_list[["0"]]

valid_ids_unemployed <- valid_ids_work_list[["1"]]
pdata_unemployed <- pdata_work_list[["1"]]

cat("\nWork Status (0=Employed, 1=Unemployed):\n")
print(sapply(valid_ids_work_list, length))

# build FE model for employment status

FE_workstat_0 <- plm(life_sat ~ avg_stringency + factor(sex) + factor(age_group) +
                       edulvl + factor(first_nat) + factor(civstat) + no_kids + 
                       log(real_income) + factor(year),
                     data=pdata_employed, model = "within", index = c("person_id"), effect = "individual")

FE_workstat_1 <- plm(life_sat ~ avg_stringency + factor(sex) + factor(age_group) +
                       edulvl + factor(first_nat) + factor(civstat) + no_kids + 
                       log(real_income) + factor(year),
                     data=pdata_unemployed, model = "within", index = c("person_id"), effect = "individual")

# test for heteroskedasticity
bptest(FE_workstat_0)
bptest(FE_workstat_1)


# Function to determine the type that gives the largest robust SE

largest_se_FE_workstat_0 <- get_largest_vcovHC(FE_workstat_0)
largest_se_FE_workstat_1 <- get_largest_vcovHC(FE_workstat_1)


# Print results
print(largest_se_FE_workstat_0)
print(largest_se_FE_workstat_1)

robust_se_workstat_0 <- sqrt(diag(vcovHC(FE_workstat_0, type = "HC3",cluster = "group")))
robust_se_workstat_1 <- sqrt(diag(vcovHC(FE_workstat_1, type = "HC4",cluster = "group")))

robust_se_workstat_list <- list(
  robust_se_workstat_0,
  robust_se_workstat_1
)

stargazer(FE_workstat_0, FE_workstat_1,
          type = "text", 
          se = robust_se_workstat_list, 
          #dep.var.caption = "",
          covariate.labels = c("Average Stringency Index"),
          dep.var.labels = "Life Satisfaction",   
          column.labels = c("Employed", "Unemployed"), # Column names
          keep = "avg_stringency",                  # Keep only avg_stringency coefficient
          keep.stat = c("n", "rsq"),
          model.numbers = FALSE,
          notes = "",
          notes.align = "r",
          notes.label = "",
          dep.var.caption = ""
          #out = "subgroup_workstat.tex"
)


## baseline fixed effects model comparison
#fe model with balanced panel

fe_model_life_sat_overall <- plm(life_sat ~ avg_stringency + factor(sex) + factor(age_group) + factor(work_stat) +
                                   edulvl +  factor(first_nat) + factor(civstat) + no_kids + 
                                   log(real_income) + factor(year),
                                 data=pdata, model = "within", index = c("person_id"), effect = "individual")

#pooled OLS with balanced panel

pooled_OLS_life_sat_overall <- plm(life_sat ~ avg_stringency + factor(sex) + factor(age_group) + factor(work_stat) +
                                     edulvl + factor(first_nat) + factor(civstat) + no_kids + 
                                     log(real_income) + factor(year),
                                   data=pdata, model = "pooling", index = c("person_id"))

# test for heteroskedasticity
bptest(fe_model_life_sat_overall)
bptest(pooled_OLS_life_sat_overall)


# Function to determine the type that gives the largest robust SE

largest_se_fe_model_life_sat_overall <- get_largest_vcovHC(fe_model_life_sat_overall)
largest_se_pooled_OLS_life_sat_overall <- get_largest_vcovHC(pooled_OLS_life_sat_overall)


# Print results
print(largest_se_fe_model_life_sat_overall)
print(largest_se_pooled_OLS_life_sat_overall)

robust_se_fe_model_life_sat_overall <- sqrt(diag(vcovHC(fe_model_life_sat_overall, type = "HC4",cluster = "group")))
robust_se_pooled_OLS_life_sat_overall <- sqrt(diag(vcovHC(pooled_OLS_life_sat_overall, type = "HC4",cluster = "group")))

robust_se_overall_list <- list(
  robust_se_pooled_OLS_life_sat_overall,
  robust_se_fe_model_life_sat_overall
)


stargazer(pooled_OLS_life_sat_overall, fe_model_life_sat_overall,
          type = "text", 
          se = robust_se_overall_list, 
          #dep.var.caption = "",
          #covariate.labels = c("Average Stringency Index", "Female", "16-24", "25-34", "35-44", "45-54", "55-69", "70 and above", "Unemployed", "Educational Level","Europe or NA", "Rest of the world", "In partnership", "Number of kids", "log(Real income)"),
          dep.var.labels = "Life Satisfaction",   
          covariate.labels = c("Average Stringency Index"),
          # add.lines = list(c("Year effects:", "Yes","Yes")),
          column.labels = c("Pooled OLS", "Fixed Effects"), # Column names
          keep = "avg_stringency",                  # Keep only avg_stringency coefficient
          keep.stat = c("n", "rsq"),
          #omit = "factor(year)",
          model.numbers = FALSE,
          notes = "",
          notes.align = "r",
          notes.label = "",
          dep.var.caption = ""
          #, out = "overall_comparison.tex"
)