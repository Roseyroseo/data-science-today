knitr::opts_chunk$set(echo = FALSE)
# install / load packages
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(stringr)
library(broom)
library(knitr)
library(purrr)

# Load Dataset
survey_data <- read.csv("survey_data.csv")

# Q1
# Filter relevant columns
filtered_data <- survey_data %>%
  select(Age = Q1, Gender = Q2, Role = Q5) %>%
  filter(!is.na(Age), !is.na(Gender), !is.na(Role))  # Remove rows with missing data

# Handle ambiguous or invalid Role data
# Identify invalid Role entries (e.g., blank or whitespace-only values)
invalid_roles <- filtered_data %>%
  filter(Role == "" | grepl("^\\s*$", Role))

# debugging
#print("Invalid Role Entries:")
#print(invalid_roles)

# Remove rows with invalid Role entries
cleaned_data <- filtered_data %>%
  filter(Role != "" & !grepl("^\\s*$", Role))  # Exclude blank or whitespace-only values

# Map Age to pre-defined groups for easier analysis
age_mapping <- c("18-21" = "18-21", "22-24" = "22-24", "25-29" = "25-29", 
                 "30-34" = "30-34", "35-39" = "35-39", "40-44" = "40-44", 
                 "45-49" = "45-49", "50-54" = "50-54", "55-59" = "55-59", 
                 "60-69" = "60-69", "70+" = "70+")

cleaned_data <- cleaned_data %>%
  mutate(Age_Group = recode(Age, !!!age_mapping, .default = NA_character_)) %>%
  filter(!is.na(Age_Group))  # Remove rows with unmatched Age values

# Summarize data for Gender and Age_Group distributions
role_gender_summary <- cleaned_data %>%
  group_by(Role, Gender) %>%
  summarise(Count = n(), .groups = "drop")

role_age_summary <- cleaned_data %>%
  group_by(Role, Age_Group) %>%
  summarise(Count = n(), .groups = "drop")

# Gender Distribution Summary
gender_summary_table <- role_gender_summary %>%
  group_by(Role) %>%
  mutate(Proportion = Count / sum(Count)) %>%  # Calculate proportion of each gender within the role
  arrange(Role, desc(Count))  # Arrange by role and descending count

# Age Distribution Summary
age_summary_table <- role_age_summary %>%
  group_by(Role) %>%
  mutate(Proportion = Count / sum(Count)) %>%  # Calculate proportion of each age group within the role
  arrange(Role, desc(Count))  # Arrange by role and descending count
# Gender with the Highest Proportion
gender_summary <- role_gender_summary %>%
  group_by(Role) %>%
  mutate(Proportion = Count / sum(Count)) %>%  # Calculate gender proportion
  slice_max(Proportion, n = 1) %>%  # Get the gender with the highest proportion
  select(Role, Gender, g_prop = Proportion)

# Age Group with the Highest Proportion
age_summary <- role_age_summary %>%
  group_by(Role) %>%
  mutate(Proportion = Count / sum(Count)) %>%  # Calculate age group proportion
  slice_max(Proportion, n = 1) %>%  # Get the age group with the highest proportion
  select(Role, Age = Age_Group, a_prop = Proportion)

# Combine Gender and Age summaries into a single table
role_summary_table <- gender_summary %>%
  left_join(age_summary, by = "Role")

# Display the summary table
role_summary_table %>%
  kable(
    caption = "Distribution of Age and Gender among Industry Professionals")

# Create visualizations
custom_colors <- c(
  "Man" = "lightsteelblue",
  "Woman" = "lightpink", 
  "Nonbinary" = "thistle",  
  "Prefer not to say" = "lemonchiffon2",  
  "Prefer to self-describe" = "darkseagreen1"  
)

# Gender Distribution Bar Plot
gender_plot <- ggplot(role_gender_summary, aes(x = Count, y = Role, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Gender Distribution Across Roles",
    x = "Count",
    y = "Role",
    fill = "Gender"
  ) +
  scale_fill_manual(values = custom_colors)  # Apply custom colors

# Convert Age to Numeric Midpoints
age_midpoints <- c("18-21" = 19.5, "22-24" = 23, "25-29" = 27, "30-34" = 32,
                   "35-39" = 37, "40-44" = 42, "45-49" = 47, "50-54" = 52,
                   "55-59" = 57, "60-69" = 65, "70+" = 75)

# Add a numeric column for age midpoints
cleaned_data <- cleaned_data %>%
  mutate(Age_Numeric = recode(Age_Group, !!!age_midpoints, .default = NA_real_))

# Create Box Plots for Age Distribution
age_boxplot <- ggplot(cleaned_data, aes(x = Age_Numeric, y = Role, fill = Role)) +
  geom_boxplot(outlier.color = "black", outlier.size = 1.5) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Age Distribution Across Roles",
    x = "Age",
    y = "Role"
  ) +
  theme(
    legend.position = "none",  # Remove legend as Role is already on the y-axis
    axis.text.y = element_text(size = 10)  # Adjust role label size for readability
  )

# Display the box plot
print(age_boxplot)

# Display Plots
print(gender_plot)

-----------------------------------

# Q2
  # Filter relevant data from the survey
  career_data <- survey_data %>%
  select(
    Role = Q5,                # Current or most recent role
    Region = Q3               # Region or country
  ) %>%
  filter(!is.na(Role), !is.na(Region))  # Remove rows with missing data

# Clean data by removing blank or whitespace-only entries
cleaned_data <- career_data %>%
  filter(
    Role != "" & !grepl("^\\s*$", Role),   # Remove blank/whitespace-only Role
    Region != "" & !grepl("^\\s*$", Region)  # Remove blank/whitespace-only Region
  )

# Count the number of responses per country and select the top 15
top_countries <- cleaned_data %>%
  group_by(Region) %>%  # Group by country/region
  summarise(Count = n(), .groups = "drop") %>%  # Count the number of responses
  arrange(desc(Count)) %>%  # Sort by descending order of responses
  slice_head(n = 20)  # Select the top 20 countries

# Extract the names of the top 20 countries
top_country_names <- top_countries$Region

# Filter data for the top 20 countries and exclude 'Other'
filtered_data <- cleaned_data %>%
  filter(Region %in% top_country_names & Region != "Other")  # Keep only top 15 countries and exclude 'Other'

# Summarize role distribution by top 20 countries
region_role_summary_filtered <- filtered_data %>%
  group_by(Region, Role) %>%
  summarise(Count = n(), .groups = "drop") %>%  # Count responses for each role per country
  group_by(Region) %>%
  mutate(Proportion = Count / sum(Count))  # Calculate proportion of each role within each country

# Find the most popular role by region
most_popular_role_by_region <- region_role_summary_filtered %>%
  group_by(Region) %>%  # Group by region
  slice_max(Proportion, n = 1) %>%  # Select the row with the highest proportion
  select(Region, Role, Proportion)  # Keep only relevant columns

# Display the table
most_popular_role_by_region %>%
  kable(
    caption = "Most Common Role among Respondents by Region")

# Shorten Region names for readability in visualization
region_role_summary_filtered <- region_role_summary_filtered %>%
  mutate(
    Region = str_trunc(Region, 15, side = "right", ellipsis = "..."),  # Shorten Region names to 15 characters
    Role = str_trunc(Role, 20, side = "right", ellipsis = "...")       # Shorten Role names to 20 characters
  )

# Create heatmap for top 20 countries
heatmap_plot_top_countries <- ggplot(region_role_summary_filtered, aes(x = Region, y = Role, fill = Proportion)) +
  geom_tile(color = "white") +  # Add gridlines between tiles
  scale_fill_gradient(low = "mistyrose", high = "lightpink2") +  # Gradient for proportions
  theme_minimal(base_size = 14) +  # Minimal theme for readability
  labs(
    title = "Role Distribution by Region (Top 20)",
    x = "Country",
    y = "Role",
    fill = "Proportion"
  ) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),  # Rotate x-axis labels for better readability
    legend.position = "right"  # Place legend to the right
  )

# Display the heatmap
print(heatmap_plot_top_countries)

----------------------------------

# Q3
  # Function to clean and reshape data for analysis
  reshape_tools_group <- function(data, group_label) {
    data %>%
      pivot_longer(
        cols = starts_with("Q"),  # Include all relevant columns (Q7, Q9, Q26_A)
        names_to = "Category", 
        values_to = "Response"
      ) %>%
      filter(!is.na(Response), Response != "None", Response != "") %>%  # Remove invalid responses
      group_by(Response) %>%
      summarise(Count = n(), .groups = "drop") %>%
      mutate(Proportion = Count / sum(Count)) %>%
      arrange(desc(Proportion)) %>%
      mutate(Group = group_label)
  }

# Filter relevant data from the survey
tools_languages_products_data <- survey_data %>%
  select(
    Role = Q5,                # Current or most recent role
    starts_with("Q7_"),       # Programming languages
    starts_with("Q9_"),       # Tools (e.g., IDEs)
    starts_with("Q26_A_")     # Cloud products
  ) %>%
  filter(!is.na(Role))  # Remove rows with missing roles

# Segment data for data scientists and all other respondents
data_scientists <- tools_languages_products_data %>%
  filter(Role == "Data Scientist")  # Filter for data scientists

all_other_respondents <- tools_languages_products_data %>%
  filter(Role != "Data Scientist")  # All other respondents

# Reshape and summarize data for both groups
tools_data_scientists <- reshape_tools_group(data_scientists, "Data Scientists")
tools_all_others <- reshape_tools_group(all_other_respondents, "All Other Respondents")

# Combine data for comparison
comparison_data <- bind_rows(tools_data_scientists, tools_all_others)

# Prepare contingency table for chi-square test
contingency_table <- comparison_data %>%
  group_by(Response, Group) %>%
  summarise(Count = sum(Count), .groups = "drop") %>%
  pivot_wider(
    names_from = Group,
    values_from = Count,
    values_fill = 0  # Fill missing values with 0
  ) %>%
  as.data.frame()  # Convert to a data frame to avoid tibble row names warning

# Convert the 'Response' column to row names
rownames(contingency_table) <- contingency_table$Response  # Assign 'Response' as row names
contingency_table <- contingency_table[, -1]  # Remove the 'Response' column from the data frame

# Perform chi-square test with Monte Carlo simulation
chi_square_result_sim <- chisq.test(contingency_table, simulate.p.value = TRUE, B = 10000)

# Create bar plots for visualization
# Top tools for data scientists
tools_top20_data_scientists <- tools_data_scientists %>%
  slice_max(Proportion, n = 20)

top_employed_plot <- ggplot(tools_top20_data_scientists, aes(x = reorder(Response, Proportion), y = Proportion, fill = Response)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Top 20 Trade Tools among Data Scientists",
    x = "Tool/Language/Product",
    y = "Proportion"
  ) +
  theme_minimal()

# Comparison plot between data scientists and all other respondents
top_tools_languages_summary <- comparison_data %>%
  group_by(Group) %>%
  slice_max(Proportion, n = 20) %>%
  ungroup()

comparison_plot <- ggplot(top_tools_languages_summary, aes(x = reorder(Response, Proportion), y = Proportion, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(
    title = "Top 20 Trade Tools Comparison: Data Scientists vs All Other Respondents",
    x = "Tool/Language/Product",
    y = "Proportion",
    fill = "Group"
  ) +
  scale_fill_manual(
    values = c(
      "Data Scientists" = "lightblue2", 
      "All Other Respondents" = "pink" 
    )
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",  # Move legend to the bottom
    legend.title = element_text(size = 12),  # Customize legend title size
    legend.text = element_text(size = 10),   # Customize legend text size
    plot.title = element_text(hjust = 0.5)   # Center the plot title
  )

# Display results
print("Chi-Square Test Results with Simulated P-Values:")
print(chi_square_result_sim)

if (chi_square_result_sim$p.value < 0.05) {
  print("Reject the null: Data scientists use different tools compared to all other respondents.")
} else {
  print("Fail to reject the null: Data scientists use similar tools as all other respondents.")
}

print(top_employed_plot)
print(comparison_plot)

------------------------------------------

#Q4
  
  # Select Relevant Columns
  education_salary_data <- survey_data %>%
  select(Education = Q4, Compensation = Q24) %>%
  filter(!is.na(Education), !is.na(Compensation))  # Remove rows with missing data

# Clean UTF-8 Encoding
education_salary_data <- education_salary_data %>%
  mutate(
    Compensation = iconv(Compensation, from = "UTF-8", to = "ASCII", sub = "?")  # Replace invalid UTF-8 with '?'
  )

# Identify Rows with Invalid Characters
invalid_rows <- education_salary_data %>%
  filter(str_detect(Compensation, "\\?"))

# Clean the Compensation Column
cleaned_data <- education_salary_data %>%
  mutate(
    Compensation = str_replace_all(Compensation, "[\\$,]", ""),  # Remove dollar signs, commas
    Compensation = str_trim(Compensation),  # Remove extra spaces
    Compensation = str_replace_all(Compensation, "[–—]", "-"),  # Replace non-standard dashes
    Compensation = case_when(
      # Handle ranges by calculating the midpoint
      str_detect(Compensation, "-") ~ map_dbl(
        str_split(Compensation, "-"),
        ~ ifelse(all(!is.na(as.numeric(.))), mean(as.numeric(.), na.rm = TRUE), NA_real_)
      ),
      TRUE ~ as.numeric(Compensation)  # Convert single numeric values
    )
  ) %>%
  # Remove rows with invalid Compensation values
  filter(!is.na(Compensation))

# Simplify Education Levels
cleaned_data <- cleaned_data %>%
  mutate(
    Education = str_replace_all(Education, "’", "'"),  # Replace curly apostrophe with straight apostrophe
    Education = case_when(
      grepl("No formal education", Education) ~ "High School",
      grepl("Some college", Education) ~ "Some College",
      grepl("Bachelor's degree", Education) ~ "Bachelor's",
      grepl("Master's degree", Education) ~ "Master's",
      grepl("Doctoral degree", Education) ~ "Doctorate",
      grepl("Professional degree", Education) ~ "Professional",
      TRUE ~ "Other"
    )
  )

# Summarize Data
education_summary <- cleaned_data %>%
  group_by(Education) %>%
  summarise(
    Mean_Salary = mean(Compensation, na.rm = TRUE),
    Median_Salary = median(Compensation, na.rm = TRUE),
    SD_Salary = sd(Compensation, na.rm = TRUE),
    Count = n()
  ) %>%
  arrange(desc(Mean_Salary))

# Visualizations
# Box Plot
ggplot(cleaned_data, aes(x = Education, y = Compensation, fill = Education)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 200000)) +  # Set y-axis limits
  theme_minimal() +
  labs(
    title = "Salary Distribution by Education Level",
    x = "Education Level",
    y = "Compensation (USD)"
  )+
  theme(
    legend.position = "none",  # Remove legend for cleaner presentation
    axis.text.x = element_text(angle = 30, hjust = 1)  # Rotate x-axis labels
  )

# Bar Plot
ggplot(education_summary, aes(x = reorder(Education, Mean_Salary), y = Mean_Salary, fill = Education)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Average Salary by Education Level",
    x = "Education Level",
    y = "Mean Salary (USD)"
  )

# ANOVA Test
anova_result <- aov(Compensation ~ Education, data = cleaned_data)
summary(anova_result)

# Linear Regression
lm_model <- lm(Compensation ~ Education, data = cleaned_data)
#summary(lm_model)

# Evaluate Regression Model
predicted <- predict(lm_model, cleaned_data)
mae <- mean(abs(predicted - cleaned_data$Compensation))

print(paste("Mean Absolute Error (MAE):", mae))

# Summarize the regression model using the broom package
lm_summary <- tidy(lm_model)

# Display the table in a clean format
lm_summary %>%
  kable(
    caption = "Model Results: Relationship Between Education and Compensation",
    col.names = c("Term", "Estimate", "Std. Error", "t-value", "p-value")
  )

----------------------------------------------------

# AA

# Load the world population data 
world_population <- read_csv("world_population.csv", show_col_types = FALSE)

# Process Survey Data
# Select relevant columns and filter out null or blank countries
survey_clean <- survey_data %>%
  select(
    Age = Q1,
    Gender = Q2,
    Country = Q3
  ) %>%
  filter(!is.na(Age), !is.na(Gender), !is.na(Country), Country != "", Country != "In which country do you currently reside?")  # Remove invalid rows

# Standardize country names in the survey data
survey_clean <- survey_clean %>%
  mutate(
    Country = case_when(
      Country == "United Kingdom of Great Britain and Northern Ireland" ~ "United Kingdom",
      Country == "United States of America" ~ "United States",
      Country == "Republic of Korea" ~ "South Korea",
      Country == "Viet Nam" ~ "Vietnam",
      Country == "Iran, Islamic Republic of..." ~ "Iran",
      TRUE ~ Country  # Keep other names as is
    )
  )

# Clean Gender
survey_clean <- survey_clean %>%
  mutate(
    Gender = case_when(
      Gender == "Man" ~ "Male",
      Gender == "Woman" ~ "Female",
      TRUE ~ "Other"
    )
  )

# Summarize the survey data by country
survey_summary <- survey_clean %>%
  group_by(Country) %>%
  summarise(
    Respondents = n(),
    Gender_Distribution = list(table(Gender))
  )

# Process World Population Data
# Select and clean relevant columns
world_clean <- world_population %>%
  select(Country = `Country/Territory`, Population_2020 = `2020 Population`, World_Percentage = `World Population Percentage`)

# Merge survey and world population data
merged_data <- survey_summary %>%
  left_join(world_clean, by = "Country") %>%
  mutate(
    Survey_Percentage = Respondents / sum(Respondents) * 100
  ) %>%
  filter(!is.na(World_Percentage))  # Drop rows with no match in world data

# Gender Representation
# Calculate total number of survey respondents
total_respondents <- nrow(survey_clean)

# Calculate survey gender proportions based on total respondents
gender_summary <- survey_clean %>%
  group_by(Gender) %>%
  summarise(
    Count = n(),
    Proportion = Count / total_respondents * 100  # Proportion relative to total respondents
  ) %>%
  mutate(Source = "Survey")  # Add a column for the source of data

# Add global gender proportions
global_gender <- data.frame(
  Gender = c("Male", "Female", "Other"),
  Count = c(NA, NA, NA),  # Placeholder for counts
  Proportion = c(48, 48, 4),  # Assumed global proportions
  Source = "Global"  # Add a source column
)

# Combine survey and global gender data
gender_comparison <- bind_rows(gender_summary, global_gender)

# Create gender comparison bar plot
ggplot(gender_comparison, aes(x = Gender, y = Proportion, fill = Source)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(
    title = "Gender Representation: Survey vs. Global",
    x = "Gender",
    y = "Proportion (%)",
    fill = "Source"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(scale = 1) # Adjust y-axis to percentage format
  ) + 
  scale_fill_manual(
    values = c(
      "Survey" = "lavender",   # Custom color for Survey
      "Global" = "lightblue1"      # Custom color for Global
    )
  )

# Align Survey and Global data for comparison
gender_disparity <- gender_comparison %>%
  group_by(Gender) %>%
  summarise(
    Survey = sum(Proportion[Source == "Survey"], na.rm = TRUE),
    Global = sum(Proportion[Source == "Global"], na.rm = TRUE)
  ) %>%
  mutate(
    Absolute_Difference = abs(Survey - Global),
    Percentage_Difference = (Survey - Global) / Global * 100
  )

# Display the gender disparity table
gender_disparity_table <- gender_disparity %>%
  select(Gender, Survey, Global, Absolute_Difference, Percentage_Difference)

# Print the table for reference
gender_disparity_table %>%
  kable(
    caption = "Numerical Analysis of Gender Representation Disparities",
    col.names = c("Gender", "Survey Proportion (%)", "Global Proportion (%)", 
                  "Absolute Difference (%)", "Percentage Difference (%)"),
    digits = 2
  )

# Highlight key differences in the analysis
#print(gender_disparity_table)

# Visualization: Regional Representation
ggplot(merged_data, aes(x = reorder(Country, -Survey_Percentage), y = Survey_Percentage, fill = World_Percentage)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Regional Representation of Survey Respondents",
    x = "Country",
    y = "Survey Respondent Percentage",
    fill = "World Population %"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),  # Ensure axis labels remain horizontal
    legend.position = "bottom",  # Move legend to the bottom
    legend.title = element_text(size = 12),  # Adjust legend title size
    legend.text = element_text(size = 10),   # Adjust legend text size
    plot.title = element_text(hjust = 0.5)   # Center align the plot title
  ) +
  guides(fill = guide_legend(nrow = 1)  # Arrange legend in a single row
  ) +
  scale_fill_gradient(low = "lavenderblush2", high = "lightpink2")  # Custom gradient colors for the legend
