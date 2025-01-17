#load libraries
library(viridis)
library(gridExtra)
library(tidyverse)
library(broom)
library(purrr)

#filepaths
datapath<-"~/Documents/Hawaii/DataAnalysis/Hawaiian Language Preservation_July 17, 2023_11.31.csv"
datapath2<-"~/Documents/Hawaii/DataAnalysis/Hawaiian Language Preservation.V2_January 5, 2024_09.45.csv"
questionpath<-("~/Documents/Hawaii/hawaii_questions.csv")
#source functions
source("~/Documents/Hawaii/DataAnalysis/Davinetal_functions.R")

#everything below here can be run without change if you set the paths correctly above

# Read the list of questions from the specified file path
questions <- readLines(questionpath)

# Load the first dataset, skipping the first 69 rows (assumed to be metadata or headers)
data <- read.csv(datapath, skip = 69)

# Load the second dataset, skipping the first 87 rows (assumed to be metadata or headers)
data2 <- read.csv(datapath2, skip = 87)

# Filter the first dataset to include only rows where the duration exceeds 300 seconds
updateddata <- data %>% filter("X..ImportId...duration.." > 300)

# Keep only the first 177 rows of the filtered data
updateddata <- updateddata[1:177, ]

# Select specific columns (QID109 to QID117) from the filtered first dataset
data.firstwave <- updateddata %>% dplyr::select("X..ImportId...QID109.." : "X..ImportId...QID117..")

# Filter the second dataset to include only rows where the duration exceeds 300 seconds
updateddatap2 <- data2 %>% filter("Duration..in.seconds." > 300)

# Keep only the first 177 rows of the filtered second dataset
updateddatap2 <- updateddatap2[1:177, ]

# Select specific columns (QID109 to QID117) from the filtered second dataset
data.secondwave <- updateddatap2 %>% dplyr::select("X..ImportId...QID109.." : "X..ImportId...QID117..")

# Combine the selected data from the first and second waves into a single dataset
combinedData <- rbind(data.firstwave, data.secondwave)

############################
############################
#Ancestry
############################
############################

# Create a frequency table of ancestry data by splitting the 13th column of the combined data on commas
ancestry<-as.data.frame(table(unlist((str_split(combinedData[,13],",")))))[-1,]

# Ensure the ancestry labels are treated as character data
ancestry$Var1 <- as.character(ancestry$Var1)

# Clean up the ancestry labels by removing tab characters
ancestry$Var1 <- gsub("\t", "", ancestry$Var1)

# Clean up the ancestry labels by removing spaces
ancestry$Var1 <- gsub(" ", "", ancestry$Var1)

# Sort the ancestry data by frequency in descending order
ancdata <- ancestry[order(ancestry$Freq, decreasing = TRUE), ]

# Filter out rows where the ancestry label is "Other"
ancdata_filtered <- ancdata[ancdata$Var1 != "Other", ]

# Create a stick plot to visualize the filtered ancestry data
stickPlot(ancdata_filtered)

############################
############################
#Roots in Hawaii
############################
############################

# Create a frequency table for the 15th column of the combined dataset
# Remove the first row of the table (invalid value)
describeMore <- as.data.frame(table(combinedData[, 15]))[-1, ]

# Convert the labels (Var1 column) to character type for processing
describeMore$Var1 <- as.character(describeMore$Var1)

# Wrap the text in the labels to a maximum width of 50 characters for better readability in plots
describeMore$Var1 <- str_wrap(describeMore$Var1, width = 50)

# Sort the data by frequency in descending order
describeMore <- describeMore[order(describeMore$Freq, decreasing = TRUE), ]

# Generate a stick plot for the data with the title "Roots"
stickPlot(describeMore, title = "Roots")

############################
############################
### Proficiency by Generation for Respondents with and without Hawaiian Ancestry
############################
############################

# Extract columns 44 to 47, which contain proficiency data
profVariables <- combinedData[, c(44:47)]

# Convert proficiency data into numeric format by parsing numbers from the text
profNumeric_values <- sapply(profVariables, readr::parse_number)

# Track the corresponding questions for the extracted columns
prompts <- questions[c(44:47)]

# Convert the numeric proficiency data into a data frame for further processing
profnumericData <- as.data.frame(profNumeric_values)

# Add an age column from the 11th column of the combined dataset
# Remove any non-numeric characters from the age column and convert it to numeric
profnumericData$age <- combinedData[, 11]
profnumericData$age <- as.numeric(gsub("\\D", "", profnumericData$age))

# Categorize respondents into generational cohorts based on age
profnumericData <- profnumericData %>%
  mutate(generation = cut(age, 
                          breaks = c(18, 27, 43, 59, Inf), 
                          labels = c("GenZ", "Millennial", "GenX", "Boomers")))

### Generate Proficiency Donut Chartsdom

# Create a PDF to save the proficiency donut charts for all respondents
pdf("AllRespondentsProficiencyDonutChartsVerification.pdf", width = 8, height = 6)

# Loop through all four proficiency questions to generate charts
for (i in 1:4) {
  # Generate a chart for the current proficiency question
  # The function `allGens` takes the data and column index to create a donut chart
  allGens(profnumericData, columnIndex = i)
}

# Close the PDF device
dev.off()

# Save the prompts corresponding to the proficiency questions to a CSV file
write.csv(prompts, file = "ProficiencyDonutChartPromptsVerification")

#get sample sizes of each
table(profnumericData$generation)

############################
############################
#Who do you speak with?
############################
############################

# Define the column indices for the questions related to language use
questionNumbers <- c(78, 80, 82)

# Process the first language column (Hawaiian) by splitting responses on commas
# Create a frequency table and remove the first row (assumed placeholder or invalid value)
Hawaii <- data.frame(table(unlist(strsplit(combinedData[, questionNumbers[1]], ","))))[-1, ]

# Process the second language column (English) by splitting responses on commas
English <- data.frame(table(unlist(strsplit(combinedData[, questionNumbers[2]], ","))))

# Process the third language column (Pidgin) by splitting responses on commas
Pidgin <- data.frame(table(unlist(strsplit(combinedData[, questionNumbers[3]], ","))))

# Add a new column to each language dataset to label the language being processed
Hawaii$Language <- rep("Hawaii", length(Hawaii[, 1]))
English$Language <- rep("English", length(English[, 1]))
Pidgin$Language <- rep("Pidgin", length(Pidgin[, 1]))

# Combine the Hawaiian and English datasets into a single data frame
df1 <- rbind(Hawaii, English)

# Combine the previously combined data with the Pidgin dataset
combinedSpeaking <- rbind(df1, Pidgin)

# Correct a specific value in the `Var1` column where extra spaces exist
fix <- which(combinedSpeaking$Var1 == " Classmate(s)")
combinedSpeaking$Var1[fix] <- "Classmate(s)"

# Reorder the `Var1` factor levels to match a predefined order for visualization
combinedSpeaking <- combinedSpeaking %>%
  mutate(Var1 = factor(Var1, levels = rev(c(
    "My grandfather", "My grandmother", "My dad", "My mom", 
    "Sibling(s)", "Friend(s)", "Community member(s)", "Co-worker(s)", 
    "Teacher(s)", "Classmate(s)", "Other", "None of the above"
  ))))

# Create the horizontal dot plot with updated colors and order
gg_dot <- combinedSpeaking %>%
  arrange(Freq) %>%
  ggplot() +
  # Remove axes and superfluous grids
  theme_classic() +
  theme(axis.title = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line = element_blank()) +
  # Add a dummy point for scaling purposes
  geom_point(aes(x = max(Freq) * 1.1, y = Var1), size = 0, col = "white") +
  # Add the horizontal lines for each group
  geom_hline(yintercept = seq_along(unique(combinedSpeaking$Var1)), col = "grey80") +
  # Add a point for each frequency, colored by language
  geom_point(aes(x = Freq, y = Var1, color = Language), size = 6) +
  # Custom color palette: Hawaiian (bright), Pidgin (intermediate), English (dark cool)
  scale_color_manual(values = c("Hawaii" = "#FFDD57",  # Bright yellow for Hawaiian
                                "Pidgin" = "#FFA500",  # Orange for Pidgin
                                "English" = "#4682B4")) +  # Steel blue for English
  # Add labels for the points (optional)
  geom_text(aes(x = Freq, y = Var1, label = Freq), vjust = 0.3, hjust = -0.3, size = 3) +  
  # Customize the plot with axis labels and title
  labs(title = "Who Respondents Speak Hawaiian, English, or Pidgin With",
       x = "Frequency of Language Use",
       y = "") +  
  # Additional formatting for a clean look
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))
# Display the plot
gg_dot

###################
###################
#####.  R2
###################
###################
###################

############################
############################
#Let's consider if there are changes in percieved importance
############################
############################

# Define the range of question indices related to language importance
questionNumbers <- 16:25

# Convert the age-related column to numeric for filtering by generation
combinedData$X..ImportId...QID59_TEXT.. <- as.numeric(combinedData$X..ImportId...QID59_TEXT..)

# Filter data for Generation Z (age ≤ 27)
genZ <- filter(combinedData, X..ImportId...QID59_TEXT.. <= 27)

# Filter data for Millennials (age > 27 and ≤ 43)
mill <- filter(combinedData, X..ImportId...QID59_TEXT.. <= 43)
mill <- filter(mill, X..ImportId...QID59_TEXT.. >= 27.1)

# Filter data for Generation X (age > 43 and ≤ 59)
genX <- filter(combinedData, X..ImportId...QID59_TEXT.. <= 59)
genX <- filter(genX, X..ImportId...QID59_TEXT.. >= 43.1)

# Filter data for Baby Boomers (age ≥ 59)
boom <- filter(combinedData, X..ImportId...QID59_TEXT.. >= 59)

# The function `languageImportance` is applied to each subset of the data (see functions)
genZLI <- languageImportance(genZ, questionNumbers)
millLI <- languageImportance(mill, questionNumbers)
genXLI <- languageImportance(genX, questionNumbers)
boomLI <- languageImportance(boom, questionNumbers)

# Combine the language importance results for all generations into a single dataset
combinedLI <- rbind(genZLI, millLI, genXLI, boomLI)

# Add a new column to label the generational group for each row
# Each group contributes 10 rows to the combined dataset
combinedLI$generation <- c(rep("genZ", 10), 
                           rep("millenial", 10), 
                           rep("genX", 10), 
                           rep("boomer", 10))

### Plotting the Results
# Generate a dot plot to visualize language importance across generations
ggDot(combinedLI)

############################
############################
###Assemble a big dataset of everything by generation
############################
############################

#process data
genZBoot<-importantReplacer(genZ, questionNumbers=16:25)
millBoot<-importantReplacer(mill, questionNumbers=16:25)
genXBoot<-importantReplacer(genX, questionNumbers=16:25)
boomBoot<-importantReplacer(boom, questionNumbers=16:25)

# Add a `generation` column to each dataset to label the generational group
genZBoot$generation<-rep("genZ",length(genZBoot[,1]))
millBoot$generation<-rep("mill",length(millBoot[,1]))
genXBoot$generation<-rep("genX",length(genXBoot[,1]))
boomBoot$generation<-rep("boom",length(boomBoot[,1]))
# Combine all generational datasets into a single data frame
combinedImportanceData <- rbind(boomBoot, genXBoot, millBoot, genZBoot)

# Extract prompts corresponding to questions 16 to 25
prompts <- questions[16:25]

# Extract and clean prompt strings by removing the prefix text
promptstrings <- unlist(lapply(prompts, function(x) strsplit(x, "On a scale of 1 to 3, how important is it ")))[seq(from = 2, to = 20, by = 2)]

# Rename the columns of the combined dataset using the cleaned prompt strings and "generation"
names(combinedImportanceData) <- c(promptstrings, "generation")

# Reshape the combined data into long format for analysis
longFormCombinedImportanceData <- combinedImportanceData %>%
  pivot_longer(cols = promptstrings, names_to = "Prompt", values_to = "Importance")

# Filter out rows with Importance values less than 1
longFormCombinedImportanceData <- longFormCombinedImportanceData %>%
  filter(Importance >= 1)

# Convert the Importance column to numeric for statistical analysis
longFormCombinedImportanceData <- longFormCombinedImportanceData %>%
  mutate(Importance = as.numeric(Importance))

### Statistical Analysis

# Group the data by Prompt and nest it for separate analyses
nested_data <- longFormCombinedImportanceData %>%
  group_by(Prompt) %>%
  nest()

# Apply ANOVA and pairwise t-tests to each nested group
results <- nested_data %>%
  mutate(test_results = map(data, run_anova_and_ttests))

# Extract and view ANOVA results
anova_results <- results %>%
  mutate(anova = map(test_results, "anova")) %>%
  select(Prompt, anova) %>%
  unnest(anova)

# Extract and view pairwise t-test results for prompts with significant ANOVA results
pairwise_results <- results %>%
  mutate(pairwise = map(test_results, "pairwise")) %>%
  select(Prompt, pairwise) %>%
  unnest(pairwise)

# Print the ANOVA and pairwise t-test results
print(anova_results)
print(pairwise_results)

### Motivation Donut Charts

# Prepare the data for motivation analysis
motStats <- motivationDonutPrep(combinedData)

#lets loop over all the motivation questions
pdf("MotivationDonutVerification.pdf", width = 8, height = 6)
for (i in 1:13){
  allGens(motStats, columnIndex=i)
}
 dev.off()

###################
###################
#####.  R3
###################
###################

###################
###################
#Let's consider if there are changes in percieved importance to revitalization
###################
###################

#process data
genZ <- revitalizationProcessing(combinedData, questionNumbers=86:100, agecutmin=0, ageCutmax=27)
mill <- revitalizationProcessing(combinedData, questionNumbers=86:100, agecutmin=27.1, ageCutmax=43)
genX <- revitalizationProcessing(combinedData, questionNumbers=86:100, agecutmin=43.1, ageCutmax=59)
boom <- revitalizationProcessing(combinedData, questionNumbers=86:100, agecutmin=59.1, ageCutmax=100)
combinedLR<-rbind(genZ, mill, genX, boom)
combinedLR$generation<-c(rep("genZ",15),rep("millenial",15),rep("genX",15),rep("boomer",15))

#plot it
ggDot(combinedLR)


#test if there is a difference in perception between immersion and non-immersion programming
combinedData[, 86:93] <- combinedData[, 86:93] %>%
  mutate(across(everything(), ~ case_when(
    . == "Not well" ~ 1,
    . == "Well" ~ 2,
    . == "Very well" ~ 3,
    . == "I don't know." ~ NA_real_,
    . == "" ~ NA_real_
  )))

# Gather data from immersion columns (86:89) and label it as 'immersion'
immersion_data <- combinedData %>%
  pivot_longer(cols = 86:89, names_to = "response", values_to = "score") %>%
  mutate(program_type = "immersion") %>%
  select(program_type, score)

# Gather data from non-immersion columns (90:93) and label it as 'non_immersion'
non_immersion_data <- combinedData %>%
  pivot_longer(cols = 90:93, names_to = "response", values_to = "score") %>%
  mutate(program_type = "non_immersion") %>%
  select(program_type, score)

# Combine both immersion and non-immersion data into one dataset
longData <- bind_rows(immersion_data, non_immersion_data)

# Remove rows with NA values in 'score'
longData <- longData %>%
  filter(!is.na(score))

# Perform the Mann-Whitney U test
wilcox_test_result <- wilcox.test(score ~ program_type, data = longData)

# Print the result
print(wilcox_test_result)

########Adding an ANOVA and possible pair-wise ttest here
combinedData$X..ImportId...QID59_TEXT.. <- as.numeric(combinedData$X..ImportId...QID59_TEXT..)
# Categorize generations, I should have done this at the beginning! For anyone reading this, please have a hearty belly chuckle at me not thinking this through and adding dozens of lines of code throughout
combinedData <- combinedData %>%
  mutate(generation = case_when(
    X..ImportId...QID59_TEXT.. <= 27 ~ "GenZ",
    X..ImportId...QID59_TEXT.. > 27 & X..ImportId...QID59_TEXT.. <= 43 ~ "Millennial",
    X..ImportId...QID59_TEXT.. > 43 & X..ImportId...QID59_TEXT.. <= 59 ~ "GenX",
    X..ImportId...QID59_TEXT.. > 59 ~ "Boomer"
  ))
# List of columns for which we want to perform the ANOVA
columns_to_test <- c(86:93)  
# Function to run ANOVA and pairwise t-tests if ANOVA is significant
run_anova_and_pairwise <- function(df, column_name) {
  # Prepare the formula for ANOVA
  formula <- as.formula(paste(column_name, "~ generation"))  
  # Perform ANOVA
  anova_result <- aov(formula, data = df)  
  # Check if the ANOVA is significant
  anova_summary <- summary(anova_result)
  p_value <- anova_summary[[1]][["Pr(>F)"]][1]  
  # If significant, run pairwise t-tests
  if (p_value < 0.05) {
    pairwise_tests <- pairwise.t.test(
      df[[column_name]], 
      df$generation, 
      p.adjust.method = "BH"  # Benjamini-Hochberg correction for multiple comparisons
    )
    return(list(anova = anova_summary, pairwise = pairwise_tests))
  } else {
    return(list(anova = anova_summary, pairwise = NULL))
  }
}
# Loop over the selected columns and run ANOVA and pairwise t-tests
results <- lapply(names(combinedData)[columns_to_test], function(column_name) {
  run_anova_and_pairwise(combinedData, column_name)
})
# View results
for (i in 1:length(columns_to_test)) {
  cat("Results for", names(combinedData)[columns_to_test[i]], ":\n")
  print(results[[i]]$anova)
  if (!is.null(results[[i]]$pairwise)) {
    print(results[[i]]$pairwise)
  } else {
    cat("No significant differences between generations for", names(combinedData)[columns_to_test[i]], "\n\n")
  }
}


###################
###################
#Let's look at schooling across generations
###################
###################

#We're going to subsample just the columns we need and make a few new ones
educationData<-data.frame(combinedData[,32:41],generation=profnumericData$generation, immersion= combinedData[,30])
educationData$immersion[which(educationData$immersion=="")]<-NA
#convert the good fair poor columns numbers
educationData[, 2:10] <- educationData[, 2:10] %>%
  mutate(across(everything(), ~ case_when(
    . == "Poor" ~ 1,
    . == "Fair" ~ 2,
    . == "Good" ~ 3,
    . == "" ~ NA_real_
  )))
#convert the schooling contribution column to numeric
educationData[, 1] <- case_when(
  educationData[, 1] == "1 – My schooling did not contribute to my ability to use Hawaiian." ~ 1,
  educationData[, 1] == "2 – My schooling contributed to my ability to use Hawaiian." ~ 2,
  educationData[, 1] == "3 – My schooling greatly contributed to my ability to use Hawaiian." ~ 3,
  educationData[, 1] == "" ~ NA_real_
)

#make some donuts to look at data trends
# List to store individual donut charts
donut_charts <- list()
# Unique generations in the dataset
unique_generations <- na.omit(unique(educationData$generation))
#make individual dataframe
dataSubset<-data.frame(educationData$generation, educationData[,1])%>%na.omit
names(dataSubset)<-c("generation","perception")
dataSubset$perception<-as.factor(dataSubset$perception)
#loop through generations
for (i in 1:length(unique_generations)){
	print(unique_generations[i])
pdfFileName<-paste(unique_generations[i],"_contributionofSchoolingOverall.pdf")
datasubsettarget<-filter(dataSubset, generation == unique_generations[i])
samplesize<-length(datasubsettarget[,1])
plotit<- ggplot(datasubsettarget, aes(x = "", fill = perception)) +
    geom_bar(width = 1, color = "white") +
    scale_fill_manual(values = c("3" = "skyblue", "2" = "orange", "1" = "darkred")) +
    coord_polar(theta = "y") +
    theme_void() +
    theme(legend.position = "bottom") +
    ggtitle(paste(datasubsettarget$generation)) +
    annotate("text", x = 1,y=0, label = samplesize, hjust = 0.5, vjust=50, size = 1.5) +
      geom_point(aes(x = 0, y = 0), size = 3, color = "white",inherit.aes = FALSE) +
      labs(fill="response")
pdf(pdfFileName, width = 8, height = 6)
print(plotit)
 dev.off()
}

#last bit- one line plot of the categories by generaiton for the other prompts, and then a second one breaking it down by went to immersion school or not;
#finally for non immersion, contrasting positive versus negative respondents
# Select only columns that start with "X" along with the generation column
columns_of_interest <- c("generation", grep("^X", names(educationData), value = TRUE))

# Filter the dataframe to include only the columns of interest
educationData_subset <- educationData %>% 
  select(all_of(columns_of_interest))

#get the names in place
prompts<-questions[33:41]
promptstrings<-unlist(lapply(prompts, function(x) strsplit(x, "-")))[seq(from=2, to=18,by=2)]
colnames(educationData_subset)<-c("generation","how well did your schooling contribute to your ability to use the Hawaiian language?",promptstrings)

# Group by generation and calculate the average for each column
namesofcolumns<-print(names(educationData_subset))
average_scores <- educationData_subset %>%
  group_by(generation) %>%
  summarise(across(namesofcolumns[2:11], ~ mean(., na.rm = TRUE)))

#convert to long form
average_scores <-average_scores %>% pivot_longer(2:11, names_to="RowPrompt", values_to="Average")

# View the result
print(average_scores)
average_scores %>% na.omit() %>%
  arrange(Average) %>%
  ggplot() +
  # Remove axes and superfluous grids
  theme_classic() +
  theme(axis.title = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line = element_blank()) +
  # Add a dummy point for scaling purposes (adjust the multiplier if needed)
  geom_point(aes(x = max(Average) * 1.1, y = RowPrompt), size = 0, col = "white") +
  # Add the horizontal lines for each group
  geom_hline(yintercept = seq_along(unique(combinedLI$RowPrompt)), col = "grey80") +
  # Add a point for each Average score, colored by generation
  geom_point(aes(x = Average, y = reorder(RowPrompt, Average), color = generation), size = 6) +
  # Define a color palette for generations
  scale_color_manual(values = c("GenZ" = "#FFDD57",     # Bright Yellow for GenZ
                              "Millennial" = "#FFA500", # Orange for Millennials
                              "GenX" = "#4682B4",      # Steel Blue for GenX
                              "Boomer" = "#8A2BE2")) +   # Blue Violet for Boomers 
  # Optionally, add labels for the points
  geom_text(aes(x = Average, y = reorder(RowPrompt, Average), label = round(Average, 2)), 
            vjust = 0.3, hjust = -0.3, size = 3) +  
  # Customize the plot with axis labels and title
  labs(title = "Average Scores by Prompt and Generation",
       x = "Average Score",
       y = "") +  
  # Additional formatting for a clean look
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))
 #If the above plots small, remember you can stretch the quartz window to resize 

####### For mann whitney U test convert to long form
#convert to long form
educationData_LongForm <-educationData_subset %>% pivot_longer(2:11, names_to="RowPrompt", values_to="Response") %>% na.omit()

# Filter the data to include only "GenZ" and "Millennial"
EDdata_subset <- educationData_LongForm %>%
  filter(generation %in% c("GenZ", "Millennial"))

# Function to run the Mann-Whitney U test for each RowPrompt
run_mann_whitney <- function(df) {
  # Ensure only GenZ and Millennial are present in the data
  if (length(unique(df$generation)) == 2) {
    test_result <- wilcox.test(Response ~ generation, data = df)
    return(test_result$p.value)
  } else {
    return(NA)  # Return NA if there aren't exactly two groups
  }
}

# Apply the Mann-Whitney U test for each unique RowPrompt
test_results <- EDdata_subset %>%
  group_by(RowPrompt) %>%
  summarise(p_value = run_mann_whitney(cur_data()))

# View the results
print(test_results)

##Lets look at when they attended versus ability. Our sample sizes are low so we should be cautious but I am putting this here in case someone is interested. Again, be cautious these sample sizes are very low!!

gradesImmersion<-data.frame(educationData_subset,grades=combinedData[,29], immersion=combinedData[,30])
gradesImmersion <- gradesImmersion %>%   filter(generation %in% c("GenZ", "Millennial")) %>% na.omit()

# Step 1: Clean the data by removing rows where grades is "None" or empty string ""
gradesImmersion_clean <- gradesImmersion %>%
  filter(grades != "None", grades != "")

# Step 2: Ensure 'X.Overall.quality.' is treated as a factor
gradesImmersion_clean <- gradesImmersion_clean %>%
  mutate(X.Overall.quality. = factor(X.Overall.quality., levels = c("1", "2", "3")))

# Reshape the data to long format for easier plotting
  gradesImmersion_clean[2:11] <- lapply(gradesImmersion_clean[2:11], as.numeric)
  data_long <- gradesImmersion_clean %>%
    pivot_longer(cols = 2:11, 
                 names_to = "Metric", 
                 values_to = "Score") %>%
    mutate(Metric = factor(Metric, levels = unique(Metric)))  # Control order of y-axis

  data_long %>%
    group_by(immersion, Metric) %>%
    summarise(Average = mean(Score, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(Average) %>%
    ggplot() +
    # Basic theme adjustments
    theme_classic() +
    theme(axis.title = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line = element_blank()) +
    # Dummy point for scaling
    geom_point(aes(x = max(Average) * 1.1, y = Metric), size = 0, col = "white") +
    # Horizontal lines for each metric
    geom_hline(yintercept = seq_along(unique(data_long$Metric)), col = "grey80") +
    # Plot points for each average score, colored by immersion status
    geom_point(aes(x = Average, y = reorder(Metric, Average), color = immersion), size = 6) +
    # Define colors for immersion and non-immersion
    scale_color_manual(values = c("Yes" = "#FF6347",    # Red for Immersion
                                  "No" = "#4682B4")) +  # Steel Blue for Non-Immersion
    # Add labels for the points
    geom_text(aes(x = Average, y = reorder(Metric, Average), label = round(Average, 2)), 
              vjust = 0.3, hjust = -0.3, size = 3) +  
    # Customize the plot with title and axis labels
    labs(title = "Average Scores by Metric and Immersion Status",
         x = "Average Score",
         y = "") +
    # Further theme adjustments for clarity
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10))

######################
######################
######################
############################################
######################




  