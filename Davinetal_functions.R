##Custom Theme
customTheme <- theme_classic() +
theme(axis.line.y = element_blank(),
axis.ticks.y = element_blank(),
axis.text = element_text(color="black"),
axis.title= element_blank(),
legend.position= "none")

###Stick plot function
stickPlot<-function(dataframe, title="Ancestry"){
	ggplot(dataframe, aes(x = reorder(Var1, Freq), y = Freq)) +
  geom_segment(aes(x = reorder(Var1, Freq), xend = reorder(Var1, Freq), y = 0, yend = Freq, color = Freq), linewidth = 2) +  # Thicker lines and gradient color
  geom_point(aes(color = Freq), size = 3) +  # Color gradient for points as well
  scale_color_gradient(low = "red", high = "blue") +  # Gradient from red (low) to blue (high)
  coord_flip() +  # Flip the axes to make it sideways
  theme_minimal() +
  labs(x = "Ancestry", y = "Count", title = title) +
  theme(axis.text.y = element_text(size = 10),  # Adjust the size of the Y axis labels
        axis.title.y = element_blank()) +
customTheme}

# Function to create a donut chart for a specific generation
create_donut_chart <- function(generation_data, gen,columnIndex) {
  ggplot(generation_data, aes(x = "", fill = generation_data[,2])) +
    geom_bar(width = 1, color = "white") +
    scale_fill_manual(values = c("3" = "skyblue", "2" = "orange", "1" = "darkred")) +
    coord_polar(theta = "y") +
    theme_void() +
    theme(legend.position = "bottom") +
    ggtitle(paste(gen)) +
    annotate("text", x = 1,y=0, label = prompts[columnIndex], hjust = 0.5, vjust=50, size = 1.5) +
      geom_point(aes(x = 0, y = 0), size = 3, color = "white",inherit.aes = FALSE) +
      labs(fill="response")
}

#function to loop over all the generations and store in a list
allGens<-function(numericData, columnIndex){
# List to store individual donut charts
donut_charts <- list()
# Unique generations in the dataset
unique_generations <- na.omit(unique(numericData$generation))
#make individual dataframe
dataSubset<-data.frame(numericData$generation,numericData[,columnIndex])%>%na.omit
for (i in 1:length(unique_generations)){
# Create a donut chart for each generation
gen_data <- dataSubset %>% 
    filter(dataSubset[,1]==unique_generations[i])
    gen_data[,2]<-as.factor(gen_data[,2])
donut_charts[[i]]<-create_donut_chart(gen_data, unique_generations[i],columnIndex)
}
grid.arrange(grobs = donut_charts, ncol = 2)
}

#Function to summarize the generations for perceptions of language importance

languageImportance<-function(combinedData,questionNumbers=c(16:25)){
summaryData<-matrix(ncol=3, nrow=length(questionNumbers))
for (i in 1:length(questionNumbers)){
  targetrow<-questionNumbers[i]
  response<-sort(table(combinedData[,targetrow]))
  if ("1 – It is not important."%in%names(response)==TRUE) {
  summaryData[i,1]<-response[which(names(response)=="1 – It is not important.")]
  }
  if ("2 – It is important."%in%names(response)==TRUE) {
  summaryData[i,2]<-response[which(names(response)=="2 – It is important.")]
  }
  if ("3 – It is very important."%in%names(response)) {
  summaryData[i,3]<-response[which(names(response)=="3 – It is very important.")]
  }
}
colnames(summaryData)<-c("Not.important","Important","Very.important")
summaryData<-as.data.frame(summaryData)
#convert to scores
summaryData$badScore<-summaryData[,1]*1
summaryData$postiveScore<-summaryData[,2]*2
summaryData$extraPostiveScore<-summaryData[,3]*3

summaryData <- mutate(summaryData, responses = rowSums(select(summaryData, Not.important, Important, Very.important), na.rm = TRUE))
summaryData <- mutate(summaryData, Average = rowSums(select(summaryData, badScore, postiveScore, extraPostiveScore)/responses, na.rm = TRUE))
prompts<-questions[16:25]
promptstrings<-unlist(lapply(prompts, function(x) strsplit(x, "On a scale of 1 to 3, how important is it")))[seq(from=2, to=20,by=2)]
summaryData$RowPrompt<-promptstrings
summaryData<-arrange(summaryData,desc(Average))
summaryData$RowPrompt <- str_wrap(summaryData$RowPrompt, width = 45)
summaryData$RowPrompt<-factor(summaryData$RowPrompt, levels=summaryData$RowPrompt)
summaryData[is.na(summaryData)]<-0
return(summaryData)}

#function to get the questions into numbers
importantReplacer<-function(dataframe, questionNumbers=52:64){
dataframeNumber<-dataframe[, questionNumbers]
for (i in 1:10){
target1a<-dataframeNumber[,i]=="1 – It is not important."
target1b<-dataframeNumber[,i]=="1 – It is not  important."
target2a<-dataframeNumber[,i]=="2 – It is important."
target2b<-dataframeNumber[,i]=="2 – It is  important."
target3<-dataframeNumber[,i]=="3 – It is very important."
dataframeNumber[target1a,i]<-1
dataframeNumber[target1b,i]<-1
dataframeNumber[target2a,i]<-2
dataframeNumber[target2b,i]<-2
dataframeNumber[target3,i]<-3
}
return(dataframeNumber)
}

#function to prep for donut chart of motivation
motivationDonutPrep<-function(combinedData, questionNumbers=52:64){
donutVariables <- combinedData[, questionNumbers]
donutVariables<-as.data.frame(donutVariables)
donutVariables$age<-combinedData[, 11]
donutVariables$age <- as.numeric(gsub("\\D", "", donutVariables$age))
donutVariables <-na.omit(donutVariables)
for (i in 1:length(questionNumbers)){
	target1a<-donutVariables[,i]=="Disagree"
	target2a<-donutVariables[,i]=="Neither agree nor disagree"
	target3<-donutVariables[,i]=="Agree"
	target1b<-donutVariables[,i]==" Disagree"
	target2b<-donutVariables[,i]==" Neither agree nor disagree"
	target3b<-donutVariables[,i]==" Agree"
	target4<-donutVariables[,i]==""
	donutVariables[target1a,i]<-1
	donutVariables[target2a,i]<-2
	donutVariables[target3,i]<-3	
	donutVariables[target1b,i]<-1
	donutVariables[target2b,i]<-2
	donutVariables[target3,i]<-3	
	donutVariables[target4,i]<-NA
	}
donutVariables <-na.omit(donutVariables)
#track the questions
prompts<-questions[questionNumbers]	
donutVariables <- donutVariables %>%
  mutate(generation = cut(age, breaks = c(18,27, 43, 59, Inf), labels = c("GenZ", "Millennial", "GenX", "Boomers")))
return(donutVariables)}

##Use this to process the revitalization questions for research question 3
revitalizationProcessing<-function(combinedData, questionNumbers=86:100, agecutmin=0, ageCutmax=27){
	combinedData$X..ImportId...QID59_TEXT.. <- as.numeric(combinedData$X..ImportId...QID59_TEXT..)
	data2 <- filter(combinedData, X..ImportId...QID59_TEXT.. <= ageCutmax)
	data2 <- filter(combinedData, X..ImportId...QID59_TEXT.. >= agecutmin)
	targetVariables <- combinedData[, questionNumbers]
	summaryData<-matrix(ncol=4, nrow=length(questionNumbers))
	for (i in 1:length(questionNumbers)){
  		targetrow<-questionNumbers[i]
  		response<-sort(table(data2[,targetrow]))
  		if ("Not well"%in%names(response)==TRUE) {
  			summaryData[i,1]<-response[which(names(response)=="Not well")]
  			}
  		if ("Well"%in%names(response)==TRUE) {
  			summaryData[i,2]<-response[which(names(response)=="Well")]
  			}
  		if ("Very well"%in%names(response)) {
  			summaryData[i,3]<-response[which(names(response)=="Very well")]
  			}
  		if ("I don't know."%in%names(response)) {
  			summaryData[i,4]<-response[which(names(response)=="I don't know.")]
  			}
		}
	colnames(summaryData)<-c("Not.well","Well","Very.well","Donot.Know")
	summaryData<-as.data.frame(summaryData)
	#convert to scores
	summaryData$badScore<-summaryData[,1]*1
	summaryData$postiveScore<-summaryData[,2]*2
	summaryData$extraPostiveScore<-summaryData[,3]*3
	summaryData <- mutate(summaryData, responses = rowSums(select(summaryData, Not.well, Well, Very.well), 	na.rm = TRUE))
	summaryData <- mutate(summaryData, Average = rowSums(select(summaryData, badScore, postiveScore, extraPostiveScore)/responses, na.rm = TRUE))
	prompts<-questions[86:100]
	promptstrings<-unlist(lapply(prompts, function(x) strsplit(x, "-")))[seq(from=3, to=45,by=3)]
	summaryData$RowPrompt<-promptstrings
	summaryData<-arrange(summaryData,desc(Average))
	summaryData$RowPrompt<-factor(summaryData$RowPrompt, levels=summaryData$RowPrompt)
	return(summaryData)
	}
	
ggDot <- function(combinedLI){
	combinedLI %>%
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
  scale_color_manual(values = c("genZ" = "#FFDD57",     # Bright Yellow for GenZ
                              "millenial" = "#FFA500", # Orange for Millennials
                              "genX" = "#4682B4",      # Steel Blue for GenX
                              "boomer" = "#8A2BE2")) +   # Blue Violet for Boomers 
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
}

# Define a function to run ANOVA and pairwise t-tests
run_anova_and_ttests <- function(df) {
  # Perform ANOVA
  anova_model <- aov(Importance ~ generation, data = df)  
  # Get the ANOVA results
  anova_summary <- tidy(anova_model)  
  # Perform pairwise t-tests if ANOVA is significant
  if (anova_summary$p.value[1] < 0.05) {
    pairwise_tests <- pairwise.t.test(df$Importance, df$generation, p.adjust.method = "BH")
    pairwise_summary <- tidy(pairwise_tests)
  } else {
    pairwise_summary <- NULL
  }  
  # Return both the ANOVA and pairwise t-test results as a data frame
  list(anova = anova_summary, pairwise = pairwise_summary)
}



