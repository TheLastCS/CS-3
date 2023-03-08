# Install the required packages
install.packages("readxl")
install.packages("tidyverse")
install.packages("gridExtra")
install.packages("psych")

# Load the required packages
library(ggplot2)
library(gridExtra)
library(grid)
library(readxl)
library(dplyr)
library(psych)

df <- read_excel("smokingsurvey.xlsx")
View(df)

# Change the header name of the column 1-4
names(df)[1] <- "Age"
names(df)[2] <- "Sex"
names(df)[3] <- "Weight"
names(df)[4] <- "Height"
names(df)[7] <- "agesmoke"


# Calculate Frequencies of each observation (frequency, percent, and cumulative percent)

#Age
Age_freq <- table(df$Age)
Age_cum_freq <- cumsum(Age_freq)
Age_table <- data.frame(Age = names(Age_freq), 
                        Frequency = as.numeric(Age_freq), 
                        Cumulative_frequency = Age_cum_freq)
Age_table$Percent <- round(Age_table$Frequency/sum(Age_table$Frequency)*100, 2)
Age_table$Cumulative_percent <- round(Age_table$Cumulative_frequency/sum(Age_table$Frequency)*100, 2)

Age_label <- "Age Frequency Table"
Age_label_grob <- textGrob(Age_label, gp = gpar(fontsize = 14))


#Sex
Sex_freq <- table(df$Sex)
Sex_cum_freq <- cumsum(Sex_freq)
Sex_table <- data.frame(Sex = names(Sex_freq), 
                        Frequency = as.numeric(Sex_freq), 
                        Cumulative_frequency = Sex_cum_freq)
Sex_table$Percent <- round(Sex_table$Frequency/sum(Sex_table$Frequency)*100, 2)
Sex_table$Cumulative_percent <- round(Sex_table$Cumulative_frequency/sum(Sex_table$Frequency)*100, 2)

Sex_label <- "Sex Frequency Table"
Sex_label_grob <- textGrob(Sex_label, gp = gpar(fontsize = 14))


#weight
Weight_freq <- table(df$Weight)
Weight_cum_freq <- cumsum(Weight_freq)
Weight_table <- data.frame(Weight = names(Weight_freq), 
                           Frequency = as.numeric(Weight_freq), 
                           Cumulative_frequency = Weight_cum_freq)
Weight_table$Percent <- round(Weight_table$Frequency/sum(Weight_table$Frequency)*100, 2)
Weight_table$Cumulative_percent <- round(Weight_table$Cumulative_frequency/sum(Weight_table$Frequency)*100, 2)

Weight_label <- "Weight(kgs) Frequency Table"
Weight_label_grob <- textGrob(Weight_label, gp = gpar(fontsize = 14))

#height
Height_freq <- table(df$Height)
Height_cum_freq <- cumsum(Height_freq)
Height_table <- data.frame(Height = names(Height_freq), 
                           Frequency = as.numeric(Height_freq), 
                           Cumulative_frequency = Height_cum_freq)
Height_table$Percent <- round(Height_table$Frequency/sum(Height_table$Frequency)*100, 2)
Height_table$Cumulative_percent <- round(Height_table$Cumulative_frequency/sum(Height_table$Frequency)*100, 2)

Height_label <- "Height(m) Frequency Table"
Height_label_grob <- textGrob(Height_label, gp = gpar(fontsize = 14))

#ncigar
ncigar_freq <- table(df$ncigar)
ncigar_cum_freq <- cumsum(ncigar_freq)
ncigar_table <- data.frame(ncigar = names(ncigar_freq), 
                           Frequency = as.numeric(ncigar_freq), 
                           Cumulative_frequency = ncigar_cum_freq)
ncigar_table$Percent <- round(ncigar_table$Frequency/sum(ncigar_table$Frequency)*100, 2)
ncigar_table$Cumulative_percent <- round(ncigar_table$Cumulative_frequency/sum(ncigar_table$Frequency)*100, 2)

ncigar_label <- "ncigar Frequency Table"
ncigar_label_grob <- textGrob(ncigar_label, gp = gpar(fontsize = 14))

#nysmoke
nysmoke_freq <- table(df$nysmoke)
nysmoke_cum_freq <- cumsum(nysmoke_freq)
nysmoke_table <- data.frame(nysmoke = names(nysmoke_freq), 
                            Frequency = as.numeric(nysmoke_freq), 
                            Cumulative_frequency = nysmoke_cum_freq)
nysmoke_table$Percent <- round(nysmoke_table$Frequency/sum(nysmoke_table$Frequency)*100, 2)
nysmoke_table$Cumulative_percent <- round(nysmoke_table$Cumulative_frequency/sum(nysmoke_table$Frequency)*100, 2)

nysmoke_label <- "nysmoke Frequency Table"
nysmoke_label_grob <- textGrob(nysmoke_label, gp = gpar(fontsize = 14))

#agesmoke
agesmoke_freq <- table(df$agesmoke)
agesmoke_cum_freq <- cumsum(agesmoke_freq)
agesmoke_table <- data.frame(agesmoke = names(agesmoke_freq), 
                             Frequency = as.numeric(agesmoke_freq), 
                             Cumulative_frequency = agesmoke_cum_freq)
agesmoke_table$Percent <- round(agesmoke_table$Frequency/sum(agesmoke_table$Frequency)*100, 2)
agesmoke_table$Cumulative_percent <- round(agesmoke_table$Cumulative_frequency/sum(agesmoke_table$Frequency)*100, 2)

agesmoke_label <- "agesmoke Frequency Table"
agesmoke_label_grob <- textGrob(agesmoke_label, gp = gpar(fontsize = 14))


#Charts (Bar, Pie, BoxPlots)

#Barchart

Age_barchart <- ggplot(Age_table, aes(x = Age, y = Frequency)) +
  geom_bar(stat = "identity") +
  labs(x = "Age", y = "Frequency") +
  ggtitle("Age Distribution")

Sex_barchart <- ggplot(Sex_table, aes(x = Sex, y = Frequency)) +
  geom_bar(stat = "identity") +
  labs(x = "Sex", y = "Frequency") +
  ggtitle("Sex Distribution")

Weight_barchart <- ggplot(Weight_table, aes(x = Weight, y = Frequency)) +
  geom_bar(stat = "identity") +
  labs(x = "Weight", y = "Frequency") +
  ggtitle("Weight(kgs) Distribution")

Height_barchart <- ggplot(Height_table, aes(x = Height, y = Frequency)) +
  geom_bar(stat = "identity") +
  labs(x = "Height", y = "Frequency") +
  ggtitle("Height(m) Distribution")

ncigar_barchart <- ggplot(ncigar_table, aes(x = ncigar, y = Frequency)) +
  geom_bar(stat = "identity") +
  labs(x = "ncigar", y = "Frequency") +
  ggtitle("ncigar Distribution")

nysmoke_barchart <- ggplot(nysmoke_table, aes(x = nysmoke, y = Frequency)) +
  geom_bar(stat = "identity") +
  labs(x = "nysmoke", y = "Frequency") +
  ggtitle("nysmoke Distribution")

agesmoke_barchart <- ggplot(agesmoke_table, aes(x = agesmoke, y = Frequency)) +
  geom_bar(stat = "identity") +
  labs(x = "agesmoke", y = "Frequency") +
  ggtitle("agesmoke Distribution")


#Piechart
Age_piechart <- ggplot(Age_table, aes(x = "", y = Frequency, fill = Age)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  ggtitle("Age Distribution")

Sex_piechart <- ggplot(Sex_table, aes(x = "", y = Frequency, fill = Sex)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  ggtitle("Sex Distribution")

Weight_piechart <- ggplot(Weight_table, aes(x = "", y = Frequency, fill = Weight)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  ggtitle("Weight (kgs) Distribution")

Height_piechart <- ggplot(Height_table, aes(x = "", y = Frequency, fill = Height)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  ggtitle("Height (m) Distribution")

ncigar_piechart <- ggplot(ncigar_table, aes(x = "", y = Frequency, fill = ncigar)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  ggtitle("ncigar Distribution")

nysmoke_piechart <- ggplot(nysmoke_table, aes(x = "", y = Frequency, fill = nysmoke)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  ggtitle("nysmoke Distribution")

agesmoke_piechart <- ggplot(agesmoke_table, aes(x = "", y = Frequency, fill = agesmoke)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  ggtitle("agesmoke Distribution")

#Boxplots
Age_boxplot <- ggplot(df, aes(x = Age)) + 
  geom_boxplot(fill = "lightblue", color = "blue") + 
  ggtitle("Box plot of Age") + 
  xlab("Age")
Sex_boxplot <- ggplot(df, aes(x = Sex)) + 
  geom_boxplot(fill = "lightblue", color = "blue") + 
  ggtitle("Box plot of Sex") + 
  xlab("Sex")
Height_boxplot <- ggplot(df, aes(x = Height)) + 
  geom_boxplot(fill = "lightblue", color = "blue") + 
  ggtitle("Box plot of Height") + 
  xlab("Height")
Weight_boxplot <- ggplot(df, aes(x = Weight)) + 
  geom_boxplot(fill = "lightblue", color = "blue") + 
  ggtitle("Box plot of Weight") + 
  xlab("Weight")
ncigar_boxplot <- ggplot(df, aes(x = ncigar)) + 
  geom_boxplot(fill = "lightblue", color = "blue") + 
  ggtitle("Box plot of ncigar") + 
  xlab("ncigar")
nysmoke_boxplot <- ggplot(df, aes(x = nysmoke)) + 
  geom_boxplot(fill = "lightblue", color = "blue") + 
  ggtitle("Box plot of nysmoke") + 
  xlab("nysmoke")
agesmoke_boxplot <- ggplot(df, aes(x = agesmoke)) + 
  geom_boxplot(fill = "lightblue", color = "blue") + 
  ggtitle("Box plot of agesmoke") + 
  xlab("agesmoke")

#histogram
Age_histogram <- ggplot(df, aes(x = Age)) + 
  geom_histogram(binwidth = 0.5, fill = "blue", color = "white") + 
  ggtitle("Histogram of Age") + 
  xlab("Age")

Sex_histogram <- ggplot(df, aes(x = Sex)) + 
  geom_histogram(binwidth = 0.5, fill = "blue", color = "white") + 
  ggtitle("Histogram of Sex") + 
  xlab("Sex")
Height_histogram <- ggplot(df, aes(x = Height)) + 
  geom_histogram(binwidth = 0.5, fill = "blue", color = "white") + 
  ggtitle("Histogram of Height") + 
  xlab("Height")
Weight_histogram <- ggplot(df, aes(x = Weight)) + 
  geom_histogram(binwidth = 0.5, fill = "blue", color = "white") + 
  ggtitle("Histogram of Weight") + 
  xlab("Weight")
ncigar_histogram <- ggplot(df, aes(x = ncigar)) + 
  geom_histogram(binwidth = 0.5, fill = "blue", color = "white") + 
  ggtitle("Histogram of ncigar") + 
  xlab("ncigar")
nysmoke_histogram <- ggplot(df, aes(x = nysmoke)) + 
  geom_histogram(binwidth = 0.5, fill = "blue", color = "white") + 
  ggtitle("Histogram of nysmoke") + 
  xlab("nysmoke")
agesmoke_histogram <- ggplot(df, aes(x = agesmoke)) + 
  geom_histogram(binwidth = 0.5, fill = "blue", color = "white") + 
  ggtitle("Histogram of agesmoke") + 
  xlab("agesmoke")
#export table and grid to pdf file 
pdf("Stewart_Descriptives in R.pdf", height = 12, width = 12)


#frequencies
grid.arrange(Age_label_grob, tableGrob(Age_table,rows=NULL), ncol = 1, heights = c(0.1, 0.8))
grid.arrange(Sex_label_grob, tableGrob(Sex_table,rows=NULL), ncol = 1, heights = c(0.1, 0.8))
grid.arrange(Weight_label_grob, tableGrob(Weight_table,rows=NULL), ncol = 1, heights = c(0.1, 0.8))
grid.arrange(Height_label_grob, tableGrob(Height_table,rows=NULL), ncol = 1, heights = c(0.1, 0.8))
grid.arrange(ncigar_label_grob, tableGrob(ncigar_table,rows=NULL), ncol = 1, heights = c(0.1, 0.8))
grid.arrange(nysmoke_label_grob, tableGrob(nysmoke_table,rows=NULL), ncol = 1, heights = c(0.1, 0.8))
grid.arrange(agesmoke_label_grob, tableGrob(agesmoke_table,rows=NULL), ncol = 1, heights = c(0.1, 0.8))

#charts
grid.arrange(Age_barchart,Sex_barchart,Weight_barchart, Height_barchart,ncol=2)
grid.arrange(ncigar_barchart, nysmoke_barchart, agesmoke_barchart,ncol=2)
grid.arrange(Age_piechart, Sex_piechart,Weight_piechart,Height_piechart,ncol=2)
grid.arrange(ncigar_piechart, nysmoke_piechart, agesmoke_piechart,ncol=2)
grid.arrange(Age_histogram, Sex_histogram,Weight_histogram,Height_histogram,ncigar_histogram, nysmoke_histogram, agesmoke_histogram,ncol=2)
grid.arrange(Age_boxplot, Sex_boxplot,Weight_boxplot,Height_boxplot,ncigar_boxplot, nysmoke_boxplot, agesmoke_boxplot,ncol=2)

#descriptives
summary_label <- "Five-number summary"
summary_label_grob <- textGrob(summary_label, gp = gpar(fontsize = 14))
summary_table <- tableGrob(as.matrix(summary(df)), rows = NULL)

describe_label <- "Central Tendency and Measure of Dispersion"
describe_label_grob <- textGrob(describe_label, gp = gpar(fontsize = 14))

desc_stats = describe(df)
desc_stats$mean <- round(desc_stats$mean, 2)
desc_stats$sd <- round(desc_stats$sd, 2)
desc_stats$var <- round(desc_stats$var, 2)
desc_stats$se <- round(desc_stats$se, 2)
desc_stats$skew <- round(desc_stats$skew, 2)
desc_stats$kurtosis <- round(desc_stats$kurtosis, 2)

describe_table <- tableGrob(desc_stats)

grid.arrange(summary_label_grob,summary_table,ncol=1)
grid.arrange(describe_label_grob,describe_table,ncol=1)


# Close the PDF file
dev.off()
