install.packages('epiDisplay')

library(epiDisplay)

# Frequency Tables & Bar Charts
tab1(smokingsurvey$`Age:`, main = "Bar Chart of Age")
tab1(smokingsurvey$`Sex:`, main = "Bar Chart of Sex")
tab1(smokingsurvey$`Weight: (kgs)`, main = "Bar Chart of Weight (kgs)")
tab1(smokingsurvey$`Height: (m)`, main = "Bar Chart of Height (m)")
tab1(smokingsurvey$`ncigar`, main = "Bar Chart of ncigar")
tab1(smokingsurvey$`nysmoke`, main = "Bar Chart of nysmoke")
tab1(smokingsurvey$`age smoke`, main = "Bar Chart of age smoke")

# Pie Charts
tb = smokingsurvey$`Age:`
tb.freq = table(tb)
pie(tb.freq, main = "Pie Chart of Age")

tb=smokingsurvey$`Sex:`
tb.freq = table(tb)
pie(tb.freq, main = "Pie Chart of Sex")

tb=smokingsurvey$`Weight: (kgs)`
tb.freq = table(tb)
pie(tb.freq, main = "Pie Chart of Weight (kgs)")

tb=smokingsurvey$`Height: (m)`
tb.freq = table(tb)
pie(tb.freq, main = "Pie Chart of Height (m)")

tb=smokingsurvey$`ncigar`
tb.freq = table(tb)
pie(tb.freq, main = "Pie Chart of ncigar")

tb=smokingsurvey$`nysmoke`
tb.freq = table(tb)
pie(tb.freq, main = "Pie Chart of nysmoke")

tb=smokingsurvey$`age smoke`
tb.freq = table(tb)
pie(tb.freq, main = "Pie Chart of age smoke")

# Histograms
hist(smokingsurvey$`Age:`, main = "Histogram of Age", xlab = "Age")
hist(smokingsurvey$`Sex:`, main = "Histogram of Sex", xlab = "Sex")
hist(smokingsurvey$`Weight: (kgs)`, main = "Histogram of Weight (kgs)", xlab = "Weight (kgs)")
hist(smokingsurvey$`Height: (m)`, main = "Histogram of Height (m)", xlab = "Height (m)")
hist(smokingsurvey$`ncigar`, main = "Histogram of ncigar", xlab = "ncigar")
hist(smokingsurvey$`nysmoke`, main = "Histogram of nysmoke", xlab = "nysmoke")
hist(smokingsurvey$`age smoke`, main = "Histogram of age smoke", xlab = "age smoke")

# Box Plots
boxplot(smokingsurvey$`Age:`, horizontal=TRUE, main = "Age Box Plot", xlab = "Age")
boxplot(smokingsurvey$`Sex:`, horizontal=TRUE, main = "Sex Box Plot", xlab = "Sex")
boxplot(smokingsurvey$`Weight: (kgs)`, horizontal=TRUE, main = "Weight Box Plot (kgs)", xlab = "Weight")
boxplot(smokingsurvey$`Height: (m)`, horizontal=TRUE, main = "Height Box Plot (m)", xlab = "Height")
boxplot(smokingsurvey$ncigar, horizontal=TRUE, main = "Ncigar Box Plot", xlab = "Ncigar")
boxplot(smokingsurvey$nysmoke, horizontal=TRUE, main = "Nysmoke Box Plot", xlab = "Nysmoke")
boxplot(smokingsurvey$`age smoke`, horizontal=TRUE, main = "Age Smoke Box Plot", xlab = "Age Smoke")
