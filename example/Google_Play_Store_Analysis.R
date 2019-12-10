install.packages("ggplot2")
install.packages("plotly")
install.packaages("dplyr")

library(ggplot2)
library(plotly)
library(dplyr)

data <- read.csv("GooglePlaystoreDataset.csv",stringsAsFactors=FALSE)

#To check for NULL Values in dataset

data <- data[complete.cases(data), ]

is.na(data)

TopCategoryApp <- aggregate(Rating~Category, data, max)

total <- merge(data,TopCategoryApp,by=c("Category","Rating"))

df = total[-1,]

Reorder<-df[order(df$Rating,decreasing = TRUE),]

Top <- head(Reorder,10)

agg_data<-select(Top,c(1,2,3))

Top_Rating<-ggplot(agg_data, aes(App, Rating,fill=App))

Top_Rating +geom_bar(stat = "identity") +
  
  xlab("Apps") + ylab("") +
  
  ggtitle("Top 10 Rating Apps") +
  
  theme(axis.text.x = element_text(angle=15, hjust=1),legend.position="none")

data$Installs<-gsub('^[+]*|[+ ]*$', '', data$Installs)

data$Installs <- gsub(",", "", data$Installs)

data$Installs<-as.numeric(as.factor(data$Installs))

sum_install <- aggregate(Installs~Category, data, sum)

df1 = sum_install[-1,]

Pie_categoryinstalls <- plot_ly(sum_install, labels = ~Category, values = ~Installs, type = 'pie') %>%
  
  layout(title = 'Total Installations in each Category',
         
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

Pie_categoryinstalls

Paid_Apps<-subset(df,Type=="Paid")

Free_Paid_Apps<-ggplot(data = Paid_Apps, aes(x = Category, y = Rating)) +
  
  geom_bar(aes(fill = App), stat = "identity") +
  
  theme(axis.text.x = element_text(angle=25, hjust=1),legend.position="none") +
  
  xlab("Apps") + ylab("Count") + ggtitle("Top Apps in Paid category")

ggplotly(Free_Paid_Apps)

