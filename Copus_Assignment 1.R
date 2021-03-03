##Amy Copus
##64060 - Assignment 1
##22-Jan-2021

###all the libraries!!!!!####
library(tidyverse) #tidyverse for the everything
library(arsenal) #descriptives table
##library(ggplot2)
#######


### 1. Download a dataset & import into R
iris.ML<-(iris) #get iris data
view(iris.ML)



### 3. descriptive statistics for a selection of quant and categorical variables

##descriptive statistics using library-arsenal

##start recording output##

##setwd() ###THIS IS GOING TO CREATE A DOCUMENT... soo.. somewhere you can find it.
sink("./acopus_assignment1.txt", append = T)



  descriptives <- tableby.control(
    test = T,
    total = T,
    numeric.test = "kwt", cat.test = "chisq",
    numeric.stats = c("meansd", "medianq1q3", "range", "Nmiss2"),   #stats
    cat.stats = c("countpct", "Nmiss2"),
    stats.labels = list( ###stats labels
      meansd = "Mean (SD)",
      medianq1q3 = "Median (Q1, Q3)",
      range = "Min - Max",
      Nmiss2 = "Missing"
      )
    )
  my_labels <- list(     ##label the groups
    Sepal.Length = "Sepal Length",
    Sepal.Width = "Sepal Width",
    Petal.Length = "Petal Length",
    Petal.Width = "Petal Width"
    )
  table_two <- tableby(Species ~ .,     ##label the columns
                       data = iris.ML,
                       control = descriptives
                       )
  summary(table_two,     #make the table
          labelTranslations = my_labels,
          title = "Descriptive Statistics of Iris Data",
          text=TRUE)

  sink() ##stop recording output##
##check files for "acopus_assignment1 - it will look better opened in the txt file.##



## 4. Transform at least one variable


iris.ML<-transform(iris.ML, Sepal.Ratio = (Sepal.Width/Sepal.Length)) #create new column = sepal W/L


##view(iris.ML)



## 5. Plot at least one quant variable and one scatterplot

##scatterplot - petal length/width - species by color

ggplot(data = iris.ML, mapping = aes(x = Petal.Length, y = Petal.Width, color = Species))+
  geom_point(size = 2)+
  scale_color_manual(values = c("darkorchid4", "gold2", "springgreen4"))+ ##plot colors
  labs(title = "Iris Scatterplot", ##title
       x = "Petal Length", #x axis title
       y = "Petal Width")+ #y axis title
  theme_light()+
  theme(
    plot.title = element_text(color = "black", size = 14, face = "bold", hjust = 0.5), #title = color, size, bold, center
    axis.line = element_line(color = "dimgray", size = 1, linetype = "solid") #axis line = color, size, type
  )







##plotting quant data

boxplot(Sepal.Length~Species, data=iris.ML,
        main="Iris Boxplot", #plot label
        xlab="",ylab="", #removing auto x/y titles
        yaxt="none") #removes auto numbers
axis(2, seq(50,80,5), las=2) #y axis numbering and rotation
abline(h=seq(60,75,5), lty=3, col="gray77") #horizontal gridlines
mtext(side=1, line = 2, "Species", col = 'black', font=2,cex=1.2) #x axis title, color, bold
mtext(side=2, line = 2, "Sepal Length", col = 'black', font=2,cex=1.2) #y axis title, color, bold


  

##END OF ASSIGNMENT###  