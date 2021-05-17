### ST4248 WINE PREDICTION ###
data = read.csv("C:/Users/Kiat Kai/Desktop/NOTES/Y4S2/ST4248/winequality-red.csv")
library(ggplot2)
library(corrplot)
library(fmsb)
sum(is.na(data))
# There are no missing data in this dataset.

####################################
# Explanatory Descriptive Analysis #
####################################

# Distribution of wine quality
ggplot(data, aes(quality)) + 
  geom_histogram(binwidth=1, color="white", fill="sienna3") + 
  labs(title="Distribution of wine quality") 
    # Observations: The data set is imbalanced hence we perform upsampling 
    # and downsampling

# Distribution of alcohol content
ggplot(data, aes(alcohol)) + 
  geom_histogram(binwidth=0.2, color="white", fill="salmon3") + 
  labs(title="Distribution of alcohol content")
# Observations: Seems like most wine alcohol content is between 9 to 10%

balanced_segment = read.csv("C:/Users/Kiat Kai/Desktop/NOTES/Y4S2/ST4248/wine_segmentations.csv")
balanced_segment = subset(balanced_segment, select=-X)
# Distribution of wine quality
ggplot(balanced_segment, aes(quality)) + 
  geom_bar(color="white", fill="sienna3") + 
  labs(title="Distribution of wine quality") 
# Observations: The data set is imbalanced hence we perform upsampling 
# and downsampling

# Distribution of alcohol content
ggplot(balanced_segment, aes(alcohol)) + 
  geom_histogram(binwidth=0.2, color="white", fill="salmon3") + 
  labs(title="Distribution of alcohol content")
     # Observations: Seems like most wine alcohol content is between 9 to 111%

# Boxplot of different wine quality value
ggplot(balanced_segment, aes(x=quality,y=citric.acid, fill=quality)) +
  geom_boxplot() +
  labs(title="Boxplot of citric acid for different wine quality",
       x="Wine Quality")

# Correlation Plot
res = cor(balanced_segment)
corrplot(res, type = "upper", 
         tl.col = "black", tl.srt = 35)
corrplot(res, type = "upper",
         tl.col = "black", tl.srt = 35)
    # Observations: No variables seems to be highly correlated with each other

# Correlation Plot but with try to remove variables that are highly collinear
# Maybe remove "fixed.acidity"
df1 = subset(df, select = -fixed.acidity)
res = cor(df1)
corrplot(res, type = "upper", 
         tl.col = "black", tl.srt = 35)
corrplot(res, type = "upper", method = "number",
         tl.col = "black", tl.srt = 35)

#############################################################
### Creating individual spider plots for wine quality = 3 ###
#############################################################
# Creating a spider plot for different wine quality by averages of variables
wine_3 = data.frame(lapply(df[df$quality == 3,], mean))
wine_3 = subset(wine_3, select=-c(free.sulfur.dioxide,total.sulfur.dioxide))
wine_3 <- rbind(rep(12,10) , rep(0,10) , wine_3)
radarchart( wine_3  , axistype=0 ,
            
            #custom polygon
            pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.4) , plwd=3 , plty=1, 
            
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,12,3), cglwd=0.2,
            
            #custom labels
            vlcex=0.8 
)

#############################################################
### Creating individual spider plots for wine quality = 5 ###
#############################################################
# Creating a spider plot for different wine quality by averages of variables
wine_5 = data.frame(lapply(df[df$quality == 5,], mean))
wine_5 = subset(wine_5, select=-c(free.sulfur.dioxide,total.sulfur.dioxide))
wine_5 <- rbind(rep(12,10) , rep(0,10) , wine_5)
radarchart( wine_5  , axistype=0 ,
            
            #custom polygon
            pcol=rgb(0.8,0.2,0.5,0.9) , pfcol=rgb(0.8,0.2,0.5,0.4) , plwd=3 , plty=1, 
            
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,12,3), cglwd=0.2,
            
            #custom labels
            vlcex=0.8 
)

#############################################################
### Creating individual spider plots for wine quality = 8 ###
#############################################################
# Creating a spider plot for different wine quality by averages of variables
wine_8 = data.frame(lapply(df[df$quality == 8,], mean))
wine_8 = subset(wine_8, select=-c(free.sulfur.dioxide,total.sulfur.dioxide))
wine_8 <- rbind(rep(12,10) , rep(0,10) , wine_8)
radarchart( wine_8  , axistype=0 ,
            
            #custom polygon
            pcol=rgb(0.7,0.5,0.1,0.9) , pfcol=rgb(0.7,0.5,0.1,0.4) , plwd=3 , plty=1, 
            
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,12,3), cglwd=0.2,
            
            #custom labels
            vlcex=0.8 
)

####################################################
## Read in data with upsampling and segmentations ##
####################################################
full_df_segment = read.csv("C:/Users/Kiat Kai/Desktop/NOTES/Y4S2/ST4248/full_df_with_segmentation.csv")
full_df_segment = subset(full_df_segment, select=-X)

###############################################################
### Creating individual spider plots for wine quality = bad ###
###############################################################
# Creating a spider plot for different wine quality by averages of variables
wine_bad = data.frame(lapply(balanced_segment[balanced_segment$quality == "bad",1:11], mean))
wine_bad = subset(wine_bad, select=-c(free.sulfur.dioxide,total.sulfur.dioxide))
wine_bad <- rbind(rep(12,10) , rep(0,10) , wine_bad)
radarchart( wine_bad  , axistype=0 ,
            
            #custom polygon
            pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.4) , plwd=1 , plty=1, 
            
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,12,3), cglwd=0.2,
            
            #custom labels
            vlcex=0.8 
)
##################################################################
### Creating individual spider plots for wine quality = normal ###
##################################################################
# Creating a spider plot for different wine quality by averages of variables
wine_normal = data.frame(lapply(full_df_segment[full_df_segment$quality == "normal",1:11], mean))
wine_normal = subset(wine_normal, select=-c(free.sulfur.dioxide,total.sulfur.dioxide))
wine_normal <- rbind(rep(12,10) , rep(0,10) , wine_normal)
radarchart( wine_normal  , axistype=0 ,
            
            #custom polygon
            pcol=rgb(0.8,0.2,0.5,0.9) , pfcol=rgb(0.8,0.2,0.5,0.4) , plwd=1 , plty=1, 
            
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,12,3), cglwd=0.2,
            
            #custom labels
            vlcex=0.8 
)
################################################################
### Creating individual spider plots for wine quality = good ###
################################################################
# Creating a spider plot for different wine quality by averages of variables
wine_good = data.frame(lapply(full_df_segment[full_df_segment$quality == "good",1:11], mean))
wine_good = subset(wine_good, select=-c(free.sulfur.dioxide,total.sulfur.dioxide))
wine_good <- rbind(rep(12,10) , rep(0,10) , wine_good)
radarchart( wine_good  , axistype=0 ,
            
            #custom polygon
            pcol=rgb(0.7,0.5,0.1,0.9) , pfcol=rgb(0.7,0.5,0.1,0.4) , plwd=1 , plty=1, 
            
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,12,3), cglwd=0.2,
            
            #custom labels
            vlcex=0.8 
)
###############################################
## Combing al 3 quality into one spider plot ##
###############################################
wine_bad = data.frame(lapply(full_df_segment[full_df_segment$quality == "bad",1:11], mean))
wine_bad = subset(wine_bad, select=-c(free.sulfur.dioxide,total.sulfur.dioxide))
wine_normal = data.frame(lapply(full_df_segment[full_df_segment$quality == "normal",1:11], mean))
wine_normal = subset(wine_normal, select=-c(free.sulfur.dioxide,total.sulfur.dioxide))
wine_good = data.frame(lapply(full_df_segment[full_df_segment$quality == "good",1:11], mean))
wine_good = subset(wine_good, select=-c(free.sulfur.dioxide,total.sulfur.dioxide))
wine_all = rbind(wine_bad, wine_normal)
wine_all = rbind(wine_all,wine_good)
wine_all <- rbind(rep(12,10) , rep(0,10) , wine_all)


colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )

# plot with default options:
radarchart( wine_all  , axistype=0 , 
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=0.5 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
            #custom labels
            vlcex=0.8 
)
# Add a legend
legend(x=1.35, y=0.9, legend = c("bad","normal","good"), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.1, pt.cex=2)

##############################################################################

############################################################################
### Plotting the barplot to compare the scores from the models in python ###
############################################################################

# For Upsampling:
model_results = read.csv("C:/Users/Kiat Kai/Desktop/NOTES/Y4S2/ST4248/model_results_upsample.csv")
model_results = subset(model_results, select=-X)
model_results = model_results[-6,]
ggplot(model_results, aes(Models,Accuracy.Score, fill=Models)) +
  geom_col() + theme(legend.position = "none",text = element_text(size=10)) + 
  scale_fill_brewer(palette="Dark2") + ylim(0.0, 1.0) + 
  geom_text(aes(label = round(Accuracy.Score,4)), vjust = -0.5) + 
  labs(title="Accuracy Score for different models (Upsample)")

# For original data without upsampling:
model_results = read.csv("C:/Users/Kiat Kai/Desktop/NOTES/Y4S2/ST4248/model_results_noupsample.csv")
model_results = subset(model_results, select=-X)
model_results = model_results[-6,]
ggplot(model_results, aes(Models,Accuracy.Score, fill=Models)) +
  geom_col() + theme(legend.position = "none",text = element_text(size=10)) + 
  scale_fill_brewer(palette="Dark2") + ylim(0.0, 1.0)  + 
  geom_text(aes(label = round(Accuracy.Score,4)), vjust = -0.5) + 
  labs(title="Accuracy Score for different models (Original)")

# For original data with upsampling and segmentations:
model_results = read.csv("C:/Users/Kiat Kai/Desktop/NOTES/Y4S2/ST4248/model_results_upsample_segmentation.csv")
model_results = subset(model_results, select=-X)
model_results = model_results[-6,]
ggplot(model_results, aes(Models,Accuracy.Score, fill=Models)) +
  geom_col() + theme(legend.position = "none",text = element_text(size=10)) + 
  scale_fill_brewer(palette="Dark2") + ylim(0.0, 1.0)  + 
  geom_text(aes(label = round(Accuracy.Score,4)), vjust = -0.5) + 
  labs(title="Accuracy Score for different models (Up-sample with segmentation)")
