# Read Head and str
head(Data1)
str(Data1)
# check for missing data
apply(Data1, MARGIN = 2, FUN = function(x) sum(is.na(x)))
library(Amelia)
missmap(Data1, col = c("blue", "grey"), legend = FALSE)

#create a data frame for independent variable
iv_all<-data.frame(Data1$`P_Age (0 to 19)`,Data1$`P_Age (20 to 44)`,Data1$`P_Age(45 to 64)`,Data1$`P_Age( 65 and above)`,Data1$P_Whites,Data1$P_Asians,Data1$P_Africans,Data1$P_others,Data1$P_Good_health,Data1$P_Fair_health,Data1$P_Bad_Health,Data1$P_No_Qual,Data1$P_Level_1,Data1$P_Level_2,Data1$P_Level_3,Data1$P_Level_4,Data1$P_Level_other)
colnames(iv_all)<-c ("0_19","20_44", "45_64", "65 and above","Whites","Asians", "Africans", "others","good_heath","fair_health", "bad_health","No_qual","level_1", "level_2", "level_3", "level_4","level_other")

#create a data frame for dependent variable
total_deaths <- data.frame(Data1$P_C_Deaths)
colnames(total_deaths)<- c("total_deaths")

#check whether dependent variable is normally distributed ?
boxplot(total_deaths,col=(c("gold")),main="Boxplot for total deaths", ylab="total deaths")

#normalise dependent variable 
#Create a function to normalise the dependent variable
normalise <- function(x){return((x - min(x,na.rm = TRUE))/ (max(x,na.rm = TRUE) - min(x,na.rm = TRUE)))}
total_deaths<-as.data.frame(apply(total_deaths,2,normalise))
#check again for dependent variable for normalisation
boxplot(total_deaths,col=(c("gold")),main="Boxplot for total deaths", ylab="total deaths")
#create a data frame for all variables
final_df <- data.frame(total_deaths,iv_all)
#check qq plot for normalisation
library(car)
qqPlot(final_df$total_deaths ,main = "Scatter Plot Covid Deaths")
#hence data is normally distributed
#testing correlation with matrix
cor.m.final_df <- cor(final_df, use = "pairwise.complete.obs", method = "spearman")
round(cor.m.final_df, digits = 2)
print(cor.m.final_df)
#visualisation for correlation matrix
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
#corrplot
install.packages("corrplot")

library(corrplot)

corrplot(cor.m.final_df, method="color", col=col(200),type="upper",tl.col="black",addCoef.col = "black",number.cex =0.5, tl.srt=90,t1.size=8, main = "Corrplot total_deaths ~independent variables",line = -2)
#Remove variables that are no required after correlation testing

# select variables by excluding those not required; the %in% operator means 'matching'
new_data <- names(final_df) %in% c("level_3","level_4","level_other","good_heath","fair_health","others")

# the ! operator means NOT
new_data1 <- final_df[!new_data]
str(new_data1)
rm(new_data)
colnames(new_data1)<-c ("total_deaths","0_19","20_44", "45_64", "65 and above","Whites","Asians", "Africans", "bad_health","No_qual","level_1", "level_2")

#check again for correlation
cor.new_data1 <- cor(new_data1, use = "pairwise.complete.obs", method = "spearman")
round(cor.new_data1, digits = 2)
print(cor.new_data1)

corrplot(cor.new_data1, method="color", col=col(200),type="upper",tl.col="black",addCoef.col = "black",number.cex =0.5, tl.srt=90,t1.size=8)


#create new IV data frame

iv_data <- data.frame(new_data1$`0_19`,new_data1$`20_44`,new_data1$`45_64`,new_data1$`65 and above`,new_data1$Whites,new_data1$Asians,new_data1$Africans,new_data1$bad_health,new_data1$No_qual,new_data1$level_1,new_data1$level_2)
colnames(iv_data)<-c ("0_19","20_44", "45_64", "65 and above","Whites","Asians", "Africans", "bad_health","No_qual","level_1", "level_2")

# Kaiser-Meyer-Olkin statistics: if overall MSA > 0.6, proceed to factor analysis
library(psych)
KMO(cor(iv_data))

# Determine Number of Factors to Extract
library(nFactors)

# plot a scree plot of eigenvalues
install.packages("factoextra")
library(factoextra)
eigen.val<- get_eigenvalue(ig.pca)
eigen.val
ig.pca<-prcomp(iv_data,scale = TRUE )
fviz_eig(ig.pca)

library(ggplot2)

# get eigenvalues
ev <- eigen(cor(iv_data))
ev$values
# plot a scree plot of eigenvalues
plot(ev$values, type="b", col="blue", xlab="Variables",main = "Determining no. of factors")

# calculate cumulative proportion of eigenvalue and plot
ev.sum<-0
for(i in 1:length(ev$value)){
  ev.sum<-ev.sum+ev$value[i]
}
ev.list1<-1:length(ev$value)
for(i in 1:length(ev$value)){
  ev.list1[i]=ev$value[i]/ev.sum
}
ev.list2<-1:length(ev$value)
ev.list2[1]<-ev.list1[1]
for(i in 2:length(ev$value)){
  ev.list2[i]=ev.list2[i-1]+ev.list1[i]
}
plot (ev.list2, type="b", col="red", xlab="number of components", ylab ="cumulative proportion")

# Varimax Rotated Principal Components
# retaining 'nFactors' components
library(psych)
fit <- principal(iv_data, nfactors=3, rotate="varimax")
fit

#remove variables that are not required after factor analysis
rem_data <- names(iv_data) %in% c("0_19","20_44","Asians","Africans","level_1","level_2")
iv_data1 <- iv_data[!rem_data]
rm(rem_data)


#testing correlation using cor test

cor.test(final_df$total_deaths, final_df$X0_19, method = "spearman")
cor.test(final_df$total_deaths, final_df$X20_44, method = "spearman")
cor.test(final_df$total_deaths, final_df$X45_64, method = "spearman")
cor.test(final_df$total_deaths, final_df$X65.and.above, method = "spearman")
cor.test(final_df$total_deaths, final_df$Whites, method = "spearman")
cor.test(final_df$total_deaths, final_df$Asians, method = "spearman")
cor.test(final_df$total_deaths, final_df$Africans, method = "spearman")
cor.test(final_df$total_deaths, final_df$others, method = "spearman")
cor.test(final_df$total_deaths, final_df$good_heath, method = "spearman")
cor.test(final_df$total_deaths, final_df$fair_health, method = "spearman")
cor.test(final_df$total_deaths, final_df$bad_health, method = "spearman")
cor.test(final_df$total_deaths, final_df$No_qual, method = "spearman")
cor.test(final_df$total_deaths, final_df$level_1, method = "spearman")
cor.test(final_df$total_deaths, final_df$level_2, method = "spearman")
cor.test(final_df$total_deaths, final_df$level_3, method = "spearman")
cor.test(final_df$total_deaths, final_df$level_4, method = "spearman")
cor.test(final_df$total_deaths, final_df$level_other, method = "spearman")


# correlation circle and cos2 values
library(factoextra)
fviz_pca_var(ig.pca, col.var = "black")
var <- get_pca_var(ig.pca)
head(var$cos2, 11)


fviz_pca_var(ig.pca, col.var = "cos2",alpha.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)

eig.val <- get_eigenvalue(ig.pca)
eig.val

# select variables by excluding those not required; the %in% operator means 'matching'
r.data <- names(iv_data) %in% c("level_2","45_64","0_19","20_44","Africans","Asians")

# the ! operator means NOT
vr <- iv_data[!r.data]
str(vr)
rm(r.data)
colnames(vr)<-c ("65 and above","Whites","bad_health","No_qual","level_1")
#prepare the data
vr <- na.omit(vr) # listwise deletion of missing
boxplot(vr) # visualise the variables
# scale to 0-1
vr <- apply(vr, MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X)))
summary(vr)
boxplot(vr)  # visualise the variables after scaling

#calculating no. of clusters in data frame vik
wss <- (nrow(vr)-1)*sum(apply(vr,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(vr,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# K-Means Cluster Analysis
fit1 <- kmeans(vr, 4) # 4 cluster solution

library(cluster)
clusplot(vr, fit1$cluster, color=TRUE, shade=TRUE,
          lines=0)

# get cluster means
aggregate(vr3, by=list(fit2$cluster),FUN = mean)

# append cluster assignment
mydata2 <- data.frame(mydata2, fit2$cluster)
mydata2
write.csv(mydata2,file.choose())

