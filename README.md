# Customer-Segmentation-using-KMeans-in-R
Customer segmentation is one of unsupervised learning’s most important applications. Employing clustering algorithms to identify the numerous customer subgroups enables businesses to target specific consumer groupings.
![image](https://github.com/LangatErick/Customer-Segmentation-using-KMeans-in-R/assets/124883947/ccc5341c-a0c8-4f5f-bb11-773891694142)

# What is Customer Segmentation?
Customer segmentation is the process of breaking down the customer base into various groups of people that are similar in many ways that are important to marketing, such as gender, age, interests, and various spending habits.

Companies that use customer segmentation operate under the premise that each customer has unique needs that must be addressed through a particular marketing strategy. Businesses strive to develop a deeper understanding of the customers they are aiming for. Therefore, they must have a clear objective and should be designed to meet the needs of every single customer. A deeper understanding of client preferences and the criteria for identifying profitable segments can also be gained by businesses through the data collected.

## How to Implement Customer Segmentation in R?

In the first step of this data science project, we will perform data exploration. We will import the essential packages required for this role and then read our data. Finally, we will go through the input data to gain necessary insights about it.

```{r warning=FALSE, message=FALSE}
#import libraries
library(tidyverse)
library(janitor)
library(ggplot2)
# library()
```

```{r warning=FALSE, message=FALSE}
#import dataset
customer_data <-read_csv("Mall_Customers.csv") %>% clean_names()
names(customer_data)
```
![image](https://github.com/LangatErick/Customer-Segmentation-using-KMeans-in-R/assets/124883947/b44aa79b-5bee-4c34-a571-ffc145a0a11d)

```{r warning=FALSE, message=FALSE}
#check duplicate rows
sum(duplicated(customer_data$customer_id))
#data structure
glimpse(customer_data)
```
![image](https://github.com/LangatErick/Customer-Segmentation-using-KMeans-in-R/assets/124883947/72cb9c19-9bdb-445a-ae93-b9f47e8b5cd4)

```{r warning=FALSE, message=FALSE}
#convert gender to a factor variable
customer_data$gender <- factor(customer_data$gender)
levels(customer_data$gender)
```

We will now display the first six rows of our dataset using the head() function and use the summary() function to output summary of it.

```{r warning=FALSE, message=FALSE}
head(customer_data)
```
![image](https://github.com/LangatErick/Customer-Segmentation-using-KMeans-in-R/assets/124883947/a9e6972d-69b8-407c-99b3-124e20e2c497)

```{r warning=FALSE, message=FALSE}
#check summary
summary(customer_data[,-1]) #%>% DT::datatable()
```
![image](https://github.com/LangatErick/Customer-Segmentation-using-KMeans-in-R/assets/124883947/e21a374e-283d-4ace-afe2-694706ca2440)

### Customer Gender Visualization

In this, we will create a barplot and a piechart to show the gender distribution across our customer_data dataset.

```{r warning=FALSE, message=FALSE}
# barplot(
#   table(customer_data$gender),
#   col = rainbow(2),
#  )
theme_set(theme_test())
plotdata <- customer_data %>% count(gender)

plotdata %>% 
  ggplot(aes(x=gender, y=n)) +
  geom_bar(stat = 'identity', fill=rainbow(2))+
  geom_text(aes(label=n))+
  theme(axis.text.x = element_text(
    angle = 45,
    hjust = 1
  ))
```
![image](https://github.com/LangatErick/Customer-Segmentation-using-KMeans-in-R/assets/124883947/f4621807-cdaf-4eac-8617-9dc545a1325f)

From the above bar-plot, we observe that the **number of females** is **higher** than the **males**.

```{r warning=FALSE, message=FALSE}
d <- customer_data %>% tabyl(gender) %>% 
              adorn_pct_formatting() %>% 
              as_tibble() 
d
```
![image](https://github.com/LangatErick/Customer-Segmentation-using-KMeans-in-R/assets/124883947/7e0f5d28-358c-425f-a1d0-54faae41d069)

Now, let us visualize a chart to observe the ratio of male and female distribution.

```{r warning=FALSE, message=FALSE}
d %>% 
  ggplot(aes(x=gender, y=n))+
  geom_bar(stat = 'identity', fill=rainbow(2)) +
  geom_text(aes(label=percent))+
  #geom_text(aes(label=n))+
  xlab(" " )
```
![image](https://github.com/LangatErick/Customer-Segmentation-using-KMeans-in-R/assets/124883947/cf3751b9-64f3-465a-ba34-2b350fe1d248)

From the above graph, we conclude that the percentage of females is **56%**, whereas the percentage of males in the customer dataset is **44%**.

### Visualization of Age Distribution

Let us plot a histogram to view the distribution to plot the frequency of customer ages. We will first proceed by taking a summary of the Age variable.

```{r warning=FALSE, message=FALSE}
#Histogram of Ages
hist(customer_data$age,
    col=rainbow(11),
    main="Histogram to Show Count of Age Class",
    xlab="Age Class",
    ylab="Frequency",
    labels=TRUE) 
```
![image](https://github.com/LangatErick/Customer-Segmentation-using-KMeans-in-R/assets/124883947/4a5f1481-d9a1-4b51-9f41-0e0e8de5e92b)
```{r warning=FALSE, message=FALSE}
#barplot
# boxplot(customer_data$age,
#        col="deepskyblue",
#        main="Boxplot for Descriptive Analysis of Age")
# IQR(customer_data$age)
# quantile(customer_data$age, 1/4)
# quantile(customer_data$age, 2/4)
# quantile(customer_data$age, 3/4)
customer_data %>%  
  ggplot(aes(age))+
  geom_boxplot(fill="blue")
```
![image](https://github.com/LangatErick/Customer-Segmentation-using-KMeans-in-R/assets/124883947/45eb2987-aeeb-415b-aa02-aa6838a0bf5a)
From the above two visualizations, we conclude that the maximum customer ages are between 30 and 35. The minimum age of customers is 18, whereas, the maximum age is 70.
### Analysis of the Annual Income of the Customers
In this section of the R project, we will create visualizations to analyze the annual income of the customers. We will plot a histogram and then we will proceed to examine this data using a density plot.
```{r warning=FALSE, message=FALSE}
#Histogram
library(patchwork)
p1 <- customer_data %>% 
  ggplot(aes(annual_income_k)) +
  geom_density(fill='indianred1',
               bw=5)+
  ggtitle('Density Plot for Annual Income')

p2 <- customer_data %>% 
  ggplot(aes(annual_income_k))+
  geom_histogram(binwidth = 5,
                 fill='indianred2',
                 col='blue',
                 )+
  ylab('')+
  ggtitle(' Histogram for Annual income')

p1+p2
```
![image](https://github.com/LangatErick/Customer-Segmentation-using-KMeans-in-R/assets/124883947/2c71914b-ff60-4be7-b0f3-0a4d0e1bb90c)

From the above descriptive analysis, we conclude that the minimum annual income of the customers is 15 and the maximum income is 137. People earning an average income of 70 have the highest frequency count in our histogram distribution. The average salary of all the customers is 60.56. In the Kernel Density Plot that we displayed above, we observe that the annual income has a normal distribution.

## Analyzing the Spending Score of the Customers
```{r warning=FALSE, message=FALSE}
summary(customer_data$spending_score_1_100)
```
![image](https://github.com/LangatErick/Customer-Segmentation-using-KMeans-in-R/assets/124883947/a7f5a873-d9c2-4a38-9b62-b19c0e13af6e)

```{r warning=FALSE, message=FALSE}
#boxplot
d1 <- customer_data %>% 
  ggplot(aes(spending_score_1_100))+
  geom_boxplot(fill='deepskyblue',
               col='orange')+
  geom_vline(xintercept = 50.2,#mean.
             col='red')+
  geom_vline(xintercept =34.75,#1st Qu.
             col='yellow')+
  geom_vline(xintercept =73,#3rd Qu.
             col='darkorange'
               )
#Histogram

d2 <- customer_data %>% 
  ggplot(aes(spending_score_1_100))+
  geom_histogram(binwidth=11,
                 fill='skyblue',
                 col='red')+
   labs(
     x='spending',
     y='count',
     title = 'Histogram of Spending Score',
     subtitle = 'customer spending habit'
   )

d1+d2
```
![image](https://github.com/LangatErick/Customer-Segmentation-using-KMeans-in-R/assets/124883947/77148ed6-2590-48ba-a331-473a29b7e263)

The minimum spending score is 1, maximum is 99 and the average is 50.20. We can see Descriptive Analysis of Spending Score is that Min is 1, Max is 99 and avg. is 50.20. From the histogram, we conclude that customers between classes 40 and 50 have the highest spending score among all the classes.

# K-means Algorithm

While using the k-means clustering algorithm, the first step is to indicate the number of clusters (k) that we wish to produce in the final output. The algorithm starts by selecting k objects from dataset randomly that will serve as the initial centers for our clusters. These selected objects are the cluster means, also known as centroids. Then, the remaining objects have an assignment of the closest centroid. This centroid is defined by the Euclidean Distance present between the object and the cluster mean. We refer to this step as **“cluster assignment”**. When the assignment is complete, the algorithm proceeds to calculate new mean value of each cluster present in the data. After the recalculation of the centers, the observations are checked if they are closer to a different cluster. Using the updated cluster mean, the objects undergo reassignment. This goes on repeatedly through several iterations until the cluster assignments stop altering. The clusters that are present in the current iteration are the same as the ones obtained in the previous iteration.

### Summing up the K-means clustering –

-   We specify the number of clusters that we need to create.

-   The algorithm selects k objects at random from the dataset. This object is the initial cluster or mean.

-   The closest centroid obtains the assignment of a new observation. We base this assignment on the Euclidean Distance between object and the centroid.

-   k clusters in the data points update the centroid through the calculation of the new mean values present in all the data points of the cluster. The kth cluster’s centroid has a length of p that contains means of all variables for observations in the k-th cluster. We denote the number of variables with p.

-   Iterative minimization of the total within the sum of squares. Then through the iterative minimization of the total sum of the square, the assignment stop wavering when we achieve maximum iteration. The default value is 10 that the R software uses for the maximum iterations.

### Determining Optimal Clusters

While working with clusters, you need to specify the number of clusters to use. You would like to utilize the optimal number of clusters. To help you in determining the optimal clusters, there are three popular methods –**but we're going to utilize `Gap Statistic method`**

-   Elbow method

-   Silhouette method

-   Gap statistic

#### Gap Statistic Method

In 2001, researchers at Stanford University – **R. Tibshirani, G.Walther and T. Hastie** published the Gap Statistic Method. We can use this method to any of the clustering method like K-means, hierarchical clustering etc. Using the gap statistic, one can compare the total intracluster variation for different values of k along with their expected values under the null reference distribution of data. With the help of **Monte Carlo simulations**, one can produce the sample dataset. For each variable in the dataset, we can calculate the range between min(xi) and max (xj) through which we can produce values uniformly from interval lower bound to upper bound.

For computing the gap statistics method we can utilize the clusGap function for providing gap statistic as well as standard error for a given output.

```{r warning=FALSE, message=FALSE}
library(cluster)
set.seed(169)
nclust <- clusGap(customer_data[,3:5], 
                  FUN = kmeans, 
                  K.max = 7, B = 50)
library(factoextra)
fviz_gap_stat(nclust)

```
![image](https://github.com/LangatErick/Customer-Segmentation-using-KMeans-in-R/assets/124883947/c79fc51b-bb13-41e0-ba24-b92a80be22fe)

```{r warning=FALSE, message=FALSE}
k6 <- kmeans(customer_data[,3:5], 
             centers = 6, 
             iter.max = 10, 
             nstart = 1,
       algorithm = "Lloyd",
       trace = FALSE)
k6$centers
```
![image](https://github.com/LangatErick/Customer-Segmentation-using-KMeans-in-R/assets/124883947/e5b92a57-e1f0-4630-9397-7aa2bd84df0a)

In the output of our kmeans operation, we observe a list with several key pieces of information. From this, we conclude the useful information being –

-   **cluster –** This is a vector of several integers that denote the cluster which has an allocation of each point.

-   **totss –** This represents the total sum of squares.

-   **centers –** Matrix comprising of several cluster centers

-   **withinss –** This is a vector representing the intra-cluster sum of squares having one component per cluster.

-   **tot.withinss –** This denotes the total intra-cluster sum of squares.

-   **betweenss –** This is the sum of between-cluster squares.

-   **size –** The total number of points that each cluster holds.

# Visualizing the Clustering Results using the First Two Principle Components
```{r warning=FALSE, message=FALSE}
pcclust=prcomp(customer_data[,3:5],scale=FALSE) #principal component analysis
summary(pcclust)
pcclust$rotation[,1:2]

customer_data %>% 
  ggplot(aes(x=spending_score_1_100, 
             y=annual_income_k, 
             )) +
  
  geom_point(
    stat = 'identity',
   aes(color = as.factor(k6$cluster))
  )+
  scale_color_discrete(name=" ",
              breaks=c("1", "2", "3", "4", "5", "6"),
              labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5", "Cluster 6")) +
  
  ggtitle("Segments of Mall Customers", subtitle = "Using K-means Clustering")
```
![image](https://github.com/LangatErick/Customer-Segmentation-using-KMeans-in-R/assets/124883947/8fe681cc-b677-494d-ac52-f0b315139fc9)

From the above visualization, we observe that there is a distribution of 6 clusters as follows –

**Cluster 6 and 4 –** These clusters represent the customer_data with the medium income salary as well as the medium annual spend of salary.

**Cluster 1 –** This cluster represents the customer_data having a high annual income as well as a high annual spend.

**Cluster 3 –** This cluster denotes the customer_data with low annual income as well as low yearly spending of income.

**Cluster 2 –** This cluster denotes a high annual income and low yearly spending.

**Cluster 5 –** This cluster represents a low annual income but a high yearly expenditure.

```{r}
kCols=function(vec){cols=rainbow (length (unique (vec)))
return (cols[as.numeric(as.factor(vec))])}

digCluster<-k6$cluster; dignm<-as.character(digCluster); # K-means clusters

plot(pcclust$x[,1:2], col =kCols(digCluster),pch =19,xlab ="K-means",ylab="classes")
legend("bottomleft",unique(dignm),fill=unique(kCols(digCluster)))
```
![image](https://github.com/LangatErick/Customer-Segmentation-using-KMeans-in-R/assets/124883947/7057388a-565c-49fd-b9ce-224b1efe70bc)

# Summary
In this data science project, we went through the customer segmentation model. We developed this using
a class of machine learning known as unsupervised learning. Specifically, we made use of a clustering
algorithm called K-means clustering. We analyzed and visualized the data and then proceeded to implement
our algorithm. Hope you enjoyed this customer segmentation project of machine learning using R.
