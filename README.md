

# myshinyApp is based on the following research question:


Does vaccination status affect mask-wearing behavior(Criticize_Mask)?

The app incorporates Chi-square Test and Fisher's Exact Test to assess the relationship between vaccination status and mask-wearing behavior.

Here, Null hypothesis: Vaccination status and mask-wearing behavior are independent.
Alternative hypothesis: Vaccination status and mask-wearing behavior are not independent.

Test Results:
Chi-square Test: The Chi-square test result indicates a p-value=1, which means we fail to reject the null hypothesis. This suggests that there is no statistically significant relationship between vaccination status and mask-wearing behavior.
Fisher's Exact Test: Fisher's Exact test also produces a p-value=1, reinforcing the conclusion that there is no evidence of a significant association between these two variables.

Graphical Presenation:
Bar chart: Displays the count of vaccinated and non-vaccinated individuals categorized by mask wearing behavior.
Density Plot: Illustrate the probability distribution of vaccinated individuals in relation to their mask-wearing behavior.
Scatter plot: Provides a point-based visualization of the relationship between vaccination and mask-wearing criticism.


Conclusion: 
The statistical tests and graphical representation suggest that vaccination status does not significantly impact mask-wearing behavior. This means that individuals decisions to wear masks are independent of their vaccination status. However, further investigation with a larger dataset or additional factors could provide more insights into behavioral patterns.


Instructions:

install.packages(c("shiny","DT","MASS","ggplot2","rsconnect","randomForest","dplyr"))

Run these library into R-console

library(shiny)
library(DT)
library(randomForest)
library(MASS)
library(ggplot2)
library(rsconnect)
library(dplyr)


# Type the following code into R-console for setting the working directory

setwd("C:/Dr. Chang/www/myshinyApp") and then press Control +Enter (Note: everybody should have individual path my path is here : "C:/Dr. Chang/www/myshinyApp")

# Check the working directory into R-console. Type the following code into "R console"


getwd() and then press control+Enter



# Run into the local machine before deploy

shiny::runApp()

# Connect the RStudio with Deploy into R-console


rsconnect: :deployApp()


# From GitHub page, if we want to run my "R-shiny" then type the following "R code" into the "R-console"

shiny::runGitHub("myshinyApp", "bdatta2021") and then press control+Enter.


