## the analysis of variance is performed in R by fitting a linear model 
# define factors 
str(Simulated_Data)
picture <- as.factor(picture)
levels(picture) # show levels

# 1 Get descriptives
describeBy(manipulation_check, 
           group = picture)

# 2 Compute the analysis of variance
mod0 <- aov(manipulation_check ~ picture)
summary(mod0)

## significantly different

# 3 post-hoc test  
posthoc<-TukeyHSD(mod0)
posthoc
plot(posthoc)
## "sexy-elderly" and "young-sexy" contrasts are significant, "young-elderly" is not.
```


###Rmarkdown
```{r descriptives, include = FALSE}
descriptives <- describeBy(Simulated_Data$Acceptance, 
           group = Simulated_Data$picture)
```

```{r planned contrast, include  = FALSE}
#Tip: our dataset is unbalanced, since the analysis of variance is performed in R by fitting a linear model 
#created from indicator variables for the levels of the factor.  
#so this validity of this approach does not depend on balance in the data. 

# look at the levels of our factor
levels(picture)
# Defining contrast matrix
##tell R which groups to compare
c1 <- c(0, 1, -1) # sexy vs. young
c2 <- c(-1, 1, 0) # sexy vs. elderly
c3 <- c(-1, 0, 1) # New: young vs. elderly
## combining groups into a matrix
mat <- cbind(c1,c2,c3)

# tell R that the matrix gives the contrasts you want
contrasts(picture) <- mat

#Compute the analysis of variance
model1 <- lm(acceptance ~ picture, Simulated_Data)
summary(model1)

#Summary of the multiple comparisons
sum_plcon <- summary.aov(model1, split=list(picture=list("sexy vs. young"=1, "sexy vs. elderly"=2, "young vs. elderly"=3)), expand.split = TRUE)
sum_plcon

# Report results
p_plcon <- sum_plcon[[1]][, 5] 
fstat_plcon <- sum_plcon[[1]][, 4] 
df_plcon <- sum_plcon[[1]][, 1]

``` 
