data = read.csv("C:\\Users\\Mohammed\\Downloads\\Game Rankings - GameFAQs.csv")
df0 = data.frame(data)
summary(df0) # NA's present
df = na.omit(df0) # Omit NA values

# Performing regression
X = df$Length..Hours.
Y = df$Rating...5
model = lm(Y~X, data=df)
summary(model)

# Scatter plot with Regression Line
ggplot(df, aes(x=Length..Hours.,y=Rating...5)) + geom_point() + geom_smooth(method='lm', formula= y~x)
# Distribution of Residuals
ggplot(data = data.frame(model$residuals), aes(x=model.residuals)) + geom_histogram()
# Residual Plot
ggplot(data = data.frame(model$residuals, model$fitted.values), 
       aes(x=model.fitted.values, y=model.residuals)) + 
  geom_point() + geom_hline(yintercept = 0, linetype="dashed", color = "red")

# Performing regression after removing outliers
out = boxplot(Y)$out
Xo = head(X,521)
Yo = head(Y,521)
dfout = data.frame(Xo,Yo)
model2 = lm(Yo~Xo, dfout)
summary(model2)
plot(Xo,Yo, xlab = "Length in Hours", ylab = "Rating / 5")
abline(model2$coefficients, col='red')
