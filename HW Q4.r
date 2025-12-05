data <- read_excel("~/Desktop/experimental_design/FunctionalReach.xlsx")
unique(data$Group)
length(unique(data$Group))
sum(table(data$Group))

# Sum of squares

grand_mean <- mean(data$Response)
SST <- sum((data$Response - grand_mean)^2)
SST

group_means <- tapply(data$Response, data$Group, mean)
group_sizes <- table(data$Group)
SSB <- sum(group_sizes * (group_means - grand_mean)^2)
SSB

print(SST-SSB)

MSB <- SSB / (length(unique(data$Group)) - 1)
MSW <- (SST-SSB) / (sum(table(data$Group)) - length(unique(data$Group)))
MSB
MSW

F <- MSB / MSW
F

# Checking assumptions
model <- aov(Response ~ Group, data = data)
plot(model, which=2)

group_sds <- tapply(data$Response, data$Group, sd)
group_sds
print(max(group_sds) / min(group_sds))
