## Script to Generate 1500 leads for historical data
library(charlatan)

Name <- ch_name(1500)
  
State <- sample(c("Colorado","Indiana","Montana","Idaho"), 1500,replace = TRUE,prob = c(0.1,.3,.4,.2))
StateCoef <- dummy(State) 
sc <- StateCoef %>% as_tibble()


Lead_Source <- sample(c("Social Media","Employee Referral","Advertisement"),1500, replace = TRUE, prob = c(.2,.3,.5))
LSCoef <- dummy(Lead_Source) 
ls <- LSCoef %>% as_tibble()


LogAmount <- rgamma(1500, 200,30)
Amount <- exp(LogAmount)
plot(density(Amount))
plot(density(LogAmount))

Month <- sample(1:12, 1500, replace = TRUE)
iswinter <- ifelse(Month %in% c(11,12,1), 1,0)

## Simulate some data
linpred =-20 + -10*iswinter + 3*LogAmount + 2*sc$StateColorado + -.4*ls$Lead_SourceAdvertisement + sc$StateIdaho + 3*sc$StateIndiana - 10*sc$StateMontana + 4*ls$`Lead_SourceEmployee Referral` + ls$`Lead_SourceSocial Media`


## Simulate the response
pi <- exp(linpred) / (1 + exp(linpred))
Converted <- rbinom(n=length(pi), size=1, prob=pi)

plot(linpred, Converted)
## GLM with some data

m1 <- glm(Converted ~ Amount + Month + State + Lead_Source, family = binomial(link = "logit"))
summary(m1)

m2 <- lm(pi ~ Amount + Month + State + Lead_Source)
summary(m2)

df = tibble(Name, Amount, Month, State, Lead_Source, Converted)
write.csv(df, "Lead_Data.csv", row.names=FALSE)

