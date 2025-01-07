# get operating character result from oc.postprob

oc <- ocPostprob(
  nnE = c(10, 20, 30), truep = 0.4, p0 = 0.2,
  p1 = 0.3, tL = 0.6, tU = 0.8, parE = c(1, 1), sim = 100, wiggle = FALSE
)
oc$oc
# plotOc(oc)


oc <- ocPostprob(
  nnE = c(10, 20, 30), truep = 0.4, p0 = 0.2,
  p1 = 0.3, tL = 0.6, tU = 0.8, parE = c(1, 1), sim = 100, wiggle = TRUE
)
oc$oc

data <- data.frame(Decision = as.factor(oc$Decision),
           Looks = oc$SampleSize)

table(data$Decision, data$Looks)

data %>% group_by(Decision, Looks) %>% summarise(n = n())

data %>% group_by(SampleSize) %>% summarise(n = n())

ggplot(dat, aes(x = SampleSize, y = Decision)) +
  geom_bar(stat = "identity")

data %>% group_by(Looks, Decision) %>% summarise(tot = n())


table(oc$Decision, oc$SampleSize) / oc$params$sim
table(oc$Decision, oc$SampleSize)

oc$Decision <- ifelse(oc$Decision == TRUE, "GO",
                      ifelse(oc$Decision == FALSE, "Stop", "Grey"))

oc$Decision

data.frame(looks = c(res1$wiggled_nnrE))

table(oc$Decision, oc$SampleSize)

tt = data.frame(Decision = res$Decision,
                look = res$SampleSize)
tt %>% group_by(look) %>% summarise(success = per())
