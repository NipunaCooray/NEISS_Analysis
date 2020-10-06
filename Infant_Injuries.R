if(!require(here)) { install.packages("here"); require(here)}
set_here()
# NEISS by Harley Wickham is not available in cran, need to get via devtools
if(!require(devtools)) { install.packages("devtools"); require(devtools)}
if(!require(neiss)) { install_github("hadley/neiss"); require(neiss)}
if(!require(dplyr)) { install.packages("dplyr"); require(dplyr)}
if(!require(ggplot2)) { install.packages("ggplot2"); require(ggplot2)}

data(injuries)
names(injuries)

# Each row is a case, i.e. injury. The consumer product(s) implicated in the injury are in prod1 and prod2 as numbers, which can be looked up in another data set, products.
data(products)
names(products)

injuries <- left_join(injuries, products, by = c("prod1" = "code")) %>% 
  rename(product = title)
injuries$product <- as.factor(injuries$product)
injuries %>% group_by(product) %>% summarise(total = sum(weight)) %>% 
  top_n(10, total) %>% arrange(desc(total))

injuries$location <- as.factor(injuries$location)

ageinjury <- injuries %>% 
  group_by( age = as.numeric(cut(age, breaks = (seq(0,100, by = 1))))-1) %>%
  summarise(total = sum(weight))

ageinjurylessone <- injuries %>% filter(injuries$age < 1)

summary(ageinjurylessone)

sexageinjurylessone <- ageinjurylessone %>% 
  group_by(sex, age) %>%
  summarise(total = sum(weight))

ggplot(data = sexageinjurylessone, 
       aes(x = age, y = total, color = sex)) +
  geom_line(size = 1.5, alpha = 0.9) +
  scale_color_manual(values = c("deeppink3", "deepskyblue4","peachpuff4")) + 
  theme(legend.title=element_blank(), legend.justification=c(1,1), legend.position=c(1,1)) +
  ylab("Estimated number of infant injuries") + xlab("Age") + 
  ggtitle("Total Injuries of infants by Age and Sex")


# Locations of infant injuries
whereinjuryunder1 <- ageinjurylessone %>% group_by(location) %>% summarise(total = sum(weight))

ggplot(data = whereinjuryunder1, 
       aes(x = location, y = total)) +
  geom_bar(stat = "identity", fill = "pink", alpha = 0.8) + 
  theme(legend.position="none", axis.title.x = element_blank(),
        axis.text.x= element_text(angle=45, hjust = 1)) +
  ylab("Estimated number of injuries of infants") +
  ggtitle("Location of Injuries of infants")