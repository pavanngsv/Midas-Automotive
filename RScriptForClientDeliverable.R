# USEFUL PACKAGES
library(tidyverse)
library(causact)

# LOAD DATA
carsDF = readr::read_csv("/Users/pavankumarn/Documents/WorkingDirectory/carsFixed.csv")
# VIEW QUICK SUMMARY OF DATA
carsDF %>%
  group_by(shopID) %>%
  summarize(numberOfObservations = n(),
            numberOfBossVisits = sum(boss))

# CREATE GRAPHICAL MODEL
graph = dag_create() %>%
  dag_node("Cars Fixed","K",
           data = carsDF$carsFixed,
           rhs = poisson(rate)) %>%
  dag_node("Exp Cars Fixed - Shop Level","rate",
           rhs = exp(alpha_shop + beta_shop * x),
           child = "K") %>%
  dag_node("Intercept - Shop Level","alpha_shop",
           rhs = normal(alpha,sigma_alpha),
           child = "rate") %>%
  dag_node("Boss Effect - Shop Level","beta_shop",
           rhs = normal(beta,sigma_beta),
           child = "rate") %>%
  dag_node("Intercept - Midas Level","alpha",
           rhs = normal(3,2),
           child = "alpha_shop") %>%
  dag_node("Std Dev - Midas Level","sigma_alpha",
           rhs = uniform(0,2),
           child = "alpha_shop") %>%
  dag_node("Exp Boss Effect - Midas Level","beta",
           rhs = normal(0,1),
           child = "beta_shop") %>%
  dag_node("Std Dev Boss Effect","sigma_beta",
           rhs = uniform(0,2),
           child = "beta_shop") %>%
  dag_node("Boss Present","x",
           data = carsDF$boss,
           child = "rate") %>%
  dag_plate("Observation","i",
            nodeLabels = c("K","rate","x")) %>%
  dag_plate("Shop","j",
            nodeLabels = c("beta_shop","alpha_shop"),
            data = carsDF$shopID,
            addDataNode = TRUE)

# DISPLAY GRAPHICAL MODEL
graph %>% dag_render()

# GET POSTERIOR DISTRIBUTION
drawsDF = graph %>% dag_numpyro(seed = 1234567)

drawsDF %>% dagp_plot()

#########################################################################################################################################
#CODE FOR FIRST GRAPH
#########################################################################################################################################
#create a new column according to boss values
carsDF$Boss_Presence <- ifelse(carsDF$boss == 1, "Available", "Unavailable")

#Add labels to each shop
shopID.labs <- c("Shop 1", "Shop 2", "Shop 3","Shop 4","Shop 5")
names(shopID.labs) <- c("1", "2", "3","4","5")

#Applying colors to the values created above
valuesNeedingColors = unique(carsDF$Boss_Presence)
colorList = c("gray",
              "orange")
names(colorList) = valuesNeedingColors


graph1 = carsDF %>% ggplot(aes(x = observation, y = carsFixed, fill=Boss_Presence)) +
  geom_bar(stat = "identity") + 
  facet_wrap(~shopID, ncol = 1, labeller=labeller(shopID=shopID.labs)) + 
  scale_fill_manual(values=colorList) +
  labs(title = "Cars Fixed Over the Last 10 Weeks",subtitle = "Effect of Patrick's Presence on Car Fixing", x = "Days", y = "Number of Cars Fixed")+ 
  guides(fill=guide_legend(title="Patrick's Availability")) +
  theme_minimal(15)

graph1

ggsave("FD_Graph1.pdf",height=10, width=15)

#########################################################################################################################################
#CODE FOR SECOND GRAPH
#########################################################################################################################################

carsDF1 <- carsDF %>% group_by(shopID) %>% 
  summarize(q05_1 = stats::quantile(postDF$shop_1,0.05),
            q50_1 = stats::quantile(postDF$shop_1,0.50),
            q95_1 = stats::quantile(postDF$shop_1,0.95),
            q05_2 = stats::quantile(postDF$shop_2,0.05),
            q50_2 = stats::quantile(postDF$shop_2,0.50),
            q95_2 = stats::quantile(postDF$shop_2,0.95),
            q05_3 = stats::quantile(postDF$shop_3,0.05),
            q50_3 = stats::quantile(postDF$shop_3,0.50),
            q95_3 = stats::quantile(postDF$shop_3,0.95),
            q05_4 = stats::quantile(postDF$shop_4,0.05),
            q50_4 = stats::quantile(postDF$shop_4,0.50),
            q95_4 = stats::quantile(postDF$shop_4,0.95),
            q05_5 = stats::quantile(postDF$shop_5,0.05),
            q50_5 = stats::quantile(postDF$shop_5,0.50),
            q95_5 = stats::quantile(postDF$shop_5,0.95))

data1 = carsDF1 %>% select(shopID,q05_1,q50_1,q95_1) %>% filter(shopID == 1) %>% mutate(maxexpense = (ceiling(q95_1)*100)) %>% mutate(minexpense = (floor(q05_1)*100))
data2 = carsDF1 %>% select(shopID,q05_2,q50_2,q95_2) %>% filter(shopID == 2) %>% mutate(maxexpense = (ceiling(q95_2)*100)) %>% mutate(minexpense = (floor(q05_2)*100))
data3 = carsDF1 %>% select(shopID,q05_3,q50_3,q95_3) %>% filter(shopID == 3) %>% mutate(maxexpense = (ceiling(q95_3)*100)) %>% mutate(minexpense = (floor(q05_3)*100))
data4 = carsDF1 %>% select(shopID,q05_4,q50_4,q95_4) %>% filter(shopID == 4) %>% mutate(maxexpense = (ceiling(q95_4)*100)) %>% mutate(minexpense = (floor(q05_4)*100))
data5 = carsDF1 %>% select(shopID,q05_5,q50_5,q95_5) %>% filter(shopID == 5) %>% mutate(maxexpense = (ceiling(q95_5)*100)) %>% mutate(minexpense = (floor(q05_5)*100))


mainDF = bind_rows(data1, data2,data3,data4,data5)  

annotations <- data.frame(
  shopID = c(3),
  annotation_text = c("Shop managed by Micheal - Patrick's Brother")
)

# separate out caption for readability of code
plotCaption = 
  "Average Additional Cars Fixed (dark point) within 90% range of possible values \n *Assume revenue per car fixed is $100"

carsDF %>% mutate(revenue_per_day = carsDF$carsFixed*100)

mainDF%>% 
  ggplot(aes(y=shopID)) + 
  geom_linerange(aes(xmin = q05_1, xmax= q95_1), size = 5, color = "lightsteelblue3") + 
  geom_point(aes(x = q50_1), size = 7, color = "darkorange3") + 
  geom_linerange(aes(xmin = q05_2, xmax= q95_2), size = 5, color = "lightsteelblue3") + 
  geom_point(aes(x = q50_2), size = 7, color = "darkorange3") + 
  geom_linerange(aes(xmin = q05_3, xmax= q95_3), size = 5, color = "lightsteelblue3") + 
  geom_point(aes(x = q50_3), size = 7, color = "darkorange3") + 
  geom_text(data = annotations, aes(x = 1.2, y=3.1, label = annotation_text),
            vjust = -0.5, hjust = 0, size = 5, color = "red") +
  geom_linerange(aes(xmin = q05_4, xmax= q95_4), size = 5, color = "lightsteelblue3") + 
  geom_point(aes(x = q50_4), size = 7, color = "darkorange3") + 
  geom_linerange(aes(xmin = q05_5, xmax= q95_5), size = 5, color = "lightsteelblue3") + 
  geom_point(aes(x = q50_5), size = 7, color = "darkorange3") + 
  labs(title = "Expected Additional Cars Fixed when Patrick is Present", x = "Expected Additional Cars Fixed", y = "Shops", caption = plotCaption) +
  scale_x_continuous(limits = c(0,22), breaks = seq(0, 22, by = 2)) +
  geom_text(aes(x = 18, label = paste0("Expected Addtional Revenue = " , scales::dollar(minexpense), " to ", scales::dollar(maxexpense))), vjust = 0.5, hjust = 0, size = 5, color = "red") +
  theme_minimal(17)

ggsave("FD_Graph2.pdf",height=12, width=20)
