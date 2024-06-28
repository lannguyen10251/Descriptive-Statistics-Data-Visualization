library(ggplot2)
library(dplyr)

# Đọc dữ liệu từ file CSV
Dataset_prepared <- read.csv("E:/UEH/KHDL/Dataset_prepared.csv")
#Vẽ chart Year_Birth
Dataset_prepared$Year_Birth <- as.character(Dataset_prepared$Year_Birth)
Dataset_prepared$Year_Birth <- as.numeric(Dataset_prepared$Year_Birth)
ggplot(Dataset_prepared, aes(x = Year_Birth, fill = factor(Response))) +
  geom_histogram(binwidth = 4, position = "dodge") +
  labs(title = "Biểu đồ histogram cho biến Response theo Năm sinh",
       x = "Năm sinh khách hàng",
       y = "Số lượng khách hàng",
       fill = "Response") +
  scale_fill_manual(values = c("0" = "blue", "1" = "pink")) +
  theme_minimal()+
  stat_bin(geom = "text", aes(label = after_stat(count)), vjust = -0.5, 
           binwidth = 4, position = "dodge")
#Chart Education
library(dplyr)
data_education <- Dataset_prepared %>%
  group_by(Response, Education) %>%
  summarize(Count = n()) %>%
  mutate(Response = as.factor(Response))
data_education$Education <- factor(data_education$Education, levels = c("Basic", "Graduation", "2n Cycle","Master", "PhD"))
library(ggplot2)
ggplot(data_education, aes(x = Education, y = Count, fill = Response)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Count), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Biểu đồ cột đôi cho biến Response theo Trình độ học vấn",
       x = "Trình độ học vấn",
       y = "Số lượng") +
  scale_fill_manual(values = c("0" = "blue", "1" = "pink")) +
  theme_minimal()

#Chart Marital_Status
library(dplyr)
data_maritalstatus <- Dataset_prepared %>%
  group_by(Response, Marital_Status) %>%
  summarize(Count = n()) %>%
  arrange(Marital_Status) %>%
  mutate(Response = as.factor(Response))
library(ggplot2)
ggplot(data_maritalstatus, aes(x = Marital_Status, y = Count, fill = Response)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Count), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Biểu đồ cột đôi cho biến Response theo Tình trạng hôn nhân",
       x = "Tình trạng hôn nhân",
       y = "Số lượng") +
  scale_fill_manual(values = c("0" = "blue", "1" = "pink")) +
  theme_minimal()

#Vẽ chart Income
ggplot(Dataset_prepared, aes(x = Income, fill = factor(Response))) +
  geom_histogram(binwidth = 5000, position = "dodge") +
  labs(title = "Biểu đồ histogram cho biến Response theo Thu nhập",
       x = "Thu nhập",
       y = "Số lượng khách hàng",
       fill = "Response") +
  scale_fill_manual(values = c("0" = "blue", "1" = "pink")) +
  theme_minimal()+
  stat_bin(geom = "text", aes(label = after_stat(count)), vjust = -0.5, 
           binwidth = 5000, position = "dodge")

#Chart Kidhome
library(dplyr)
data_kidhome <- Dataset_prepared %>%
  group_by(Response, Kidhome) %>%
  summarize(Count = n()) %>%
  arrange(Kidhome) %>%
  mutate(Response = as.factor(Response))
library(ggplot2)
ggplot(data_kidhome, aes(x = Kidhome, y = Count, fill = Response)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Count), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Biểu đồ cột đôi cho biến Response theo Số lượng trẻ em trong nhà",
       x = "Số lượng trẻ em trong nhà",
       y = "Số lượng") +
  scale_fill_manual(values = c("0" = "blue", "1" = "pink")) +
  theme_minimal()

#Chart Teenhome
library(dplyr)
data_teenhome <- Dataset_prepared %>%
  group_by(Response, Teenhome) %>%
  summarize(Count = n()) %>%
  arrange(Teenhome) %>%
  mutate(Response = as.factor(Response))
library(ggplot2)
ggplot(data_teenhome, aes(x = Teenhome, y = Count, fill = Response)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Count), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Biểu đồ cột đôi cho biến Response theo Số lượng trẻ vị thành niên trong nhà",
       x = "Số lượng trẻ vị thành niên trong nhà",
       y = "Số lượng") +
  scale_fill_manual(values = c("0" = "blue", "1" = "pink")) +
  theme_minimal()


#Vẽ chart Recency
Dataset_prepared$Recency <- as.numeric(Dataset_prepared$Recency)
ggplot(Dataset_prepared, aes(x = Recency, fill = factor(Response))) +
  geom_histogram(binwidth = 6, position = "dodge") +
  labs(title = "Biểu đồ histogram cho biến Response theo Lần mua gần nhất",
       x = "Lần mua gần nhất",
       y = "Số lượng khách hàng",
       fill = "Response") +
  scale_fill_manual(values = c("0" = "blue", "1" = "pink")) +
  theme_minimal()+
  stat_bin(geom = "text", aes(label = after_stat(count)), vjust = -0.5, 
           binwidth = 6, position = "dodge")

#Vẽ chart MntWines
ggplot(Dataset_prepared, aes(x = MntWines, fill = factor(Response))) +
  geom_histogram(binwidth = 140, position = "dodge") +
  labs(title = "Biểu đồ histogram cho biến Response theo Số tiền mua sản phẩm rượu",
       x = "Số tiền mua sản phẩm rượu",
       y = "Số lượng khách hàng",
       fill = "Response") +
  scale_fill_manual(values = c("0" = "blue", "1" = "pink")) +
  theme_minimal()+
  stat_bin(geom = "text", aes(label = after_stat(count)), vjust = -0.5, 
           binwidth = 140, position = "dodge")

#Vẽ chart MntFruits
ggplot(Dataset_prepared, aes(x = MntFruits, fill = factor(Response))) +
  geom_histogram(binwidth = 15, position = "dodge") +
  labs(title = "Biểu đồ histogram cho biến Response theo Số tiền mua sản phẩm trái cây",
       x = "Số tiền mua sản phẩm trái cây",
       y = "Số lượng khách hàng",
       fill = "Response") +
  scale_fill_manual(values = c("0" = "blue", "1" = "pink")) +
  theme_minimal()+
  stat_bin(geom = "text", aes(label = after_stat(count)), vjust = -0.5, 
           binwidth = 15, position = "dodge")

#Vẽ chart MntMeatProducts
ggplot(Dataset_prepared, aes(x = MntMeatProducts, fill = factor(Response))) +
  geom_histogram(binwidth = 80, position = "dodge") +
  labs(title = "Biểu đồ histogram cho biến Response theo Số tiền mua sản phẩm thịt",
       x = "Số tiền mua sản phẩm thịt",
       y = "Số lượng khách hàng",
       fill = "Response") +
  scale_fill_manual(values = c("0" = "blue", "1" = "pink")) +
  theme_minimal()+
  stat_bin(geom = "text", aes(label = after_stat(count)), vjust = -0.5, 
           binwidth = 80, position = "dodge")

#Vẽ chart MntFishProducts
ggplot(Dataset_prepared, aes(x = MntFishProducts, fill = factor(Response))) +
  geom_histogram(binwidth = 20, position = "dodge") +
  labs(title = "Biểu đồ histogram cho biến Response theo Số tiền mua sản phẩm cá",
       x = "Số tiền mua sản phẩm cá",
       y = "Số lượng khách hàng",
       fill = "Response") +
  scale_fill_manual(values = c("0" = "blue", "1" = "pink")) +
  theme_minimal()+
  stat_bin(geom = "text", aes(label = after_stat(count)), vjust = -0.5, 
           binwidth = 20, position = "dodge")

#Vẽ chart MntSweetProducts
ggplot(Dataset_prepared, aes(x = MntSweetProducts, fill = factor(Response))) +
  geom_histogram(binwidth = 15, position = "dodge") +
  labs(title = "Biểu đồ histogram cho biến Response theo Số tiền mua sản phẩm đồ ngọt",
       x = "Số tiền mua sản phẩm đồ ngọt",
       y = "Số lượng khách hàng",
       fill = "Response") +
  scale_fill_manual(values = c("0" = "blue", "1" = "pink")) +
  theme_minimal()+
  stat_bin(geom = "text", aes(label = after_stat(count)), vjust = -0.5, 
           binwidth = 15, position = "dodge")

#Vẽ chart MntGoldProds
ggplot(Dataset_prepared, aes(x = MntGoldProds, fill = factor(Response))) +
  geom_histogram(binwidth = 20, position = "dodge") +
  labs(title = "Biểu đồ histogram cho biến Response theo Số tiền mua sản phẩm vàng",
       x = "Số tiền mua sản phẩm vàng",
       y = "Số lượng khách hàng",
       fill = "Response") +
  scale_fill_manual(values = c("0" = "blue", "1" = "pink")) +
  theme_minimal()+
  stat_bin(geom = "text", aes(label = after_stat(count)), vjust = -0.5, 
           binwidth = 20, position = "dodge")

#Chart NumDealPurchases
library(dplyr)
data_numdealspurchases <- Dataset_prepared %>%
  group_by(Response, NumDealsPurchases) %>%
  summarize(Count = n()) %>%
  arrange(NumDealsPurchases) %>%
  mutate(Response = as.factor(Response))
data_numdealspurchases$NumDealsPurchases <- factor(data_numdealspurchases$NumDealsPurchases , levels = c("0","1","2","3","4","5","6","7","8"))
library(ggplot2)
ggplot(data_numdealspurchases, aes(x = NumDealsPurchases, y = Count, fill = Response)) + 
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Count), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Biểu đồ cột đôi cho biến Response theo Số lần được giảm giá",
       x = "Số lần được giảm giá",
       y = "Số lượng") +
  scale_fill_manual(values = c("0" = "blue", "1" = "pink")) +
  theme_minimal()


#Chart NumWebPurchases
library(dplyr)
data_numwebpurchases <- Dataset_prepared %>%
  group_by(Response, NumWebPurchases) %>%
  summarize(Count = n()) %>%
  arrange(NumWebPurchases) %>%
  mutate(Response = as.factor(Response))
data_numwebpurchases$NumWebPurchases <- factor(data_numwebpurchases$NumWebPurchases , levels = c("0","1","2","3","4","5","6","7","8","9","10","11"))
library(ggplot2)
ggplot(data_numwebpurchases, aes(x = NumWebPurchases, y = Count, fill = Response)) + 
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Count), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Biểu đồ cột đôi cho biến Response theo Số lần mua hàng qua trang web",
       x = "Số lần mua hàng qua trang web",
       y = "Số lượng") +
  scale_fill_manual(values = c("0" = "blue", "1" = "pink")) +
  theme_minimal()

#Chart NumCatalogPurchases
library(dplyr)
data_numcatalogpurchases <- Dataset_prepared %>%
  group_by(Response, NumCatalogPurchases) %>%
  summarize(Count = n()) %>%
  arrange(NumCatalogPurchases) %>%
  mutate(Response = as.factor(Response))
data_numcatalogpurchases$NumCatalogPurchases <- factor(data_numcatalogpurchases$NumCatalogPurchases , levels = c("0","1","2","3","4","5","6","7","8","9"))
library(ggplot2)
ggplot(data_numcatalogpurchases, aes(x = NumCatalogPurchases, y = Count, fill = Response)) + 
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Count), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Biểu đồ cột đôi cho biến Response theo Số lần mua qua danh mục quảng cáo",
       x = "Số lần mua qua danh mục quảng cáo",
       y = "Số lượng") +
  scale_fill_manual(values = c("0" = "blue", "1" = "pink")) +
  theme_minimal()

#Chart NumStorePurchases
library(dplyr)
data_numstorepurchases <- Dataset_prepared %>%
  group_by(Response, NumStorePurchases) %>%
  summarize(Count = n()) %>%
  arrange(NumStorePurchases) %>%
  mutate(Response = as.factor(Response))
data_numstorepurchases$NumStorePurchases <- factor(data_numstorepurchases$NumStorePurchases , levels = c("0","1","2","3","4","5","6","7","8","9","10","11","12","13"))
library(ggplot2)
ggplot(data_numstorepurchases, aes(x = NumStorePurchases, y = Count, fill = Response)) + 
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Count), position = position_dodge(width = 1.2), vjust = -0.5) +
  labs(title = "Biểu đồ cột đôi cho biến Response theo Số lần mua hàng trực tiếp",
       x = "Số lần mua hàng trực tiếp",
       y = "Số lượng") +
  scale_fill_manual(values = c("0" = "blue", "1" = "pink")) +
  theme_minimal()

#Chart NumWebVisitsMonth
library(dplyr)
data_numswebvisitmonth <- Dataset_prepared %>%
  group_by(Response, NumWebVisitsMonth) %>%
  summarize(Count = n()) %>%
  arrange(NumWebVisitsMonth) %>%
  mutate(Response = as.factor(Response))
data_numswebvisitmonth$NumWebVisitsMonth <- factor(data_numswebvisitmonth$NumWebVisitsMonth , levels = c("0","1","2","3","4","5","6","7","8","9","10","11","12","13"))
library(ggplot2)
ggplot(data_numswebvisitmonth, aes(x = NumWebVisitsMonth, y = Count, fill = Response)) + 
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Count), position = position_dodge(width = 1.2), vjust = -0.5) +
  labs(title = "Biểu đồ cột đôi cho biến Response theo Số lần truy cập 
                    vào trang web trong tháng trước",
       x = "Số lần truy cập vào trang web trong tháng trước",
       y = "Số lượng") +
  scale_fill_manual(values = c("0" = "blue", "1" = "pink")) +
  theme_minimal()

