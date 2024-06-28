# Đọc dữ liệu từ file CSV
Dataset_prepared <- read.csv("E:/UEH/KHDL/Dataset_prepared.csv")
######Thống kê mô tả cấu trúc dữ liệu#####
# Thêm thư viện dplyr
library(dplyr)

# Chia lại năm sinh theo thập kỷ từ 1930 đến 1990
Dataset_prepared$Year_Birth <- cut(Dataset_prepared$Year_Birth, 
                                   breaks = c(1930, 1940, 1950, 1960, 1970, 1980, 1990, 2000),
                                   labels = c("1930s", "1940s", "1950s", "1960s", "1970s", "1980s", "1990s"))

# Chia lại thu nhập thành 2 nhóm: trên 10000 và dưới 10000
Dataset_prepared$Income <- factor(ifelse(Dataset_prepared$Income > 10000, "Over 10000", "Under 10000"))

# Tăng giá trị của các cột MntWines, MntFruits, MntMeatProducts, MntFishProducts, MntSweetProducts, MntGoldProds lên bằng giá trị hiện tại cộng thêm 10000
Dataset_prepared[c("MntWines", "MntFruits", "MntMeatProducts", "MntFishProducts", "MntSweetProducts", "MntGoldProds")] <- lapply(Dataset_prepared[c("MntWines", "MntFruits", "MntMeatProducts", "MntFishProducts", "MntSweetProducts", "MntGoldProds")], function(x) x + 10000)

# Tính phần trăm các loại thành phần cho Recency
Dataset_prepared$Recency <- cut(Dataset_prepared$Recency, 
                                breaks = c(0, 30, 60, Inf), 
                                labels = c("1 - 30 ngày", "30 - 60 ngày", "trên 3 tháng")) 

# Các thuộc tính cần tính phần trăm
attributes_to_process <- c("Kidhome", "Teenhome", "Response", "NumDealsPurchases", "NumWebPurchases", "NumCatalogPurchases", "NumStorePurchases", "NumWebVisitsMonth")

# Tạo một dataframe để lưu dữ liệu về phần trăm các loại thành phần
result_df <- data.frame()

## Lặp qua từng cột trong Dataset_prepared
for (col_name in colnames(Dataset_prepared)) {
  # Kiểm tra điều kiện để tính phần trăm các loại thành phần
  if (!is.numeric(Dataset_prepared[[col_name]]) || col_name %in% attributes_to_process) {
    percentages <- prop.table(table(Dataset_prepared[[col_name]])) * 100
    
    # Tạo dataframe chứa thông tin về cột thuộc tính, thành phần và phần trăm
    temp_df <- data.frame(
      attribute = col_name,
      component = as.character(names(percentages)),
      percentage = as.numeric(percentages)
    )
    
    # Gộp dữ liệu vào result_df
    result_df <- bind_rows(result_df, temp_df)
  } else if (col_name %in% c("MntWines", "MntFruits", "MntMeatProducts", "MntFishProducts", "MntSweetProducts", "MntGoldProds")) {
    # Tính phần trăm các loại thành phần sau khi tăng giá trị của các cột lên bằng giá trị hiện tại cộng thêm 10000
    labels <- paste("$", seq(10000, 100000, by = 10000), "-$", seq(20000, 110000, by = 10000))
    percentages <- prop.table(table(cut(Dataset_prepared[[col_name]], breaks = 10, labels = labels))) * 100
    
    # Tạo dataframe chứa thông tin về cột thuộc tính, thành phần và phần trăm
    temp_df <- data.frame(
      attribute = col_name,
      component = as.character(names(percentages)),
      percentage = as.numeric(percentages)
    )
    
    # Gộp dữ liệu vào result_df
    result_df <- bind_rows(result_df, temp_df)
  }
}
print(result_df) 

######Thống kê mô tả tổng hợp các thuộc tính của dữ liệu#####

# Mã hóa các thành phần trong biến Education thành số
Dataset_prepared$Education <- as.numeric(factor(Dataset_prepared$Education, levels = unique(Dataset_prepared$Education)))

# Mã hóa các thành phần trong biến Marital_Status thành số
Dataset_prepared$Marital_Status <- as.numeric(factor(Dataset_prepared$Marital_Status, levels = unique(Dataset_prepared$Marital_Status)))

# Lấy chỉ số của các cột số (numeric)
numeric_columns <- sapply(Dataset_prepared, is.numeric)

# Lọc các cột số
numeric_dataset <- Dataset_prepared[, numeric_columns]

# Tính min, mean, median, max, và độ lệch chuẩn của các cột số trong Dataset_prepared
summary_stats <- data.frame(
  Attribute = names(numeric_dataset),
  Min = round(sapply(numeric_dataset, min, na.rm = TRUE), 2),
  Mean = round(sapply(numeric_dataset, mean, na.rm = TRUE), 2),
  Median = round(sapply(numeric_dataset, median, na.rm = TRUE), 2),
  Max = round(sapply(numeric_dataset, max, na.rm = TRUE), 2),
  SD = round(sapply(numeric_dataset, sd, na.rm = TRUE), 2)
)

print(summary_stats)