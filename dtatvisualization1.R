library(dplyr)
library(echarts4r)

# 读取数据
data <- read.csv("C:\\Users\\李昭阳\\Downloads\\daily-minimum-temperatures-in-me (1).csv")
#查看数据格式
head(data)
#查看是否存在无效行
invalid_dates <- data[is.na(data$Date), ]
print(invalid_dates)
# 删除包含无效日期的行
data <- data[!is.na(data$Date), ]



# 转换日期格式，确保正确读取日期
data$Date <- as.Date(data$Date, format = "%Y-%m-%d")

# 检查是否成功转换日期
if (any(is.na(data$Date))) {
  print("日期转换出错，请检查日期格式")
}

# 提取年份和月份和日期
data$Year <- format(data$Date, "%Y")
data$Month <- format(data$Date, "%m")
data$day <- format(data$Date, "%m%d")

# 删除所有非数字和小数点的字符，并去除空格
data$Daily.minimum.temperatures.in.Melbourne..Australia..1981.1990 <- gsub("[^0-9.]", "", data$Daily.minimum.temperatures.in.Melbourne..Australia..1981.1990)
data$Daily.minimum.temperatures.in.Melbourne..Australia..1981.1990 <- trimws(data$Daily.minimum.temperatures.in.Melbourne..Australia..1981.1990)

# 再次转换为数值类型
data$Daily.minimum.temperatures.in.Melbourne..Australia..1981.1990 <- as.numeric(data$Daily.minimum.temperatures.in.Melbourne..Australia..1981.1990)

# 删除包含NA的行
data <- data %>% filter(!is.na(Daily.minimum.temperatures.in.Melbourne..Australia..1981.1990))

# 按年份和日期汇总每月的最低温度
agg_data <- data %>%
  group_by(Year, day) %>%
  summarise(MinTemperature = mean(Daily.minimum.temperatures.in.Melbourne..Australia..1981.1990, na.rm = TRUE), .groups = "drop")

# 检查agg_data是否为空
print(agg_data)

# 使用 echarts4r 创建3D柱状图
agg_data %>%
  e_charts(day) %>%
  e_bar_3d(Year, MinTemperature, shading = "lambert") %>%
  e_visual_map(MinTemperature, 
               min = 0, 
               max = 20, 
               inRange = list(color = c("#A6C8FF", "#B5E2B2", "#F8E28C", "#FFB3B3", "#F88D8D")), 
               text = c("0°C", "5°C", "10°C", "15°C", "20°C")) %>%  # 使用不包含粉色的渐变色
  e_grid_3d(show = TRUE) %>%
  e_theme("light") %>%  # 设置亮色主题
  e_title("Daily Minimum Temperatures in Melbourne, Australia, 1981-1990") %>%
  e_x_axis_3d(name = "Day") %>%
  e_y_axis_3d(name = "Year") %>%
  e_z_axis_3d(name = "Minimum Temperature", max = 20)  # 设置z轴最大值为20



