library(sf)
geodata <- st_read("World_Countries_(Generalized)_9029012925078512962.geojson")
library(readxl)
excel_data <- read_excel("hdr-data.xlsx")

# 检查每个国家是否有2010和2019的数据
inequality_data <- excel_data %>%
  filter(year %in% c(2010, 2019)) %>%
  group_by(country) %>%
  filter(n() == 2) %>% # 确保每个国家有2010和2019两行数据
  ungroup()
library(dplyr)

pre_process_data1 <-excel_data %>% filter(year==2010) 
pre_process_data1 <- pre_process_data1 %>% select('countryIsoCode','year','value')
pre_process_data2 <- excel_data %>% filter(year==2019)

merged_table <- pre_process_data2 %>%
  left_join(pre_process_data1, by = "countryIsoCode")
colnames(merged_table)
merged_table <- merged_table %>% select('countryIsoCode','country','year.x','value.x',
                                        'year.y','value.y')
res <- merged_table %>% mutate(difference=value.x-value.y)
res <- res %>% rename('COUNTRY'='country')
res <- res %>% select('COUNTRY','difference')
res2 <- geodata %>%
  left_join(res, by = "COUNTRY")
library(ggplot2)

# 绘制地图，按不平等指数着色
ggplot(res2) +
  geom_sf(aes(fill = difference)) +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(title = "Map with Inequality Difference")

st_write(res2,'world_inequlityDifference.geojson')
