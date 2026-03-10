#先处理基线数据
library(haven)
background2011 <- read_dta("~/Desktop/科研数据/CHARLS全数据/2011/household_and_community_questionnaire_data/demographic_background.dta")

library(dplyr)
wave1 <- background2011 %>%
  select(ID,householdID,communityID,ba002_1,ba002_2,ba002_3,ba003,bc001,bd001,bd007s1,
         bd007s2,bd007s3,bd007s4,bd007s5,bd007s6,bd007s7,bd007s8,be001,bf004)
setwd("~/Desktop/自己的科研：极端气温和老年人认知")

wave1 <- wave1 %>%
  rename(
    Birth_Year = ba002_1,
    Birth_Month = ba002_2,
    Birth_Day = ba002_3
  )

#因为有很多老年人用农历生日，因此简单的解决办法是把所有月份往后一个月
wave1 <- wave1 %>%
  mutate(
    Birth_Year = case_when(
      ba003 == 2 & Birth_Month == 12 ~ Birth_Year + 1,   # 阴历 + 月份为12 → 跨年
      TRUE ~ Birth_Year
    ),
    Birth_Month = case_when(
      ba003 == 2 & Birth_Month == 12 ~ 1,                # 阴历 + 月份为12 → 月设为1
      ba003 == 2 ~ Birth_Month + 1,                      # 其他阴历 → 月 +1
      TRUE ~ Birth_Month                                 # 阳历 → 不变
    )
  )
#综合考虑，不需要Birth_Day这样的变量
wave1$Birth_Day <- NULL

#构建成人教育变量，是否参加过成人教育
wave1 <- wave1 %>%
  mutate(
    Adult_Edu = case_when(
      bd007s1 == 1 ~ 0,  # 没参加过
      rowSums(select(., bd007s2:bd007s8), na.rm = TRUE) >= 1 ~ 1,  # 任一参加过
      TRUE ~ NA_real_  # 无法判断
    )
  )

#然后不需要这些原始的变量了
wave1 <- wave1 %>%
  select(-c(bd007s1, bd007s2, bd007s3, bd007s4, bd007s5, bd007s6, bd007s7, bd007s8))


#构建年龄变量，并且筛选年龄大于等于60岁的
wave1 <- wave1 %>%
  mutate(Age_2011 = 2011 - Birth_Year) %>%
  filter(Age_2011 >= 60)

#处理一下户口数据
wave1 <- wave1 %>%
  rename(Current_Hukou = Current_Kukou)
#构建factor变量
wave1 <- wave1 %>%
  mutate(
    Current_Hukou = factor(Current_Hukou,
                           levels = c(1, 2, 3, 4),
                           labels = c("1", "2", "3", "4"))
  )

#处理一下教育变量，构建教育水平，并进行分组
wave1 <- wave1 %>%
  mutate(
    Edu_Group = case_when(
      bd001 %in% 1:3 ~ 1,    # 无正式教育
      bd001 == 4 ~ 2,        # 小学
      bd001 == 5 ~ 3,        # 初中
      bd001 >= 6 ~ 4         # 高中及以上
    )
  )
wave1 <- wave1 %>% rename(Edu_level = bd001)
#可以删掉了
wave1$Edu_level <- NULL

#然后处理婚姻情况
# Marital_Status: 1 = 已婚同居, 2 = 已婚不同居, 3 = 分居, 4 = 离异, 5 = 丧偶, 6 = 从未结婚
wave1 <- wave1 %>% rename(Marital_Status = be001)
wave1 <- wave1 %>%
  mutate(
    Marital_Status = as.factor(Marital_Status)
  )

#把农村/城市给加入一下，1农村，2城镇社区
household_roster2011 <- read_dta("~/Desktop/科研数据/CHARLS全数据/2011/household_and_community_questionnaire_data/household_roster.dta")
wave1 <- wave1 %>%
  left_join(househo_roster2011 %>% select(a001,householdID) %>%
              rename(Area_Type = a001), by = "householdID")
wave1 <- wave1 %>%
  mutate(
    Area_Type = as.factor(Area_Type)
  )

#保存这个基线的不变数据
write.csv(wave1, "基线的不变数据.csv", row.names = FALSE, fileEncoding = "UTF-8")

#获得一下基线的健康数据
health_status2011 <-  read_dta("~/Desktop/科研数据/CHARLS全数据/2011/household_and_community_questionnaire_data/health_status_and_functioning.dta")

#现在我们来排除一下基线的个体是否在基线存在认知功能障碍
own_wave1 <- own_wave1  %>%
  left_join(
    health_status2011 %>%
      select(ID, da005_2_, da007_12_),
    by = "ID"
  )

#把NA值的个体全部删除
own_wave1 <- own_wave1 %>%
  filter(!is.na(da005_2) & !is.na(da007_12))   # 仅保留有诊断信息的个体
#把在基线有认知疾病的个体全部删除
own_wave1 <- own_wave1 %>%
  filter(da005_2 != 1 & da007_12 != 1)         # 删除患有认知相关疾病的个体

own_wave1 <- own_wave1 %>% rename(Brain_damage = da005_2,Memory_disease = da007_12)


#现在开始提取我需要的变量了！
#先把社交活动提取出来
own_wave1 <- own_wave1 %>%
  left_join(health_status2011 %>% select(ID,da056s1:da056s12),
            by = "ID")
#创建变量，是否参与社会活动（二分类）
own_wave1 <- own_wave1 %>%
  mutate(
    social_participation2011 = case_when(
      da056s12 == 12 ~ 0,  # 完全没有参与
      rowSums(!is.na(across(starts_with("da056s"))) & across(starts_with("da056s")) != 12) > 0 ~ 1,
      TRUE ~ NA_real_
    )
  )
#创建变量，参加社会活动广度
own_wave1 <- own_wave1 %>%
  mutate(
    social_participation_count2011 = rowSums(across(starts_with("da056s"), ~ .x %in% 1:11), na.rm = TRUE)
  )
#可以删除了
own_wave1 <- own_wave1 %>% select(-starts_with("da056s"))

#可以开始构建ADL了，先把问卷的内容拿出来
own_wave1 <- own_wave1 %>%
  left_join(health_status2011 %>% select(ID,db010:db015),
            by = "ID")

#构建是否存在ADL，如果全是NA值代表没有功能障碍
adl_items2011 <- c("db010", "db011", "db012", "db013", "db014", "db015")
own_wave1 <- own_wave1 %>%
  mutate(
    adl_difficulty2011 = case_when(
      rowSums(across(all_of(adl_items2011), ~ .x %in% c(3, 4)), na.rm = TRUE) > 0 ~ 1, # 至少一项有困难
      rowSums(is.na(across(all_of(adl_items2011)))) == length(adl_items) ~ 0,         # 全是 NA，推断为无障碍
      TRUE ~ 0  # 剩下默认无障碍
    )
  )
#构建ADL得分
own_wave1 <- own_wave1 %>%
  mutate(
    adl_score2011 = rowSums(across(all_of(adl_items2011), ~ .x %in% c(3, 4)), na.rm = TRUE),
    adl_score2011 = if_else(rowSums(is.na(across(all_of(adl_items2011)))) == length(adl_items2011), 0, adl_score2011)
  )
#可以删除了
own_wave1 <- own_wave1 %>% select(-starts_with("db01"))


#构建CES-D
#先提取出来需要的变量
own_wave1 <- own_wave1 %>%
  left_join(health_status2011 %>% select(ID,dc009:dc018),
            by = "ID")
#计算CED-D得分，多重插补
# 加载必要的包
library(mice)
library(dplyr)
# Step 1: 提取 CESD 原始变量
cesd_vars <- c("dc009", "dc010", "dc011", "dc012", "dc013", 
               "dc014", "dc015", "dc016", "dc017", "dc018")
cesd_data <- own_wave1 %>% select(all_of(cesd_vars))
cesd_data <- cesd_data %>% mutate(across(everything(), ~ as.numeric(.)))
# Step 2: 多重插补（默认 m = 5）
imp <- mice(cesd_data, m = 5, method = "pmm", seed = 123)
# Step 3: 提取第一个插补数据集（也可以 later 做平均）
completed_data <- complete(imp, 1)
# Step 4: 反向题得分（dc013 和 dc016）
completed_data <- completed_data %>%
  mutate(
    dc013_r = 5 - dc013,
    dc016_r = 5 - dc016
  )
# Step 5: 计算 CESD 总得分
completed_data <- completed_data %>%
  mutate(
    cesd_score2011_imp = dc009 + dc010 + dc011 + dc012 +
      dc014 + dc015 + dc017 + dc018 +
      dc013_r + dc016_r
  )
# Step 6: 将多重插补得分合并到原始数据中
own_wave1$cesd_score2011_imp <- completed_data$cesd_score2011_imp
#删除不需要的
own_wave1 <- own_wave1 %>% select(-starts_with("dc0"))


#结果变量，认知分数
#提取需要的
own_wave1 <- own_wave1 %>%
  left_join(health_status2011 %>% select(ID,dc001s1:dc002,dc006s1:dc006s10,dc019:dc023,dc025,dc027s1:dc027s10),
            by = "ID")
#构建得分
library(dplyr)
own_wave1 <- own_wave1 %>%
  mutate(
    # 1. 定向力（Orientation）
    orientation2011 = rowSums(!is.na(across(c(dc001s1, dc001s2, dc001s3, dc002))), na.rm = TRUE),
    
    # 2. 即时回忆（Immediate Recall）
    immediate_recall2011 = rowSums(!is.na(across(dc006s1:dc006s10)), na.rm = TRUE),
    
    # 3. 数学能力（Numerical ability）
    math2011 = rowSums(!is.na(across(dc019:dc023)), na.rm = TRUE),
    
    # 4. 图形复制（Figure drawing）
    figure2011 = ifelse(!is.na(dc025), 1, 0),
    
    # 5. 延迟回忆（Delayed Recall）
    delayed_recall2011 = rowSums(!is.na(across(dc027s1:dc027s10)), na.rm = TRUE),
    
    # 最终总认知得分（加总五个分项）
    cognition_total2011 = orientation2011 + immediate_recall2011 + math2011 + figure2011 + delayed_recall2011)
#可以删除不需要的了
own_wave1 <- own_wave1 %>% select(-starts_with("dc0"))

#计算BMI
#把需要的数据转移过去
biomarkers2011 <- read_dta("~/Desktop/科研数据/CHARLS全数据/2011/household_and_community_questionnaire_data/biomarkers.dta")
own_wave1 <- own_wave1 %>%
  left_join(biomarkers2011 %>% select(ID,qi002,ql002) %>%
              rename(Height = qi002,Weight = ql002),by = "ID")
#存在NA值，但是因为是一个协变量，因此不删除NA值
#计算BMI指标
own_wave1$BMI2011 <- with(own_wave1, ql002 / (qi002/100)^2)

# 保存为 R 的本地数据格式（推荐）
saveRDS(own_wave1, file = "基线所有都处理完了.rds")

# 如果你也想导出为 CSV 备份（可选）
write.csv(own_wave1, file = "基线所有都处理完了.csv", row.names = FALSE)

#先处理一下数据，因为已经明确这些人在基线的时候没有认知功能障碍，因此可以删除了
wave1_2$Brain_damage <- NULL
wave1_2$Memory_disease <- NULL

#开始处理wave2的数据，因为2011（wave1）的数据竟然没有性别，因此先把性别给加入
#导入2013年的背景数据
library(haven)
background2013 <- read_dta("~/Desktop/科研数据/CHARLS全数据/2013/CHARLS2013_Dataset/Demographic_Background.dta")

#还有一个问题，wave1的ID和householdID需要修改
library(dplyr)
wave1_2 <- wave1_2 %>%
  mutate(
    householdID = as.character(householdID),   # 强制转为字符串
    ID = as.character(ID),                     # 也转换个体ID为字符型
    householdID = paste0(householdID, "0"),    # householdID 尾部补0
    individual_suffix = substr(ID, nchar(ID)-1+1, nchar(ID)),  # 提取原ID末尾两位
    ID = paste0(householdID, individual_suffix)  # 合成新12位ID
  ) %>%
  select(-individual_suffix)  # 清理临时变量

#把性别变量加入
wave1_2 <- wave1_2 %>%
  left_join(background2013 %>% select(ID,ba001_w2_1),
            by = "ID")
#1 = male, 2 = female

#修改名字
wave1_2 <- rename(wave1_2, gender = ba001_w2_1)

#修改一下排列顺序,把calendar_type和2011年的年龄去掉
wave1_2 <- wave1_2 %>%
  select(ID,householdID,communityID,gender,Area_Type,Birth_Year,Birth_Month,Current_Hukou,Marital_Status,
         spouse_edu,Adult_Edu,Edu_Group,social_participation2011,social_participation_count2011,
         adl_difficulty2011,adl_score2011,cesd_score2011_imp,orientation2011,immediate_recall2011,math2011,
         figure2011,delayed_recall2011,cognition_total2011,Height2011,Weight2011,BMI2011)

#导入2013年的身体健康数据
health_status2013 <- read_dta("~/Desktop/科研数据/CHARLS全数据/2013/CHARLS2013_Dataset/Health_Status_and_Functioning.dta")

#先处理社会参与
wave1_2 <- wave1_2 %>%
  left_join(health_status2013 %>% select(ID,da056s1:da056s12),
            by = "ID")
#创建变量，是否参与社会活动（二分类）
wave1_2 <- wave1_2 %>%
  mutate(
    social_participation2013 = case_when(
      da056s12 == 12 ~ 0,  # 完全没有参与
      rowSums(!is.na(across(starts_with("da056s"))) & across(starts_with("da056s")) != 12) > 0 ~ 1,
      TRUE ~ NA_real_
    )
  )
#创建变量，参加社会活动广度
wave1_2 <- wave1_2 %>%
  mutate(
    social_participation_count2013 = rowSums(across(starts_with("da056s"), ~ .x %in% 1:11), na.rm = TRUE)
  )
#可以删除了
wave1_2 <- wave1_2 %>% select(-starts_with("da056s"))

#处理ADL
wave1_2 <- wave1_2 %>%
  left_join(health_status2013 %>% select(ID,db010,db011,db012,db013,db014,db015),
            by = "ID")

#构建是否存在ADL，如果全是NA值代表没有功能障碍
adl_items2013 <- c("db010", "db011", "db012", "db013", "db014", "db015")
wave1_2 <- wave1_2 %>%
  mutate(
    adl_difficulty2013 = case_when(
      rowSums(across(all_of(adl_items2013), ~ .x %in% c(3, 4)), na.rm = TRUE) > 0 ~ 1, # 至少一项有困难
      rowSums(is.na(across(all_of(adl_items2013)))) == length(adl_items2013) ~ 0,         # 全是 NA，推断为无障碍
      TRUE ~ 0  # 剩下默认无障碍
    )
  )
#构建ADL得分
wave1_2 <- wave1_2 %>%
  mutate(
    adl_score2013 = rowSums(across(all_of(adl_items2013), ~ .x %in% c(3, 4)), na.rm = TRUE),
    adl_score2013 = if_else(rowSums(is.na(across(all_of(adl_items2013)))) == length(adl_items2013), 0, adl_score2013)
  )
#可以删除了
wave1_2 <- wave1_2 %>% select(-starts_with("db"))

#构建CES-D
# 加载必要的包
library(mice)
library(dplyr)
# 步骤 1: 合并 CESD 各题变量
wave1_2 <- wave1_2 %>%
  left_join(health_status2013 %>% select(ID, dc009:dc018), by = "ID")
# 步骤 2: 提取 CESD 各项题目并转换为 numeric
cesd_vars2013 <- c("dc009.x", "dc010.x", "dc011.x", "dc012.x", "dc013.x", 
                   "dc014.x", "dc015.x", "dc016.x", "dc017.x", "dc018.x")
cesd_data2013 <- wave1_2 %>%
  select(all_of(cesd_vars2013)) %>%
  mutate(across(everything(), ~ as.numeric(.)))
# 步骤 3: 多重插补 (PMM 插补法)
imp2013 <- mice(cesd_data2013, m = 5, method = "pmm", seed = 123)
# 步骤 4: 提取第一个插补数据集
completed_data2013 <- complete(imp2013, 1)
# 步骤 5: 处理反向题（dc013 和 dc016），并计算 CESD 得分
completed_data2013 <- completed_data2013 %>%
  mutate(
    dc013_r = 5 - dc013.x,
    dc016_r = 5 - dc016.x,
    cesd_score2013_imp = dc009.x + dc010.x + dc011.x + dc012.x +
      dc014.x + dc015.x + dc017.x + dc018.x +
      dc013_r + dc016_r
  )
# 步骤 6: 合并回主数据集
wave1_2$cesd_score2013_imp <- completed_data2013$cesd_score2013_imp
#删除不需要的
wave1_2 <- wave1_2 %>% select(-starts_with("dc0"))

# 2013年最后的，认知分数
wave1_2 <- wave1_2 %>%
  left_join(health_status2013 %>% select(ID,dc001s1:dc002,dc006_1_s1:dc006_1_s10,dc019:dc023,dc025,dc027s1:dc027s10),
            by = "ID")
#构建得分
library(dplyr)
wave1_2 <- wave1_2 %>%
  mutate(
    # 1. 定向力（Orientation）
    orientation2013 = rowSums(!is.na(across(c(dc001s1, dc001s2, dc001s3, dc002))), na.rm = TRUE),
    
    # 2. 即时回忆（Immediate Recall）
    immediate_recall2013 = rowSums(!is.na(across(dc006_1_s1:dc006_1_s10)), na.rm = TRUE),
    
    # 3. 数学能力（Numerical ability）
    math2013 = rowSums(!is.na(across(dc019:dc023)), na.rm = TRUE),
    
    # 4. 图形复制（Figure drawing）
    figure2013 = ifelse(!is.na(dc025), 1, 0),
    
    # 5. 延迟回忆（Delayed Recall）
    delayed_recall2013 = rowSums(!is.na(across(dc027s1:dc027s10)), na.rm = TRUE),
    
    # 最终总认知得分（加总五个分项）
    cognition_total2013 = orientation2013 + immediate_recall2013 + math2013 + figure2013 + delayed_recall2013)
#可以删除不需要的了
wave1_2 <- wave1_2 %>% select(-starts_with("dc0"))

#最终还是觉得舍弃BMI指数了
wave1_2 <- wave1_2 %>%
  select(-matches("^Height2011|^Weight2011|^BMI2011"))

setwd("~/Desktop/自己的科研：极端气温和老年人认知")
# 如果你也想导出为 CSV 备份（可选）
write.csv(wave1_2, file = "wave1和wave2处理完毕.csv", row.names = FALSE)

#开始处理wave3数据
#先导入已经处理好的数据
wave1_2_3 <- read.csv("~/Desktop/自己的科研：极端气温和老年人认知/wave1和wave2处理完毕.csv")
#导入2015年的健康数据
library(haven)
health_status2015 <- read_dta("~/Desktop/科研数据/CHARLS全数据/2015/CHARLS2015r/Health_Status_and_Functioning.dta")
#先处理社会参与
wave1_2_3 <- wave1_2_3 %>%
  mutate(ID = as.character(ID))
wave1_2_3 <- wave1_2_3 %>%
  left_join(health_status2015 %>% select(ID,da056s1:da056s12),
            by = "ID")
#创建变量，是否参与社会活动（二分类）
wave1_2_3 <- wave1_2_3 %>%
  mutate(
    social_participation2015 = case_when(
      da056s12 == 12 ~ 0,  # 完全没有参与
      rowSums(!is.na(across(starts_with("da056s"))) & across(starts_with("da056s")) != 12) > 0 ~ 1,
      TRUE ~ NA_real_
    )
  )
#创建变量，参加社会活动广度
wave1_2_3 <- wave1_2_3 %>%
  mutate(
    social_participation_count2015 = rowSums(across(starts_with("da056s"), ~ .x %in% 1:11), na.rm = TRUE)
  )
#可以删除了
wave1_2_3 <- wave1_2_3 %>% select(-starts_with("da056s"))

#处理ADL
wave1_2_3 <- wave1_2_3 %>%
  left_join(health_status2015 %>% select(ID,db010,db011,db012,db013,db014,db015),
            by = "ID")
#构建是否存在ADL，如果全是NA值代表没有功能障碍
adl_items2015 <- c("db010", "db011", "db012", "db013", "db014", "db015")
wave1_2_3 <- wave1_2_3 %>%
  mutate(
    adl_difficulty2015 = case_when(
      rowSums(across(all_of(adl_items2015), ~ .x %in% c(3, 4)), na.rm = TRUE) > 0 ~ 1, # 至少一项有困难
      rowSums(is.na(across(all_of(adl_items2015)))) == length(adl_items2015) ~ 0,         # 全是 NA，推断为无障碍
      TRUE ~ 0  # 剩下默认无障碍
    )
  )
#构建ADL得分
wave1_2_3 <- wave1_2_3 %>%
  mutate(
    adl_score2015 = rowSums(across(all_of(adl_items2015), ~ .x %in% c(3, 4)), na.rm = TRUE),
    adl_score2015 = if_else(rowSums(is.na(across(all_of(adl_items2015)))) == length(adl_items2015), 0, adl_score2015)
  )
#可以删除了
wave1_2_3 <- wave1_2_3 %>% select(-starts_with("db"))

#构建CES-D
# 加载必要的包
library(mice)
library(dplyr)
# 步骤 1: 合并 CESD 各题变量
wave1_2_3 <- wave1_2_3 %>%
  left_join(health_status2015 %>% select(ID, dc009:dc018), by = "ID")
# 步骤 2: 提取 CESD 各项题目并转换为 numeric
cesd_vars2015 <- c("dc009", "dc010", "dc011", "dc012", "dc013", 
                   "dc014", "dc015", "dc016", "dc017", "dc018")
cesd_data2015 <- wave1_2_3 %>%
  select(all_of(cesd_vars2015)) %>%
  mutate(across(everything(), ~ as.numeric(.)))
# 步骤 3: 多重插补 (PMM 插补法)
imp2015 <- mice(cesd_data2015, m = 5, method = "pmm", seed = 123)
# 步骤 4: 提取第一个插补数据集
completed_data2015 <- complete(imp2015, 1)
# 步骤 5: 处理反向题（dc013 和 dc016），并计算 CESD 得分
completed_data2015 <- completed_data2015 %>%
  mutate(
    dc013_r = 5 - dc013,
    dc016_r = 5 - dc016,
    cesd_score2015_imp = dc009 + dc010 + dc011 + dc012 +
      dc014 + dc015 + dc017 + dc018 +
      dc013_r + dc016_r
  )
# 步骤 6: 合并回主数据集
wave1_2_3$cesd_score2015_imp <- completed_data2015$cesd_score2015_imp
#删除不需要的
wave1_2_3 <- wave1_2_3 %>% select(-starts_with("dc0"))

# 2015年最后的，认知分数
wave1_2_3 <- wave1_2_3 %>%
  left_join(health_status2015 %>% select(ID,dc001s1:dc002,dc006s1:dc006s10,dc019:dc023,dc025,dc027s1:dc027s10),
            by = "ID")
#构建得分
library(dplyr)
wave1_2_3 <- wave1_2_3 %>%
  mutate(
    # 1. 定向力（Orientation）
    orientation2015 = rowSums(!is.na(across(c(dc001s1, dc001s2, dc001s3, dc002))), na.rm = TRUE),
    
    # 2. 即时回忆（Immediate Recall）
    immediate_recall2015 = rowSums(!is.na(across(dc006s1:dc006s10)), na.rm = TRUE),
    
    # 3. 数学能力（Numerical ability）
    math2015 = rowSums(!is.na(across(dc019:dc023)), na.rm = TRUE),
    
    # 4. 图形复制（Figure drawing）
    figure2015 = ifelse(!is.na(dc025), 1, 0),
    
    # 5. 延迟回忆（Delayed Recall）
    delayed_recall2015 = rowSums(!is.na(across(dc027s1:dc027s10)), na.rm = TRUE),
    
    # 最终总认知得分（加总五个分项）
    cognition_total2015 = orientation2015 + immediate_recall2015 + math2015 + figure2015 + delayed_recall2015)
#可以删除不需要的了
wave1_2_3 <- wave1_2_3 %>% select(-starts_with("dc0"))

setwd("~/Desktop/自己的科研：极端气温和老年人认知")
# 如果你也想导出为 CSV 备份（可选）
write.csv(wave1_2_3, file = "wave1,2,3处理完毕.csv", row.names = FALSE)


#因为阅读到了新的文章内容，需要修改一下
setwd("~/Desktop/自己的科研：极端气温和老年人认知")
wave123_fix <- read.csv("~/Desktop/自己的科研：极端气温和老年人认知/wave1,2,3处理完毕.csv")

#删除户口与成人教育与配偶教育水平
wave123_fix$Current_Hukou <- NULL
wave123_fix$Adult_Edu <- NULL
wave123_fix$spouse_edu <- NULL

#重新确定一下婚姻情况
wave123_fix <- wave123_fix %>%
  mutate(
    Marital_Status = case_when(
      Marital_Status %in% c(1, 2) ~ 1,  # 已婚同居或已婚不同居 → 有配偶
      Marital_Status %in% c(3, 4, 5, 6) ~ 0  # 分居、离婚、丧偶、未婚 → 无配偶
    )
  )

#要重新确定一下教育分组
#先把基线的数据再导入一下
library(haven)
background2011 <- read_dta("~/Desktop/科研数据/CHARLS全数据/2011/household_and_community_questionnaire_data/demographic_background.dta")
#修改一下ID
library(dplyr)
library(stringr)
background2011 <- background2011 %>%
  mutate(
    # householdID: 原本9位 → 加一个"0"在末尾，变成10位
    householdID_10 = paste0(householdID, "0"),
    
    # individualID: 用新的10位 householdID + 原来 ID 的最后2位，组成12位
    individualID_12 = paste0(householdID_10, str_sub(ID, -2, -1))
  )
#重新把教育的原始变量代入
wave123_fix <- wave123_fix %>%
  mutate(ID = as.character(ID)) %>%    # 先统一类型
  left_join(
    background2011 %>%
      mutate(individualID_12 = as.character(individualID_12)) %>%
      select(individualID_12, bd001),
    by = c("ID" = "individualID_12")
  )

#构建教育水平
wave123_fix <- wave123_fix %>%
  mutate(
    Edu_Group = case_when(
      bd001 %in% 1:5 ~ 1,          # 低教育水平：未上学、小学、初中等
      bd001 %in% 6:7 ~ 2,          # 中教育水平：高中、中专
      bd001 %in% 8:11 ~ 3,         # 高教育水平：大专、本科、硕博
      TRUE ~ NA_real_
    )
  )
write.csv(wave123_fix, file = "wave1,2,3处理完毕（2）.csv", row.names = FALSE)

#开始整理wave4的数据
wave1234 <- read.csv("~/Desktop/自己的科研：极端气温和老年人认知/wave1,2,3处理完毕（2）.csv")
wave1234$bd001 <- NULL

#导入wave4的数据
library(haven)
health_status2018 <- read_dta("~/Desktop/科研数据/CHARLS全数据/2018/CHARLS2018r/Health_Status_and_Functioning.dta")
#先处理社会参与
library(dplyr)
wave1234 <- wave1234 %>%
  mutate(ID = as.character(ID))
wave1234 <- wave1234 %>%
  left_join(health_status2018 %>% select(ID,da056_s1:da056_s12),
            by = "ID")
#创建变量，是否参与社会活动（二分类）
wave1234 <- wave1234 %>%
  mutate(
    social_participation2018 = case_when(
      da056_s12 == 12 ~ 0,  # 完全没有参与
      rowSums(!is.na(across(starts_with("da056_s"))) & across(starts_with("da056_s")) != 12) > 0 ~ 1,
      TRUE ~ NA_real_
    )
  )
#可以删除了
wave1234 <- wave1234 %>% select(-starts_with("da056_s"))
#因为社会参与的数量不同，因此为了保证可比较，因此删除社会参与数量
wave1234$social_participation_count2011 <- NULL
wave1234$social_participation_count2013 <- NULL
wave1234$social_participation_count2015 <- NULL

#处理ADL
wave1234 <- wave1234 %>%
  left_join(health_status2018 %>% select(ID,db010,db011,db012,db013,db014,db015),
            by = "ID")

#构建是否存在ADL，如果全是NA值代表没有功能障碍
adl_items2018 <- c("db010", "db011", "db012", "db013", "db014", "db015")
wave1234 <- wave1234 %>%
  mutate(
    adl_difficulty2018 = case_when(
      rowSums(across(all_of(adl_items2018), ~ .x %in% c(3, 4)), na.rm = TRUE) > 0 ~ 1, # 至少一项有困难
      rowSums(is.na(across(all_of(adl_items2018)))) == length(adl_items2018) ~ 0,         # 全是 NA，推断为无障碍
      TRUE ~ 0  # 剩下默认无障碍
    )
  )
#构建ADL得分
wave1234 <- wave1234 %>%
  mutate(
    adl_score2018 = rowSums(across(all_of(adl_items2018), ~ .x %in% c(3, 4)), na.rm = TRUE),
    adl_score2018 = if_else(rowSums(is.na(across(all_of(adl_items2018)))) == length(adl_items2018), 0, adl_score2018)
  )
#可以删除了
wave1234 <- wave1234 %>% select(-starts_with("db"))

#现在来计算ces-d
cogntion2018 <- read_dta("~/Desktop/科研数据/CHARLS全数据/2018/CHARLS2018r/Cognition.dta")
#构建CES-D
# 加载必要的包
library(mice)
library(dplyr)
# 步骤 1: 合并 CESD 各题变量
wave1234 <- wave1234 %>%
  left_join(cogntion2018 %>% select(ID, dc009:dc018), by = "ID")
# 步骤 2: 提取 CESD 各项题目并转换为 numeric
cesd_vars2018 <- c("dc009", "dc010", "dc011", "dc012", "dc013", 
                   "dc014", "dc015", "dc016", "dc017", "dc018")
cesd_data2018 <- wave1234 %>%
  select(all_of(cesd_vars2018)) %>%
  mutate(across(everything(), ~ as.numeric(.)))
# 步骤 3: 多重插补 (PMM 插补法)
imp2018 <- mice(cesd_data2018, m = 5, method = "pmm", seed = 123)
# 步骤 4: 提取第一个插补数据集
completed_data2018 <- complete(imp2018, 1)
# 步骤 5: 处理反向题（dc013 和 dc016），并计算 CESD 得分
completed_data2018 <- completed_data2018 %>%
  mutate(
    dc013_r = 5 - dc013,
    dc016_r = 5 - dc016,
    cesd_score2018_imp = dc009 + dc010 + dc011 + dc012 +
      dc014 + dc015 + dc017 + dc018 +
      dc013_r + dc016_r
  )
# 步骤 6: 合并回主数据集
wave1234$cesd_score2018_imp <- completed_data2018$cesd_score2018_imp
#删除不需要的
wave1234 <- wave1234 %>% select(-starts_with("dc0"))

# 2018年最后的，认知分数
#因为认知的问卷不一样，所以需要把2011-2015年的recall合并制作memory
wave1234$immediate_recall2011 <- (wave1234$immediate_recall2011 + wave1234$delayed_recall2011)/2
wave1234 <- wave1234 %>% rename(memory2011 = immediate_recall2011)
wave1234$cognition_total2011 <- wave1234$orientation2011 + wave1234$memory2011 + wave1234$math2011 + wave1234$figure2011

wave1234$immediate_recall2013 <- (wave1234$immediate_recall2013 + wave1234$delayed_recall2013)/2
wave1234 <- wave1234 %>% rename(memory2013 = immediate_recall2013)
wave1234$cognition_total2013 <- wave1234$orientation2013 + wave1234$memory2013 + wave1234$math2013 + wave1234$figure2013

wave1234$immediate_recall2015 <- (wave1234$immediate_recall2015 + wave1234$delayed_recall2015)/2
wave1234 <- wave1234 %>% rename(memory2015 = immediate_recall2015)
wave1234$cognition_total2015 <- wave1234$orientation2015 + wave1234$memory2015 + wave1234$math2015 + wave1234$figure2015
#删除delayed_recall
wave1234$delayed_recall2011 <- NULL
wave1234$delayed_recall2013 <- NULL
wave1234$delayed_recall2015 <- NULL

#提取需要的变量
wave1234 <- wave1234 %>%
  left_join(cogntion2018 %>% select(ID,dc001_w4,dc003_w4,dc005_w4,dc006_w4,dc028_w4_s1:dc028_w4_s10,dc029_w4_s1:dc029_w4_s10,
                                    dc030_w4_s1:dc030_w4_s10,dc047_w4_s1:dc047_w4_s10,dc014_w4_1_1,dc014_w4_2_1,dc014_w4_3_1,
                                    dc014_w4_4_1,dc014_w4_5_1,dc024_w4),
            by = "ID")
#构建得分
library(dplyr)

wave1234 <- wave1234 %>%
  mutate(
    # 1. 定向力（Orientation）：只有1得1分，其他都不得分；有 NA 就整体为 NA
    orientation2018 = rowSums(across(
      c(dc001_w4, dc003_w4, dc005_w4, dc006_w4),
      ~ ifelse(. == 1, 1, 0)
    ), na.rm = FALSE),  # 不跳过 NA
    
    # 2. Memory：40题，非 NA 得1分，NA得0分，然后除以4
    memory2018 = rowSums(across(
      c(dc028_w4_s1:dc028_w4_s10,
        dc029_w4_s1:dc029_w4_s10,
        dc030_w4_s1:dc030_w4_s10,
        dc047_w4_s1:dc047_w4_s10),
      ~ ifelse(is.na(.), 0, 1)
    ), na.rm = TRUE) / 4,
    
    # 3. Math：只要不是 NA 就得 1 分
    math2018 = rowSums(across(
      c(dc014_w4_1_1, dc014_w4_2_1, dc014_w4_3_1,
        dc014_w4_4_1, dc014_w4_5_1),
      ~ ifelse(is.na(.), 0, 1)
    ), na.rm = TRUE),
    
    # 4. 图形复制（Figure）：1为正确得1分，5为错误得0分，其余为NA
    figure2018 = case_when(
      dc024_w4 == 1 ~ 1,
      dc024_w4 == 5 ~ 0,
      dc024_w4 == 97 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # 5. 总认知得分（四项都不为NA才相加）
    cognition_total2018 = case_when(
      is.na(orientation2018) | is.na(memory2018) |
        is.na(math2018) | is.na(figure2018) ~ NA_real_,
      TRUE ~ orientation2018 + memory2018 + math2018 + figure2018
    )
  )
#可以删除不需要的了
wave1234 <- wave1234 %>% select(-starts_with("dc"))

#删掉有问题的结果，因为2018年的结构非常明确，且因为我们要做长期认知变化，因此2018年的NA值必须全部删除
wave1234 <- wave1234 %>%
  filter(!is.na(cognition_total2018))  # 必须保留该年的认知数据

#然后有两个小错误，修改一下2011到2015年的认知分数
library(dplyr)
library(stringr)
health_status2011 <- health_status2011 %>%
  mutate(
    # householdID: 原本9位 → 加一个"0"在末尾，变成10位
    householdID_10 = paste0(householdID, "0"),
    
    # individualID: 用新的10位 householdID + 原来 ID 的最后2位，组成12位
    individualID_12 = paste0(householdID_10, str_sub(ID, -2, -1))
  )


wave1234 <- wave1234 %>%
  left_join(health_status2011 %>% select(individualID_12,dc001s1:dc002,dc025),
           by = c("ID"="individualID_12"))

#构建2011得分，NA作为0
library(dplyr)
wave1234 <- wave1234 %>%
  mutate(
    # 定向力（Orientation）
    orientation2011 = 
      coalesce(as.numeric(!is.na(dc001s1)), 0) +
      coalesce(as.numeric(!is.na(dc001s2)), 0) +
      coalesce(as.numeric(!is.na(dc001s3)), 0) +
      ifelse(dc002 == 1, 1, 0),
    
    #  图形复制（Figure drawing）
    figure2011 = ifelse(dc025 == 1, 1, 0),
    
    # 最终总认知得分（加总五个分项）
    cognition_total2011 = coalesce(orientation2011, 0)  + memory2011 + math2011 +  coalesce(figure2011, 0)  )


wave1234 <- wave1234 %>%
  left_join(health_status2013 %>% select(ID,dc001s1:dc002,dc025),
            by = "ID")

#2013年构建得分
library(dplyr)
wave1234 <- wave1234 %>%
  mutate(
    # 定向力（Orientation）
    orientation2013 = 
      coalesce(as.numeric(!is.na(dc001s1)), 0) +
      coalesce(as.numeric(!is.na(dc001s2)), 0) +
      coalesce(as.numeric(!is.na(dc001s3)), 0) +
      ifelse(dc002 == 1, 1, 0),
    
    #  图形复制（Figure drawing）
    figure2013 = ifelse(dc025 == 1, 1, 0),
    
    # 最终总认知得分（加总五个分项）
    cognition_total2013 = coalesce(orientation2013, 0)  + memory2013 + math2013 +  coalesce(figure2013, 0)  )

wave1234 <- wave1234 %>%
  left_join(health_status2015 %>% select(ID,dc001s1:dc002,dc025),
            by = "ID")

#2015年构建得分
library(dplyr)
wave1234 <- wave1234 %>%
  mutate(
    # 定向力（Orientation）
    orientation2015 = 
      coalesce(as.numeric(!is.na(dc001s1)), 0) +
      coalesce(as.numeric(!is.na(dc001s2)), 0) +
      coalesce(as.numeric(!is.na(dc001s3)), 0) +
      ifelse(dc002 == 1, 1, 0),
    
    #  图形复制（Figure drawing）
    figure2015 = ifelse(dc025 == 1, 1, 0),
    
    # 最终总认知得分（加总五个分项）
    cognition_total2015 = coalesce(orientation2015, 0)  + memory2015 + math2015 +  coalesce(figure2015, 0)  )

#查看2013年的死亡数据
exit2013 <- read_dta("~/Desktop/科研数据/CHARLS全数据/2013/CHARLS2013_Dataset/Exit_Interview.dta")
# 假设 exit2013 中的 ID 是已经死亡或退出的
exit2013 <- exit2013 %>% mutate(died = 1)
wave1234 <- wave1234 %>%
  left_join(exit2013 %>% select(ID, died), by = "ID") %>%
  mutate(died = ifelse(is.na(died), 0, died))
#都没有去世，非常好
wave1234$died <- NULL
setwd("~/Desktop/自己的科研：极端气温和老年人认知")
write.csv(wave1234, file = "wave1234处理完毕.csv", row.names = FALSE)

#开始处理最后一年数据
wave12345 <- read.csv("~/Desktop/自己的科研：极端气温和老年人认知/wave1234处理完毕.csv")
#导入2020年数据
library(haven)
health_status2020 <- read_dta("~/Desktop/科研数据/CHARLS全数据/2020/CHARLS2020r/Health_Status_and_Functioning.dta")

#因为2020年的social participation减少了，所以前面的都需要改变
library(dplyr)
wave12345 <- wave12345 %>%
  mutate(ID = as.character(ID))
wave12345 <- wave12345 %>%
  left_join(health_status2011 %>% select(individualID_12,da056s1:da056s8,da056s11,da056s12),
            by = c("ID"="individualID_12"))
wave12345 <- wave12345 %>%
  mutate(
    # 合并 da056s6 和 da056s7
    sp6_7_merged = case_when(
      !is.na(da056s6) & da056s6 != 12 ~ 1,
      !is.na(da056s7) & da056s7 != 12 ~ 1,
      is.na(da056s6) & is.na(da056s7) ~ NA_real_,
      TRUE ~ 0
    ),
    
    # 计算是否有参与（只要任意项 ≠ NA 且 ≠ 12）
    participated = rowSums(
      cbind(
        across(c(da056s1:da056s5, da056s8, da056s11), ~ !is.na(.) & . != 12),
        sp6_7_merged == 1
      ),
      na.rm = TRUE
    ),
    
    # 构造最终参与变量
    social_participation2011 = case_when(
      da056s12 == 12 ~ 0,       # 明确表示“从未参与”
      participated > 0 ~ 1,     # 至少有一项参与
      TRUE ~ 0                  # 所有为 NA 或全为 12
    )
  )

wave12345 <- wave12345 %>% select(-starts_with("da"))
wave12345$sp6_7_merged <- NULL
wave12345$participated <- NULL

#修改2013年
wave12345 <- wave12345 %>%
  left_join(health_status2013 %>% select(ID,da056s1:da056s8,da056s11,da056s12),
            by = "ID")
wave12345 <- wave12345 %>%
  mutate(
    # 合并 da056s6 和 da056s7
    sp6_7_merged = case_when(
      !is.na(da056s6) & da056s6 != 12 ~ 1,
      !is.na(da056s7) & da056s7 != 12 ~ 1,
      is.na(da056s6) & is.na(da056s7) ~ NA_real_,
      TRUE ~ 0
    ),
    
    # 计算是否有参与（只要任意项 ≠ NA 且 ≠ 12）
    participated = rowSums(
      cbind(
        across(c(da056s1:da056s5, da056s8, da056s11), ~ !is.na(.) & . != 12),
        sp6_7_merged == 1
      ),
      na.rm = TRUE
    ),
    
    # 构造最终参与变量
    social_participation2013 = case_when(
      da056s12 == 12 ~ 0,       # 明确表示“从未参与”
      participated > 0 ~ 1,     # 至少有一项参与
      TRUE ~ 0                  # 所有为 NA 或全为 12
    )
  )
wave12345 <- wave12345 %>% select(-starts_with("da"))
wave12345$sp6_7_merged <- NULL
wave12345$participated <- NULL

#修改2015年的
wave12345 <- wave12345 %>%
  left_join(health_status2015 %>% select(ID,da056s1:da056s8,da056s11,da056s12),
            by = "ID")
wave12345 <- wave12345 %>%
  mutate(
    # 合并 da056s6 和 da056s7
    sp6_7_merged = case_when(
      !is.na(da056s6) & da056s6 != 12 ~ 1,
      !is.na(da056s7) & da056s7 != 12 ~ 1,
      is.na(da056s6) & is.na(da056s7) ~ NA_real_,
      TRUE ~ 0
    ),
    
    # 计算是否有参与（只要任意项 ≠ NA 且 ≠ 12）
    participated = rowSums(
      cbind(
        across(c(da056s1:da056s5, da056s8, da056s11), ~ !is.na(.) & . != 12),
        sp6_7_merged == 1
      ),
      na.rm = TRUE
    ),
    
    # 构造最终参与变量
    social_participation2015 = case_when(
      da056s12 == 12 ~ 0,       # 明确表示“从未参与”
      participated > 0 ~ 1,     # 至少有一项参与
      TRUE ~ 0                  # 所有为 NA 或全为 12
    )
  )
wave12345 <- wave12345 %>% select(-starts_with("da"))
wave12345$sp6_7_merged <- NULL
wave12345$participated <- NULL

#修改2018年的
wave12345 <- wave12345 %>%
  left_join(health_status2018 %>% select(ID,da056_s1:da056_s8,da056_s11,da056_s12),
            by = "ID")
wave12345 <- wave12345 %>%
  mutate(
    # 合并 da056s6 和 da056s7
    sp6_7_merged = case_when(
      !is.na(da056_s6) & da056_s6 != 12 ~ 1,
      !is.na(da056_s7) & da056_s7 != 12 ~ 1,
      is.na(da056_s6) & is.na(da056_s7) ~ NA_real_,
      TRUE ~ 0
    ),
    
    # 计算是否有参与（只要任意项 ≠ NA 且 ≠ 12）
    participated = rowSums(
      cbind(
        across(c(da056_s1:da056_s5, da056_s8, da056_s11), ~ !is.na(.) & . != 12),
        sp6_7_merged == 1
      ),
      na.rm = TRUE
    ),
    
    # 构造最终参与变量
    social_participation2018 = case_when(
      da056_s12 == 12 ~ 0,       # 明确表示“从未参与”
      participated > 0 ~ 1,     # 至少有一项参与
      TRUE ~ 0                  # 所有为 NA 或全为 12
    )
  )
wave12345 <- wave12345 %>% select(-starts_with("da"))
wave12345$sp6_7_merged <- NULL
wave12345$participated <- NULL


#正式开始处理2020年数据,先处理社会参与
library(dplyr)
wave12345 <- wave12345 %>%
  left_join(health_status2020 %>% select(ID,da038_s1:da038_s9),
            by = "ID")
#创建变量，是否参与社会活动（二分类）
wave12345 <- wave12345 %>%
  mutate(
    social_participation2020 = case_when(
      da038_s9 == 9 ~ 0,  # 完全没有参与
      rowSums(!is.na(across(starts_with("da038_s"))) & across(starts_with("da038_s")) != 12) > 0 ~ 1,
      TRUE ~ NA_real_
    )
  )
#可以删除了
wave12345 <- wave12345 %>% select(-starts_with("da"))

#处理ADL
wave12345 <- wave12345 %>%
  left_join(health_status2020 %>% select(ID,db001,db003,db005,db007,db009,db011),
            by = "ID")

#构建是否存在ADL，如果全是NA值代表没有功能障碍
adl_items2020 <- c("db001", "db003", "db005", "db007", "db009", "db011")
wave12345 <- wave12345 %>%
  mutate(
    adl_difficulty2020 = case_when(
      rowSums(across(all_of(adl_items2020), ~ .x %in% c(3, 4)), na.rm = TRUE) > 0 ~ 1, # 至少一项有困难
      rowSums(is.na(across(all_of(adl_items2020)))) == length(adl_items2020) ~ 0,         # 全是 NA，推断为无障碍
      TRUE ~ 0  # 剩下默认无障碍
    )
  )
#构建ADL得分
wave12345 <- wave12345 %>%
  mutate(
    adl_score2020 = rowSums(across(all_of(adl_items2020), ~ .x %in% c(3, 4)), na.rm = TRUE),
    adl_score2020 = if_else(rowSums(is.na(across(all_of(adl_items2020)))) == length(adl_items2020), 0, adl_score2020)
  )
#可以删除了
wave12345 <- wave12345 %>% select(-starts_with("db"))

#计算cesd
library(mice)
library(dplyr)
# 步骤 1: 合并 CESD 各题变量
wave12345 <- wave12345 %>%
  left_join(health_status2020 %>% select(ID, dc016:dc025), by = "ID")
# Step 2: 清洗 CESD 各题变量，处理特殊值
cesd_vars2020 <- c("dc016", "dc017", "dc018", "dc019", "dc020", 
                   "dc021", "dc022", "dc023", "dc024", "dc025")

cesd_data2020 <- wave12345 %>%
  select(all_of(cesd_vars2020)) %>%
  mutate(across(
    everything(),
    ~ ifelse(. %in% c(997, 998, 999), NA, as.numeric(.))
  ))

# Step 3: 多重插补（PMM）
library(mice)
imp2020 <- mice(cesd_data2020, m = 5, method = "pmm", seed = 123)

# Step 4: 提取第一个插补数据集
completed_data2020 <- complete(imp2020, 1)

# Step 5: 反向题并计算 CESD 总分
completed_data2020 <- completed_data2020 %>%
  mutate(
    dc020_r = 5 - dc020,
    dc023_r = 5 - dc023,
    cesd_score2020_imp = dc016 + dc017 + dc018 + dc019 +
      dc021 + dc022 + dc024 + dc025 +
      dc020_r + dc023_r
  ) %>%
  # 限制合理总分范围，去除极端值（通常 0~30 或 0~40）
  mutate(
    cesd_score2020_imp = ifelse(cesd_score2020_imp < 0 | cesd_score2020_imp > 40, NA, cesd_score2020_imp)
  )

# Step 6: 合并回主数据集
wave12345$cesd_score2020_imp <- completed_data2020$cesd_score2020_imp
#可以删除了
wave12345 <- wave12345 %>% select(-starts_with("dc"))

wave12345 <- wave12345 %>%
  left_join(health_status2020 %>% select(ID,dc001,dc003,dc004,dc005,dc012_s1:dc012_s10,dc013_s1:dc013_s10,dc014_s1:dc014_s10,
                                         dc028_s1:dc028_s10,dc007_1_1,dc007_2_1,dc007_3_1,dc007_4_1,dc007_5_1,
                                         dc009),
            by = "ID")
#构建得分
library(dplyr)

wave12345 <- wave12345 %>%
  mutate(
    # 1. 定向力（Orientation）：只有1得1分，其他都不得分；有 NA 就整体为 NA
    orientation2020 = rowSums(across(
      c(dc001, dc003, dc004, dc005),
      ~ ifelse(. == 1, 1, 0)
    ), na.rm = FALSE),  # 不跳过 NA
    
    # 2. Memory：40题，非 NA 得1分，NA得0分，然后除以4
    memory2020 = rowSums(across(
      c(dc012_s1:dc012_s10,
        dc013_s1:dc013_s10,
        dc014_s1:dc014_s10,
        dc028_s1:dc028_s10),
      ~ ifelse(is.na(.), 0, 1)
    ), na.rm = TRUE) / 4,
    
    # 3. Math：只要不是 NA 就得 1 分
    math2020 = rowSums(across(
      c(dc007_1_1, dc007_2_1, dc007_3_1,
        dc007_4_1, dc007_5_1),
      ~ ifelse(is.na(.), 0, 1)
    ), na.rm = TRUE),
    
    # 4. 图形复制（Figure）：1为正确得1分，5为错误得0分，其余为NA
    figure2020 = case_when(
      dc009 == 1 ~ 1,
      dc009 == 2 ~ 0,
      dc009 == 3 ~ 0,
      dc009 == 999 ~ 0,
      dc009 == 997 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # 5. 总认知得分（四项都不为NA才相加）
    cognition_total2020 = case_when(
      is.na(orientation2020) | is.na(memory2020) |
        is.na(math2020) | is.na(figure2020) ~ NA_real_,
      TRUE ~ orientation2020 + memory2020 + math2020 + figure2020
    )
  )
#可以删除不需要的了
wave12345 <- wave12345 %>% select(-starts_with("dc"))

#删掉有问题的结果，因为2020年的结构非常明确，且因为我们要做长期认知变化，因此2020年的NA值必须全部删除
wave12345 <- wave12345 %>%
  filter(!is.na(cognition_total2020))  # 必须保留该年的认知数据
setwd("~/Desktop/自己的科研：极端气温和老年人认知")
write.csv(wave12345, file = "wave12345处理完毕.csv", row.names = FALSE)

#开始做最后的分析
temperature_cognition <- read.csv("~/Desktop/自己的科研：极端气温和老年人认知/wave12345处理完毕.csv")
#先生成z分数，中介变量和结果变量都生成
temperature_cognition$cognition_total2011_z <- as.numeric(scale(temperature_cognition$cognition_total2011))
temperature_cognition$cesd_score2011_z     <- as.numeric(scale(temperature_cognition$cesd_score2011_imp))
temperature_cognition$adl_score2011_z      <- as.numeric(scale(temperature_cognition$adl_score2011))

temperature_cognition$cognition_total2013_z <- as.numeric(scale(temperature_cognition$cognition_total2013))
temperature_cognition$cesd_score2013_z     <- as.numeric(scale(temperature_cognition$cesd_score2013_imp))
temperature_cognition$adl_score2013_z      <- as.numeric(scale(temperature_cognition$adl_score2013))

temperature_cognition$cognition_total2015_z <- as.numeric(scale(temperature_cognition$cognition_total2015))
temperature_cognition$cesd_score2015_z     <- as.numeric(scale(temperature_cognition$cesd_score2015_imp))
temperature_cognition$adl_score2015_z      <- as.numeric(scale(temperature_cognition$adl_score2015))

temperature_cognition$cognition_total2018_z <- as.numeric(scale(temperature_cognition$cognition_total2018))
temperature_cognition$cesd_score2018_z     <- as.numeric(scale(temperature_cognition$cesd_score2018_imp))
temperature_cognition$adl_score2018_z      <- as.numeric(scale(temperature_cognition$adl_score2018))

temperature_cognition$cognition_total2020_z <- as.numeric(scale(temperature_cognition$cognition_total2020))
temperature_cognition$cesd_score2020_imp_z <- as.numeric(scale(temperature_cognition$cesd_score2020_imp))
temperature_cognition$adl_score2020_z      <- as.numeric(scale(temperature_cognition$adl_score2020))

#打开气候数据
install.packages("readxl")  # 第一次使用时安装
library(readxl)
temperature <- read_excel("~/Desktop/自己的科研：极端气温和老年人认知/Berkeely earth.xlsx")
#计算原始气温
# 构建高温和低温的参考表
ref_temp <- data.frame(
  Month = 1:12,
  High_Ref = c(-1.94, 1.30, 7.87, 14.75, 20.03, 23.63, 25.43, 24.65, 20.56, 14.20, 6.44, -0.03),
  Low_Ref  = c(-14.58, -11.66, -5.25, 1.39, 6.98, 11.45, 14.03, 13.13, 8.34, 1.37, -6.04, -12.36)
)
library(dplyr)

temperature <- temperature %>%
  left_join(ref_temp, by = "Month") %>%
  mutate(
    High_Absolute = `High-Anomaly` + High_Ref,
    Low_Absolute  = `Low-Anomaly` + Low_Ref
  )

#构建极端气温
# 按月计算2011–2018期间各月的95/5百分位
quantiles <- temperature %>%
  filter(Year >= 2011, Year <= 2018) %>%
  group_by(Month) %>%
  summarise(
    p95_high = quantile(High_Absolute, 0.95, na.rm = TRUE),
    p5_low  = quantile(Low_Absolute, 0.05, na.rm = TRUE)
  )

# 加入极端温度指示变量
temperature_extreme <- temperature %>%
  filter(Year >= 2011, Year <= 2018) %>%
  left_join(quantiles, by = "Month") %>%
  mutate(
    is_extreme_high = ifelse(High_Absolute > p95_high, 1, 0),
    is_extreme_low = ifelse(Low_Absolute < p5_low, 1, 0)
  )

extreme_summary <- temperature_extreme %>%
  filter(Year %in% c(2011, 2013, 2015, 2018)) %>%
  group_by(Year) %>%
  summarise(
    extreme_high = sum(is_extreme_high),
    extreme_low = sum(is_extreme_low)
  )
library(tidyr)
#转换为宽格式
extreme_wide <- extreme_summary %>%
  pivot_wider(
    names_from = Year,
    values_from = c(extreme_high, extreme_low),
    names_glue = "{.value}_{Year}"
  )

#合并
temperature_cognition <-cbind(temperature_cognition, extreme_wide)

#开始进行基于斜率的 K-means 聚类，因为没有2020年的气候数据，因此没有加入2020年
install.packages("lcmm")
library(lcmm)
#宽变长
library(dplyr)
library(tidyr)
cognition_long_data <- temperature_cognition %>%
  select(ID, matches("^cognition_total(2011|2013|2015|2018)_z$")) %>%
  pivot_longer(
    cols = -ID,
    names_to = "year",
    names_pattern = "cognition_total(\\d{4})_z",
    values_to = "cognition_z"
  ) %>%
  mutate(year = as.numeric(year))

library(dplyr)
library(purrr)
library(tidyr)

# Step 1: 计算每人认知变化的斜率（year vs cognition_z）
slopes_data <- cognition_long_data %>%
  group_by(ID) %>%
  filter(!is.na(cognition_z)) %>%
  summarise(
    n_obs = n(),
    slope = if (n_obs >= 2) coef(lm(cognition_z ~ year))[2] else NA_real_
  ) %>%
  ungroup()
set.seed(123)
# 先去除 NA，再聚类（你可以改 k = 3 或 4 试试）
slope_for_kmeans <- slopes_data %>%
  filter(!is.na(slope)) %>%
  select(ID, slope)

# 聚类，比如分成 3 组
kmeans_result <- kmeans(slope_for_kmeans$slope, centers = 3)

# 加入聚类标签
slope_for_kmeans <- slope_for_kmeans %>%
  mutate(group = kmeans_result$cluster)

library(dplyr)
library(ggplot2)

# 1) 按平均斜率从低到高给 cluster 排序，并重命名
ord <- slope_for_kmeans %>%
  group_by(group) %>%
  summarise(mean_slope = mean(slope, na.rm = TRUE), .groups = "drop") %>%
  arrange(mean_slope) %>%
  pull(group)

labs <- c("Declining", "Stable", "Improving")[seq_along(ord)]

slope_for_kmeans <- slope_for_kmeans %>%
  mutate(traj_group = factor(group, levels = ord, labels = labs))

# 2) 画箱线图（用新的 traj_group，和后续所有图保持一致）
ggplot(slope_for_kmeans, aes(x = traj_group, y = slope, fill = traj_group)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_boxplot(color = "black", alpha = 0.75) +
  scale_fill_manual(values = c("Declining"="#1F78B4","Stable"="#33A02C","Improving"="#FB9A99")) +
  labs(x = "Trajectory cluster (ordered by mean slope)",
       y = "Cognitive slope",
       title = "Slope-based K-means Clustering") +
  theme_minimal(base_size = 12) +
  guides(fill = "none")


#查看可能的数量
install.packages("purrr")   
library(purrr)              
wss <- function(k) {
  kmeans(slope_for_kmeans$slope, centers = k, nstart = 10)$tot.withinss
}

k.values <- 1:6
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values, type = "b", 
     xlab = "Number of clusters K", ylab = "Total within-cluster sum of squares",
     main = "Elbow Method for Optimal K")
#最终的最好就是3

#开始分析
#先把组加入
library(dplyr)
temperature_cognition <- temperature_cognition %>%
  left_join(slope_for_kmeans %>% select(ID,group),by = "ID")
# 描述性分析 — 检查 CESD、ADL 在不同 group 的均值差异
temperature_cognition %>%
  group_by(group) %>%
  summarise(
    mean_cesd2011 = mean(cesd_score2011_imp, na.rm = TRUE),
    sd_cesd2011 = sd(cesd_score2011_imp, na.rm = TRUE)
  )

temperature_cognition %>%
  group_by(group) %>%
  summarise(mean_ADL = mean(adl_score2011, na.rm = TRUE),
            sd_ADL = sd(adl_score2011, na.rm = TRUE))

install.packages("nnet")
library(nnet)
# 建议将 group 转换为因子变量
temperature_cognition$group <- as.factor(temperature_cognition$group)
temperature_cognition <- temperature_cognition %>%
  mutate(
    extreme_high_total = rowSums(across(c(extreme_high_2011, extreme_high_2013, extreme_high_2015, extreme_high_2018)), na.rm = TRUE),
    extreme_low_total = rowSums(across(c(extreme_low_2011, extreme_low_2013, extreme_low_2015, extreme_low_2018)), na.rm = TRUE)
  )


#model1
model1 <- multinom(group ~ extreme_high_total + extreme_low_total,data = temperature_cognition)
summary(model1)

#model2_high
model2_high <- multinom(group ~ extreme_high_total + gender + Area_Type + Marital_Status + Edu_Group,
                        data = temperature_cognition)
summary(model2_high)

#中介分析1
model_high_cesd <- multinom(group ~ extreme_high_total + gender + Area_Type + Marital_Status + Edu_Group +
                              cesd_score2011_z + cesd_score2013_z + cesd_score2015_z + cesd_score2018_z,
                            data = temperature_cognition) 
model_high_adl <- multinom(group ~ extreme_high_total + gender + Area_Type + Marital_Status + Edu_Group +
                              adl_score2011_z + adl_score2013_z + adl_score2015_z + adl_score2018_z,
                            data = temperature_cognition) 
model_high_social <- multinom(group ~ extreme_high_total + gender + Area_Type + Marital_Status + Edu_Group +
                               social_participation2011 + social_participation2013 + social_participation2015 +
                                social_participation2018,
                              data = temperature_cognition) 
# 提取气温系数
coef_base <- summary(model2_high)$coefficients[,"extreme_high_total"]
coef_cesd <- summary(model_high_cesd)$coefficients[,"extreme_high_total"]
coef_adl  <- summary(model_high_adl)$coefficients[,"extreme_high_total"]
coef_soc  <- summary(model_high_social)$coefficients[,"extreme_high_total"]

# 计算中介效应与中介比例
medi_cesd <- coef_base - coef_cesd
medi_adl  <- coef_base - coef_adl
medi_soc  <- coef_base - coef_soc

prop_cesd <- round(medi_cesd / coef_base * 100, 2)
prop_adl  <- round(medi_adl  / coef_base * 100, 2)
prop_soc  <- round(medi_soc  / coef_base * 100, 2)

# 合并结果
data.frame(
  group = names(coef_base),
  prop_cesd = prop_cesd,
  prop_adl = prop_adl,
  prop_soc = prop_soc
)

#检查显著性
# 载入必要包
library(nnet)

# 计算 CESD 模型的显著性
z_cesd <- summary(model_high_cesd)$coefficients / summary(model_high_cesd)$standard.errors
p_cesd <- 2 * (1 - pnorm(abs(z_cesd)))

# 计算 ADL 模型的显著性
z_adl <- summary(model_high_adl)$coefficients / summary(model_high_adl)$standard.errors
p_adl <- 2 * (1 - pnorm(abs(z_adl)))

# 计算社会参与模型的显著性
z_social <- summary(model_high_social)$coefficients / summary(model_high_social)$standard.errors
p_social <- 2 * (1 - pnorm(abs(z_social)))
# 打印 CESD 相关变量的显著性
cat("\n--- CESD p-values ---\n")
print(p_cesd[, grep("cesd", colnames(p_cesd))])

# 打印 ADL 相关变量的显著性
cat("\n--- ADL p-values ---\n")
print(p_adl[, grep("adl", colnames(p_adl))])

# 打印社会参与相关变量的显著性
cat("\n--- Social Participation p-values ---\n")
print(p_social[, grep("social_participation", colnames(p_social))])


#model2_low
model2_low <- multinom(group ~ extreme_low_total + gender + Area_Type + Marital_Status + Edu_Group,
                        data = temperature_cognition)
summary(model2_low)


#中介分析2
model_low_cesd <- multinom(group ~ extreme_low_total + gender + Area_Type + Marital_Status + Edu_Group +
                              cesd_score2011_z + cesd_score2013_z + cesd_score2015_z + cesd_score2018_z,
                            data = temperature_cognition) 
model_low_adl <- multinom(group ~ extreme_low_total + gender + Area_Type + Marital_Status + Edu_Group +
                             adl_score2011_z + adl_score2013_z + adl_score2015_z + adl_score2018_z,
                           data = temperature_cognition) 
model_low_social <- multinom(group ~ extreme_low_total + gender + Area_Type + Marital_Status + Edu_Group +
                                social_participation2011 + social_participation2013 + social_participation2015 +
                                social_participation2018,
                              data = temperature_cognition) 
# 提取气温系数
coef_base2 <- summary(model2_low)$coefficients[,"extreme_low_total"]
coef_cesd2 <- summary(model_low_cesd)$coefficients[,"extreme_low_total"]
coef_adl2  <- summary(model_low_adl)$coefficients[,"extreme_low_total"]
coef_soc2 <- summary(model_low_social)$coefficients[,"extreme_low_total"]

# 计算中介效应与中介比例
medi_cesd2 <- coef_base2 - coef_cesd2
medi_adl2  <- coef_base2 - coef_adl2
medi_soc2  <- coef_base2 - coef_soc2

prop_cesd2 <- round(medi_cesd2 / coef_base2 * 100, 2)
prop_adl2  <- round(medi_adl2  / coef_base2 * 100, 2)
prop_soc2  <- round(medi_soc2  / coef_base2 * 100, 2)

# 合并结果
data.frame(
  group = names(coef_base2),
  prop_cesd = prop_cesd2,
  prop_adl = prop_adl2,
  prop_soc = prop_soc2
)

#检查显著性
# 载入必要包
library(nnet)

# 计算 CESD 模型的显著性
z_cesd_low <- summary(model_low_cesd)$coefficients / summary(model_low_cesd)$standard.errors
p_cesd_low <- 2 * (1 - pnorm(abs(z_cesd_low)))


# 计算 ADL 模型的显著性
z_adl_low <- summary(model_low_adl)$coefficients / summary(model_low_adl)$standard.errors
p_adl_low <- 2 * (1 - pnorm(abs(z_adl_low)))

# 计算社会参与模型的显著性
z_social_low <- summary(model_low_social)$coefficients / summary(model_low_social)$standard.errors
p_social_low <- 2 * (1 - pnorm(abs(z_social_low)))
# 打印 CESD 相关变量的显著性
cat("\n--- CESD p-values ---\n")
print(p_cesd[, grep("cesd", colnames(p_cesd_low))])

# 打印 ADL 相关变量的显著性
cat("\n--- ADL p-values ---\n")
print(p_adl[, grep("adl", colnames(p_adl_low))])

# 打印社会参与相关变量的显著性
cat("\n--- Social Participation p-values ---\n")
print(p_social[, grep("social_participation", colnames(p_social_low))])

#敏感性分析
model_high_cesd_raw <- multinom(group ~ extreme_high_total + gender + Area_Type + Marital_Status + Edu_Group +
                                 cesd_score2011_imp + cesd_score2013_imp + cesd_score2015_imp + cesd_score2018_imp,
                               data = temperature_cognition)
summary(model_high_cesd_raw)

model_high_adl_raw <- multinom(group ~ extreme_high_total + gender + Area_Type + Marital_Status + Edu_Group +
                                 adl_score2011 + adl_score2013 + adl_score2015 + adl_score2018,
                               data = temperature_cognition)
summary(model_high_adl_raw)

setwd("~/Desktop/自己的科研：极端气温和老年人认知")
write.csv(temperature_cognition, file = "temperature_cognition.csv", row.names = FALSE)

temperature_cognition <- read.csv("~/Desktop/自己的科研：极端气温和老年人认知/temperature_cognition.csv")
#简单的描述性分析
library(dplyr)
library(tableone)
temperature_cognition <- temperature_cognition %>%
  mutate(Baseline_Age = 2011 - Birth_Year)

temperature_cognition <- temperature_cognition %>%
  mutate(
    # gender: 1=male, 2=female
    gender = factor(gender,
                    levels = c(1, 2),
                    labels = c("Male", "Female")),
    # Area_Type: 1=农村, 2=城市
    Area_Type = factor(Area_Type,
                       levels = c(1, 2),
                       labels = c("Rural", "Urban")),
    # Marital_Status: 1=有配偶, 2=无配偶
    Marital_Status = factor(Marital_Status,
                            levels = c(1, 2),
                            labels = c("With spouse", "Without spouse")),
    # Edu_Group: 1=低, 2=中, 3=高
    Edu_Group = factor(Edu_Group,
                       levels = c(1, 2, 3),
                       labels = c("Low", "Medium", "High"))
  )
vars <- c("Baseline_Age", "gender", "Area_Type", "Marital_Status", "Edu_Group")
factorVars <- c("gender", "Area_Type", "Marital_Status", "Edu_Group")

# ---------- 3) 生成 Table 1 ----------
table1 <- CreateTableOne(vars = vars,
                         data = temperature_cognition,
                         factorVars = factorVars)

# 打印（分类变量显示 n (%)；去引号、去多余空格）
print(table1, showAllLevels = TRUE, quote = FALSE, noSpaces = TRUE)

#画气温
library(tidyr)
library(ggplot2)

extreme_summary |>
  pivot_longer(c(extreme_high, extreme_low),
               names_to = "type", values_to = "count") |>
  ggplot(aes(x = factor(Year), y = count, fill = type)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.65) +
  labs(x = "Year", y = "Count of months", fill = "") +
  geom_text(aes(label = count), position = position_dodge(0.7), vjust = -0.3, size = 3)+
  theme_minimal(base_size = 12)


# 计算各组在每个波次的均值
library(dplyr)
library(tidyr)
library(ggplot2)

# 0) 保证有 ID 列（如果没有就临时造一个）
if (!"ID" %in% names(temperature_cognition)) {
  temperature_cognition <- temperature_cognition %>% mutate(ID = row_number())
}

# 1) 宽转长：ID, year, cog（把列名换成你的实际列名即可）
long <- temperature_cognition %>%
  select(ID,
         cognition_total2011, cognition_total2013,
         cognition_total2015, cognition_total2018) %>%
  pivot_longer(starts_with("cognition_total"),
               names_to = "wave", values_to = "cog") %>%
  mutate(year = as.integer(gsub("\\D", "", wave))) %>%
  arrange(ID, year)

# 2) 每人的认知斜率（至少有≥2个波次）
slopes <- long %>%
  group_by(ID) %>%
  filter(!is.na(cog)) %>%
  mutate(t = year - min(year, na.rm = TRUE)) %>%
  summarise(slope = if (n() >= 2) coef(lm(cog ~ t))[2] else NA_real_, .groups = "drop") %>%
  filter(!is.na(slope))

# 3) k-means 聚成3组，并按“组内平均斜率”从低到高重命名
set.seed(2024)
km <- kmeans(slopes$slope, centers = 3, nstart = 50)
slopes$cluster_raw <- km$cluster

ord_tbl <- slopes %>% group_by(cluster_raw) %>%
  summarise(mean_slope = mean(slope), .groups = "drop") %>%
  arrange(mean_slope)

slopes <- slopes %>%
  left_join(ord_tbl, by = "cluster_raw") %>%
  mutate(traj_group = factor(match(cluster_raw, ord_tbl$cluster_raw),
                             levels = 1:3,
                             labels = c("Declining","Stable","Improving")))

# ——自检：确认排序与命名——
print(slopes %>% group_by(traj_group) %>%
        summarise(mean_slope = mean(slope), n = n(), .groups = "drop"))

# 4) 把分组合回 long（后续所有图都用 traj_group）
long_lab <- long %>% inner_join(slopes %>% select(ID, traj_group, slope), by = "ID")

# 5) 箱线图（斜率分布）——与折线图同一分组
p_box <- ggplot(slopes, aes(x = traj_group, y = slope, fill = traj_group)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_boxplot(color = "black", alpha = .75) +
  scale_fill_manual(values = c("Declining"="#E41A1C","Stable"="#4DAF4A","Improving"="#377EB8")) +
  labs(title = "Slope-based K-means Clustering",
       x = "Trajectory cluster (ordered by mean slope)", y = "Cognitive slope") +
  theme_minimal(base_size = 12) +
  guides(fill = "none")

# 6) 折线图（各组随时间的平均认知分数）
traj_mean <- long_lab %>%
  group_by(traj_group, year) %>%
  summarise(mean_cog = mean(cog, na.rm = TRUE), .groups = "drop")

p_line <- ggplot(traj_mean, aes(x = year, y = mean_cog, color = traj_group, group = traj_group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Declining"="#E41A1C","Stable"="#4DAF4A","Improving"="#377EB8")) +
  scale_x_continuous(breaks = c(2011, 2013, 2015, 2018)) +
  labs(title = "Average cognitive score by trajectory group over time",
       x = "Year (wave)", y = "Mean cognitive score", color = "Trajectory") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top", plot.title = element_text(face = "bold"))

p_box; p_line


# ---- Heat only: descriptive attenuation bar chart ----
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr) 
# 只用 heat 的衰减结果
df_heat <- data.frame(
  group   = names(coef_base),   # 来自你的 model2_high 同一参照组
  CESD    = prop_cesd,
  ADL     = prop_adl,
  Social  = prop_soc
)

heat_long <- df_heat |>
  pivot_longer(c(CESD, ADL, Social),
               names_to = "Mediator", values_to = "Share") |>
  mutate(Mediator = factor(Mediator, levels = c("CESD","ADL","Social")),
         group    = factor(group))  # “1”“3”之类

# y 轴范围可按需要调整（例如 -120~150）
ymax <- max(0, ceiling(max(heat_long$Share, na.rm = TRUE)/10)*10)
ymin <- min(0, floor(min(heat_long$Share, na.rm = TRUE)/10)*10)

p_medi_heat <- ggplot(heat_long, aes(x = group, y = Share, fill = Mediator)) +
  geom_hline(yintercept = 0, color = "grey60") +
  geom_col(position = position_dodge(width = 0.8), width = 0.7, color = "black") +
  geom_text(aes(label = paste0(round(Share,1), "%")),
            position = position_dodge(width = 0.8), vjust = -0.25, size = 3) +
  scale_y_continuous(limits = c(ymin, ymax)) +
  labs(
    title    = str_wrap("Heat effect: descriptive attenuation (multinomial logit)", 50),
    subtitle = str_wrap("Bars = % change in heat coefficient/OR after adding each block (descriptive, not causal)", 70),
    x = "Trajectory group (vs reference)", y = "% change"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position    = "top",
    plot.title         = element_text(hjust = 0.5, face = "bold", lineheight = 1.05),
    plot.subtitle      = element_text(hjust = 0.5),
    plot.title.position= "plot",
    # 注意：margin 顺序是 t, r, b, l；把左/右边距调大些
    plot.margin        = margin(18, 24, 14, 24)
  )
p_medi_heat


# ---- ΔMcFadden R² mini bar chart (heat context) ----
library(nnet)

# 为了同一批样本：取所有会用到的变量的 complete cases
vars_ctrl <- c("group","gender","Area_Type","Marital_Status","Edu_Group")
blk_CESD  <- c("cesd_score2011_z","cesd_score2013_z","cesd_score2015_z","cesd_score2018_z")
blk_ADL   <- c("adl_score2011_z","adl_score2013_z","adl_score2015_z","adl_score2018_z")
blk_SOC   <- c("social_participation2011","social_participation2013",
               "social_participation2015","social_participation2018")

df_r2 <- temperature_cognition |>
  dplyr::select(dplyr::all_of(c(vars_ctrl, blk_CESD, blk_ADL, blk_SOC))) |>
  tidyr::drop_na()

m_null <- multinom(group ~ 1, data = df_r2, trace = FALSE)
m_ctrl <- multinom(group ~ gender + Area_Type + Marital_Status + Edu_Group, data = df_r2, trace = FALSE)
m_cesd <- update(m_ctrl, . ~ . + cesd_score2011_z + cesd_score2013_z + cesd_score2015_z + cesd_score2018_z)
m_adl  <- update(m_ctrl, . ~ . + adl_score2011_z + adl_score2013_z + adl_score2015_z + adl_score2018_z)
m_soc  <- update(m_ctrl, . ~ . + social_participation2011 + social_participation2013 +
                   social_participation2015 + social_participation2018)

mcF <- function(fit, null_fit) 1 - as.numeric(logLik(fit) / logLik(null_fit))

r2_ctrl <- mcF(m_ctrl, m_null)
r2_cesd <- mcF(m_cesd, m_null)
r2_adl  <- mcF(m_adl,  m_null)
r2_soc  <- mcF(m_soc,  m_null)

res_r2 <- tibble::tibble(
  Block = c("CESD","Social","ADL"),
  dR2   = c(r2_cesd - r2_ctrl, r2_soc - r2_ctrl, r2_adl - r2_ctrl)
)

p_r2 <- ggplot(res_r2, aes(Block, dR2)) +
  geom_col(width = 0.55, fill = "#4F83CC", color = "black") +
  geom_text(aes(label = sprintf("+%.3f", dR2)), vjust = -0.3, size = 3.2) +
  labs(title = "Incremental explanatory power (ΔMcFadden R²)",
       subtitle = "vs. controls-only model (multinomial logit)",
       x = NULL, y = "ΔR²") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

p_r2

# ---- Forest plot for extreme heat (OR with 95% CI) ----
extract_or_ci <- function(model, var){
  s  <- summary(model)
  b  <- s$coefficients[, var]
  se <- s$standard.errors[, var]
  tibble::tibble(
    group = names(b),
    OR  = exp(b),
    LCL = exp(b - 1.96*se),
    UCL = exp(b + 1.96*se),
    z   = b / se,
    p   = 2*(1 - pnorm(abs(b / se)))
  )
}

or_heat <- extract_or_ci(model2_high, "extreme_high_total") |>
  dplyr::mutate(sig = dplyr::case_when(
    is.na(p)            ~ "",
    p < 0.001           ~ "***",
    p < 0.01            ~ "**",
    p < 0.05            ~ "*",
    TRUE                ~ ""
  ))

p_forest_heat <- ggplot(or_heat, aes(y = group, x = OR)) +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = LCL, xmax = UCL), height = 0.15) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  # （可选）把显著性星标标在点右侧
  geom_text(aes(label = sig), nudge_x = 0.03, vjust = 0.5, size = 4) +
  scale_x_continuous(trans = "log10",
                     breaks = c(0.5, 0.75, 1, 1.25, 1.5, 2),
                     labels = scales::number_format(accuracy = 0.01)) +
  labs(title = "Effect of extreme heat on trajectory membership",
       subtitle = "Odds ratio (log scale) with 95% CI; * p<0.05, ** p<0.01, *** p<0.001",
       x = "Odds Ratio (log-scale)", y = "Trajectory group (vs reference)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

p_forest_heat

# ==== Cold only: 描述性衰减柱状图 + ΔMcFadden R² ====
library(dplyr)
library(tidyr)
library(ggplot2)
library(nnet)
library(stringr)
library(scales)

# ---- 1) 描述性衰减柱状图（cold）----
# 使用你已计算好的 prop_cesd2 / prop_adl2 / prop_soc2 与 coef_base2
df_cold <- data.frame(
  group  = names(coef_base2),   # 例如 "1","3"，参照组=2
  CESD   = prop_cesd2,
  ADL    = prop_adl2,
  Social = prop_soc2
)

cold_long <- df_cold |>
  pivot_longer(c(CESD, ADL, Social),
               names_to = "Mediator", values_to = "Share") |>
  mutate(
    Mediator = factor(Mediator, levels = c("CESD","ADL","Social")),
    group    = factor(group, levels = sort(unique(group)))  # 保证 1、3 的顺序
  )

# 对称 y 轴 & 动态文字位置
lim <- max(abs(range(cold_long$Share, na.rm = TRUE)))
p_medi_cold <- ggplot(cold_long, aes(x = group, y = Share, fill = Mediator)) +
  geom_hline(yintercept = 0, color = "grey65") +
  geom_col(position = position_dodge(width = 0.8), width = 0.7, color = "black") +
  geom_text(
    aes(label = paste0(round(Share,1), "%"),
        vjust  = ifelse(Share >= 0, -0.35, 1.2)),
    position = position_dodge(width = 0.8), size = 3
  ) +
  scale_y_continuous(limits = c(-1.1*lim, 1.1*lim)) +
  labs(
    title    = "Extreme Cold",
    subtitle = "Reference = Group 2 (Stable) — descriptive attenuation (multinomial logit)",
    x = "Trajectory group (vs reference)", y = "% change"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position     = "bottom",
    plot.title          = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle       = element_text(hjust = 0.5),
    plot.title.position = "plot",
    plot.margin         = margin(16, 22, 12, 22)
  )

p_medi_cold


# ---- 2) ΔMcFadden R² 小柱图（使用相同样本，controls-only 为基线）----
vars_ctrl <- c("group","gender","Area_Type","Marital_Status","Edu_Group")
blk_CESD  <- c("cesd_score2011_z","cesd_score2013_z","cesd_score2015_z","cesd_score2018_z")
blk_ADL   <- c("adl_score2011_z","adl_score2013_z","adl_score2015_z","adl_score2018_z")
blk_SOC   <- c("social_participation2011","social_participation2013",
               "social_participation2015","social_participation2018")

df_r2 <- temperature_cognition |>
  dplyr::select(dplyr::all_of(c(vars_ctrl, blk_CESD, blk_ADL, blk_SOC))) |>
  tidyr::drop_na()

m_null <- multinom(group ~ 1, data = df_r2, trace = FALSE)
m_ctrl <- multinom(group ~ gender + Area_Type + Marital_Status + Edu_Group, data = df_r2, trace = FALSE)
m_cesd <- update(m_ctrl, . ~ . + cesd_score2011_z + cesd_score2013_z + cesd_score2015_z + cesd_score2018_z)
m_adl  <- update(m_ctrl, . ~ . + adl_score2011_z + adl_score2013_z + adl_score2015_z + adl_score2018_z)
m_soc  <- update(m_ctrl, . ~ . + social_participation2011 + social_participation2013 +
                   social_participation2015 + social_participation2018)

mcF <- function(fit, null_fit) 1 - as.numeric(logLik(fit) / logLik(null_fit))

r2_ctrl <- mcF(m_ctrl, m_null)
r2_cesd <- mcF(m_cesd, m_null)
r2_adl  <- mcF(m_adl,  m_null)
r2_soc  <- mcF(m_soc,  m_null)

res_r2_cold <- tibble::tibble(
  Block = c("ADL","CESD","Social"),
  dR2   = c(r2_adl - r2_ctrl, r2_cesd - r2_ctrl, r2_soc - r2_ctrl)
)

p_r2_cold <- ggplot(res_r2_cold, aes(Block, dR2)) +
  geom_col(width = 0.55, fill = "#4F83CC", color = "black") +
  geom_text(aes(label = sprintf("+%.3f (%.1f%%)", dR2, dR2*100)),
            vjust = -0.35, size = 3.2) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  labs(
    title    = NULL,
    subtitle = "Incremental explanatory power (ΔMcFadden R²) vs. controls-only",
    x = NULL, y = "ΔR²"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.subtitle       = element_text(hjust = 0.5),
    plot.title.position = "plot",
    plot.margin         = margin(10, 22, 8, 22)
  )

p_r2_cold

