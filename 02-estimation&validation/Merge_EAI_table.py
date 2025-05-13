# *********************************************************
# Filename: merge_eai_tables.py
# Purpose: Merge population and electrified density tables by year and SOC,
#          calculate EAI (Electrification Access Index), and save the results.
# 1. Read CSV and Excel tables containing population and electrified density data.
# 2. Merge tables based on the 'Year' and 'SOC' columns.
# 3. Calculate the 'EAI_NTL' column and constrain its max value to 100.
# 4. Retain two decimal places and replace values greater than 100 with 100.
# 5. Save the result to a new Excel file.
# *********************************************************



import os,io
import sys

import pandas as pd
from tqdm import tqdm
import glob

# 修改标准输出流的编码
sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8')

# 定义一个函数从指定目录读取并合并两类表格的数据
def read_merge_tables(first_type_path, second_type_path):
    all_data = []

    # 读取第一类表格-电力人口
    first_type_files = glob.glob(first_type_path + '/*.csv')
    year_to_elec_file = {os.path.basename(file).split('_')[4]: file for file in first_type_files}

    # 检查year_to_elec_file映射
    print("Year to electric file mapping:")
    for year, file in year_to_elec_file.items():
        print(year, "->", file)
    
    # 读取第二类表格（一个包含所有年份的xlsx文件）
    glob_pop_files = glob.glob(second_type_path + '/*.xlsx')
    df_glob_pop = pd.read_excel(glob_pop_files[0], sheet_name='Merged_1992_2020')


    # 遍历年份列
    for year in tqdm(df_glob_pop['Year'].unique(), desc="Processing years"):
        

        # 从文件名中提取年份信息
        year = str(year)
        
        # 查找对应年份的第一类表格文件
        if year in year_to_elec_file:
            elec_file_path = year_to_elec_file[year]
            print(f"Reading {elec_file_path}")

            # 读取匹配文件的第一类表格（电力数据）
            df_elec = pd.read_csv(elec_file_path, encoding='ISO-8859-1')

            # 根据年份指定电力数据的文件名和列名
            column_name = 'CCNL_elec_p_sum' if int(year) < 2013 else 'NPP_elec_p_sum'

            # 获取当前年份的人口数据
            df_year_pop = df_glob_pop[df_glob_pop['Year'] == int(year)]

            # 使用合并（内连接）方法合并全球人口表格和电力表格
            combined_df = df_year_pop.merge(df_elec[['SOC', 'ELEMID', column_name]], 
                                                on=['SOC', 'ELEMID'], 
                                                how='left')
                
            # 计算EAI_NTL，保留两位小数，限制最大值为100
            combined_df['EAI_NTL'] = (100 * combined_df[f'{column_name}'] / combined_df['Glob_p_sum']).round(2)
            combined_df.loc[combined_df['EAI_NTL'] > 100, 'EAI_NTL'] = 100
                
            # 添加到总数据中
            all_data.append(combined_df)

        # 如果找不到对应年份电力数据，打印错误
        else:
            print(f"No matching electric file found for year {year}")

    # 合并所有数据
    return pd.concat(all_data)

# 第一类表格路径
first_type_path = r'H:\EAI_Carbon\Table\EAI\计算EAI\table_elec_count'
# 第二类表格路径
second_type_path = r'H:\EAI_Carbon\Table\EAI\计算EAI\table_pop_count'

# 合并表格
final_df = read_merge_tables(first_type_path, second_type_path)

# 保存到新的Excel文件
output_path = r'H:\EAI_Carbon\Table\EAI\计算EAI\EAI_NTL_1992-2022(包含电力人口密度).xlsx'
final_df.to_excel(output_path, index=False)

# 打印消息表示任务完成
print(f"Merged data saved to {output_path}")


# 只保留 'SOC', 'Year', 'EAI_NTL' 列，并保存到新的Excel文件
output_columns = ['SOC', 'Year', 'EAI_NTL']
final_df_subset = final_df[output_columns]
subset_output_path = r'H:\EAI_Carbon\Table\EAI\计算EAI\EAI_NTL_Results_1992_2020.xlsx'
final_df_subset.to_excel(subset_output_path, index=False)

# 打印结束信息
print(f"只包含 'SOC', 'Year', 'EAI_NTL' 列的数据已保存到 {subset_output_path}")




import pandas as pd

# 定义处理EAI_NTL列的函数
def process_eai_ntl(df):
    df['EAI_NTL'] = df['EAI_NTL'].round(2)  # 保留两位小数
    df.loc[df['EAI_NTL'] > 100, 'EAI_NTL'] = 100  # 将大于100的值替换为100
    return df

# 读取第一个文件，并处理EAI_NTL列
df_1992_2020 = pd.read_excel(r'H:\EAI_Carbon\Table\EAI\计算EAI\EAI_NTL_Results_1992_2020.xlsx')
df_1992_2020 = process_eai_ntl(df_1992_2020)

# 读取第二个文件，并处理EAI_NTL列
df_2021_2022 = pd.read_excel(r'H:\EAI_Carbon\Table\EAI\计算EAI\EAI_NTL_Results_2021_2022.xlsx')
df_2021_2022 = process_eai_ntl(df_2021_2022)

# 合并两个DataFrame
combined_df = pd.concat([df_1992_2020, df_2021_2022])

# 保存合并后的DataFrame到新文件
output_path = r'H:\EAI_Carbon\Table\EAI\计算EAI\EAI_NTL_Results_1992_2022.xlsx'
combined_df.to_excel(output_path, index=False)

print(f"合并后的数据已保存到 {output_path}")
