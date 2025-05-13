# ==================================     分区统计   ==============================================




import os
from osgeo import gdal
import numpy as np
import pandas as pd
from tqdm import tqdm
import glob

# 假设raster_path现在是包含tif文件的文件夹路径

raster_folder_path = r'F:\Data_Projects\Proj_EAI_2024\Process\Spatial_Hotspot\ShpToRast'

# 设置掩膜路径

mask_path = r'F:\Data_Projects\Proj_EAI_2024\Process\Spatial_Hotspot\ShpToRast\Globe_Country_simple_R14E36.tif' 

maskraster = gdal.Open(mask_path)
maskdata = maskraster.ReadAsArray()

unique_mask_values = np.unique(maskdata[maskdata != 0])

# 初始化结果列表
results = []



# 获取文件夹中的所有tif文件路径
tif_files = glob.glob(os.path.join(raster_folder_path, '*New_R14E36.tif'))
# print(len(tif_files))

# 限制处理的tif文件数
# tif_files = tif_files[:len(tif_files)]
# print(tif_files)


# 遍历folder中的所有tif文件
for raster_path in tif_files:

    input_raster_ds = gdal.Open(raster_path)
    input_raster_array = input_raster_ds.ReadAsArray()
    file_name = os.path.basename(raster_path)
    print(f"File: {file_name} starts zonal stats!")

    # 对于每一个在mask中出现的值，进行计算
    # 每一个值出现的次数，
    # 每个值出现次数占所有值出现次数的百分比
    for mask_value in tqdm(unique_mask_values, desc=f"Processing {file_name}"):
        masked_array = input_raster_array[maskdata == mask_value]
        masked_array = masked_array[~np.isnan(masked_array)]  # 移除NaN值
        
        if masked_array.size > 0:
            unique_values, counts = np.unique(masked_array, return_counts=True)
            total_count = masked_array.size

            # Iterate through unique values in the masked array
            for category_value, count_value in zip(unique_values, counts):
                percent_value = (count_value / total_count) * 100  # Percentage of masked pixels
                # Append results
                results.append({
                    'FileName': file_name,
                    'MaskValue': mask_value,  # Mask zone value
                    'Category': category_value,  # Unique value in the masked array
                    'Count': count_value,  # Count of the unique value
                    'Percent': percent_value  # Percentage of masked array pixels
                })
        else:
            # Append NaN result for empty masked array
            results.append({
                'FileName': file_name,
                'MaskValue': mask_value,
                'Category': np.nan,
                'Count': np.nan,
                'Percent': np.nan
            })

    

# 创建数据帧
df_results = pd.DataFrame(results)

# 将数据帧保存到Excel文件中
# Define output file path
output_table_path = r'F:\Data_Projects\Proj_EAI_2024\Table\EAI\Analysis\EAI_HotSpot_wgs84New_stat_SOC.xlsx'

df_results.to_excel(output_table_path, 
                    index=False)

print(f"Zonal statistics saved to: {output_table_path}")




