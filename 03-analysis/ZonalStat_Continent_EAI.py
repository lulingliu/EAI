
# 1 Get the shp, make it rasterized
# Shp: "F:\Data_Projects\Proj_EAI_2024\OutputData\continent_bound_id.shp"
# Shp2: "F:\Data_Projects\Proj_EAI_2024\OutputData\income_bound_id.shp"
# 使用"ATTRIBUTE=ID_cont",数据类型设置为Byte


# 2 Zonal statistics
# 分区统计，统计相同属性字段下, 不同区间内像元值的百分比
# 区间分为[0-25],[26-50],[51-75],[76-100]

# Input1: "F:\Data_Projects\Proj_EAI_2024\Process\R14C36_EAI\ByYear\19920101.tif"
# Input2: "F:\Data_Projects\Proj_EAI_2024\Process\R14C36_EAI\ByYear\20220101.tif"


# # ======================================          shp转栅格     =======================================================

# # shp转栅格
from osgeo import gdal, ogr
import os
# 打开 Shapefile，并获取图层
# shapefile_path = r"F:\Data_Projects\Proj_EAI_2024\OutputData\income_bound_id.shp"
# shapefile = ogr.Open(shapefile_path)
# layer = shapefile.GetLayer()

# # 打开栅格文件
# raster_path = r"F:\Data_Projects\Proj_EAI_2024\Process\R14C36_EAI\ByYear\19920101.tif"
# input_raster_ds = gdal.Open(raster_path)
# x_size = input_raster_ds.RasterXSize  # 获取栅格的行数
# y_size = input_raster_ds.RasterYSize  # 获取栅格的列数
# proj = input_raster_ds.GetProjection()  # 获取投影信息
# geotrans = input_raster_ds.GetGeoTransform()  # 获取地理转换信息

# # 创建新的栅格文件
output_raster_path = r'F:\Data_Projects\Proj_EAI_2024\OutputData\incomeShp_EAI_mask.tif'
# output_raster_ds = gdal.GetDriverByName('GTiff').Create(output_raster_path, x_size, y_size, 1, 
#                                                         gdal.GDT_Byte)
#                                                         # gdal.GDT_Int16)  
# # 注意：数据类型设置为32位整型,如果设为Byte,则范围为0-255
# output_raster_ds.SetProjection(proj)
# output_raster_ds.SetGeoTransform(geotrans)

# # # 执行栅格化操作，使用"ATTRIBUTE=ID_cont"确保栅格值由"ID_cont"字段确定
# gdal.RasterizeLayer(output_raster_ds, [1], layer, options=["ATTRIBUTE=ID_cont"])

# # 释放资源
# shapefile = None
# input_raster_ds = None
# output_raster_ds = None


# ======================================          分区统计，统计相同属性字段下的像元数量     =======================================================

import numpy as np
import pandas as pd
from tqdm import tqdm
import glob

# raster_path现在是包含tif文件的文件夹路径
raster_folder_path = r'F:\Data_Projects\Proj_EAI_2024\Process\R14C36_EAI\ByYear\ForStat'


# 设置掩膜路径
mask_path = output_raster_path
maskraster = gdal.Open(mask_path)
maskdata = maskraster.ReadAsArray()

unique_values = np.unique(maskdata[maskdata != 0])

# 初始化结果列表
results = []

# 获取文件夹中的所有tif文件路径
tif_files = glob.glob(os.path.join(raster_folder_path, '*.tif'))

# for raster_path in tqdm(tif_files[0:16], desc="Processing TIF Files"):
#     file_name = os.path.basename(raster_path)
#     year = file_name[16:20]
#     year2 = file_name.split('_')[3]
#     print(f"Year: {year2} starts zonal stats!")


# 遍历folder中的所有tif文件
for raster_path in tqdm(tif_files[0:16], desc="Processing TIF Files"):
    
    input_raster_ds = gdal.Open(raster_path)
    input_raster_array = input_raster_ds.ReadAsArray()
    file_name = os.path.basename(raster_path)
    print(f"File: {file_name} starts zonal stats!")

    # 从文件名中提取年份
    year = file_name[:4]  


    # 对于每一个在mask中出现的值，进行计算

    for value in unique_values:
        masked_array = input_raster_array[maskdata == value]
        masked_array = masked_array[~np.isnan(masked_array)]  # 移除NaN值
        
        if masked_array.size > 0:
            # 计算各个区间的像元值数量
            count_0_25 = np.sum((masked_array >= 0) & (masked_array <= 25))
            count_26_50 = np.sum((masked_array > 25) & (masked_array <= 50))
            count_51_75 = np.sum((masked_array > 50) & (masked_array <= 75))
            count_76_100 = np.sum((masked_array > 75) & (masked_array <= 100))

            # 计算总有效像元数量
            total_count = masked_array.size

            # 计算各个区间的百分比
            percent_0_25 = (count_0_25 / total_count) * 100
            percent_26_50 = (count_26_50 / total_count) * 100
            percent_51_75 = (count_51_75 / total_count) * 100
            percent_76_100 = (count_76_100 / total_count) * 100

            # 存储每个tif文件、每个唯一值的统计结果
            results.append({
                'Year': year,
                'ELEMID': value,
                'Percent_0_25': percent_0_25,
                'Percent_26_50': percent_26_50,
                'Percent_51_75': percent_51_75,
                'Percent_76_100': percent_76_100
            })

# 创建数据帧
df_results = pd.DataFrame(results)

# 将数据帧保存到Excel文件中
output_excel_path = r'F:\Data_Projects\Proj_EAI_2024\Table\EAI\Analysis\EAI_Grid_income_4区间百分比.xlsx'
df_results.to_excel(output_excel_path, index=False)

print("统计结果已成功保存到:", output_excel_path)

