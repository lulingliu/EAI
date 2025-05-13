from osgeo import gdal

def get_tiff_dimensions(filepath):
    dataset = gdal.Open(filepath)
    if dataset is None:
        raise IOError(f"Unable to open {filepath}")
    
    cols = dataset.RasterXSize
    rows = dataset.RasterYSize
    
    return rows, cols

def get_tiff_spatial_extent(filepath):
    dataset = gdal.Open(filepath)
    if dataset is None:
        raise IOError(f"Unable to open {filepath}")
    
    # 获取地理变换数据
    geotransform = dataset.GetGeoTransform()
    
    # 左上角坐标
    originX = geotransform[0]
    originY = geotransform[3]
    
    # 像素宽度和高度
    pixelWidth = geotransform[1]
    pixelHeight = geotransform[5]  # 注意，这个值通常是负的

    # 获取栅格尺寸
    cols = dataset.RasterXSize
    rows = dataset.RasterYSize
    
    # 计算覆盖范围
    minX = originX
    maxY = originY  # 注意，由于像素高度是负的，最大纬度实际是原点的纬度
    maxX = originX + cols * pixelWidth
    minY = originY + rows * pixelHeight  # 此处加上一个负值
    
    return (minX, minY, maxX, maxY)  # xmin, ymin, xmax, ymax

# 设置TIFF文件路径

# tiff_path = r'E:\Inputs_NTL\DMSP_CCNL\CCNL_DMSP_1992_V1.tif' 
# Rows: 16802, Columns: 43201
#　Spatial extent (minX, minY, maxX, maxY): (-180.00, -65.01, 180.00, 75.00)

# tiff_path = r'F:\Research\Inputs_NTL\result2positive_LSJ\Aligned_VNLv2\SingleAlign_BilinearIntepol_adjust_VNLv2_npp_2013.tif' 
# Rows: 16800, Columns: 43201
# Spatial extent (minX, minY, maxX, maxY): (-180.00, -65.00, 180.00, 75.00)

tiff_path = r'd:\Data_Projects\Proj_EAI_2024\Process\GlobPOP_eletrified_density\CCNL_electri_den_1992.tif'

# 获取TIFF文件的行数和列数
rows, cols = get_tiff_dimensions(tiff_path)
print(f"Rows: {rows}, Columns: {cols}")

# 获取TIFF的空间覆盖范围
bbox = get_tiff_spatial_extent(tiff_path)
minX, minY, maxX, maxY = bbox # 分别提取bbox的值，并保留两位小数
# 打印保留两位小数格式化的结果
print("Spatial extent (minX, minY, maxX, maxY): ({:.2f}, {:.2f}, {:.2f}, {:.2f})".format(minX, minY, maxX, maxY))