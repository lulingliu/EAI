import os
import numpy as np
from osgeo import gdal
from glob import glob
import matplotlib
import matplotlib.pyplot as plt
matplotlib.use('TkAgg')  # Replace 'TkAgg' with another backend if needed

# ----------------------------------------------------------------------------

def read_tiff_file(path):
    raster_dataset = gdal.Open(path)
    band = raster_dataset.GetRasterBand(1)
    array = band.ReadAsArray()
    return array




def calculate_percentiles(input_data, percentiles):
    # Filter the input data to remove non-positive values
    filtered_array = input_data[input_data > 0]

    # Calculate the percentiles using NumPy
    result = np.percentile(filtered_array, q=percentiles)

    return result




def plot_histogram_with_percentiles(input_data, percentiles, input_path,output_path,filename):
    # Filter the input data to remove non-positive values
    filtered_array = input_data[input_data > 0]

    # Calculate the maximum value of the filtered data
    # max_value = np.max(filtered_array)

    # # Set the x-axis limits to show data around the maximum value
    # x_max = max_value + 10

    # Plot the histogram
    plt.figure(figsize=(8, 6))  # Adjust the figure size as needed
    plt.hist(filtered_array, bins=200, range=(0, 110))  # Adjust the number of bins as needed

    # Add line labels for the percentiles
    for percentile in percentiles:
        percentile_value = np.percentile(filtered_array, q=percentile)
        plt.axvline(percentile_value, color='red', linestyle='--', linewidth=1)

        # Add text labels
        plt.text(percentile_value + 0.02 * (plt.xlim()[1] - plt.xlim()[0]),
                 plt.ylim()[1] * 0.9,
                 str(percentile),
                 color='red', fontsize=7, ha='center')

        # Add the percentile_value as text on the x-axis
        formatted_percentile_value = round(percentile_value, 1)
        plt.annotate(f'{formatted_percentile_value:.1f}', xy=(percentile_value, 0), xytext=(0, -2),
                     textcoords='offset points', color='red', ha='center', va='top', fontsize=7)

    # Add labels and title
    plt.xlabel('Pixel Values')
    plt.ylabel('Frequency')

    # year = input_path[-12:-4] # for PCNL

    title = "Histogram with Percentiles: " + filename
    plt.title(title)

    # Save the plot as a JPEG file with 300 dpi resolution
    output_path = output_path
    output_file = output_path + "Histogram_with_percentiles_"+ filename +".jpg"
    plt.savefig(output_file, dpi=300)

    # Show the plot (optional)
    # plt.show()
    print(f"File: {title} has been saved!")




def write_geotiff(fname, data, geo_transform, projection):
    """Create a GeoTIFF file with the given data."""
    
    driver = gdal.GetDriverByName('TIFF')
    rows, cols, nbands = data.shape[:3]
    dataset = driver.Create(fname, cols, rows, nbands, gdal.GDT_Float32)
    dataset.SetGeoTransform(geo_transform)
    dataset.SetProjection(projection)

    if nbands == 1:
        dataset.GetRasterBand(1).WriteArray(data)  # 写入数组数据
    else:
        for i in range(nbands):
            dataset.GetRasterBand(i + 1).WriteArray(data[:, :, i])
    del dataset


# ----------------------------------------------------------------------------




# -------------------------------- Multi files raed in -------------------------


folder_path = 'F:/Research/Inputs_NTL/Aligned_VNLv2/' # Resampled_NPP_VIIRS_VNL2
# folder_path = 'E:\Inputs_NTL\DMSP_CCNL' # CCNL

file_pattern = "*.tif" 

# Get a list of all TIFF file paths in the folder
file_paths = glob(os.path.join(folder_path, file_pattern))


output_path = "F:/Research/"

# Define the percentiles you want to calculate
percentiles = [5, 25, 50, 75, 90, 95]
percentiles_NPP = [5,95]
# ----------------------------------------------------------------------------




# ----------------------------------------------------------------------------

# Loop through each TIFF file and read its data
for file_path in file_paths:
    try:
        file_path_string  = os.path.basename(file_path)
        filename = 'NPP_Aligned_'+ file_path_string[45:49] # for VNLv2_NPP
        # filename = file_path_string[0:14] # for CCNL
 
        # Print the input file name
        # print(f"File: {os.path.basename(file_path)}, title: {filename}, done")
        print(f"File: {os.path.basename(file_path)}")

        # Read the TIFF file
        array = read_tiff_file(file_path)

        # For demonstration, let's print the shape of the array
        print(f"Shape: {array.shape}")

        # Call the function to calculate the percentiles
        # result = calculate_percentiles(array, percentiles)

        # Print the percentiles
        # print(f"percentiles: {percentiles}")
        # print(f"values: {np.round(result,2)}\n")


        # Call the function to plot the histogram with percentiles and save as JPEG
        plot_histogram_with_percentiles(array, percentiles_NPP, file_path, output_path,filename)


    except Exception as e:
        print(f"Error reading {file_path}: {e}")
        
# --------------------------------------------------------------------------