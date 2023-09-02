from pathlib import Path
import xarray as xr
import numpy as np
from scipy.interpolate import griddata
import os
from shapely.geometry import Point, Polygon
from shapely.ops import unary_union
import geopandas as gpd
from netCDF4 import Dataset
from osgeo import gdal
import matplotlib.pyplot as plt
import pandas as pd
import warnings
warnings.filterwarnings("ignore")

# Filepaths
path = "/Users/garyschlauch/Library/CloudStorage/Box-Box/2nd_year_paper"
path_data = path + "/data"
path_int = path_data + "/generated/intermediate"
path_int_viirs = path_int + "/viirs"
path_int_grid = path_int + "/grid"
path_int_temp = path_int + "/temp"

# Load the uniform grid shapefile
gdf_grid = gpd.read_file(path_int_grid + "/grid_latlong_0p1_degree.shp")

# Set the desired product
product = "detection"
#product = "saai"

# Function to process the VIIRS data for binary dust detection
def process_viirs_adp_detection(ds):
    
    #
    # inputs
    # ds: an xarray
    #
    # outputs
    # dust_detection:
    # latitude_array
    # longitude_array
    #

    # Convert xarray Data Arrays to NumPy masked arrays w/correct dtype
    # Select "dust present" (Dust = 1) pixels
    dust_detection = ds.Dust.where(ds.Dust == 1).to_masked_array().astype('int8')
    pqi2 = ds.PQI2.to_masked_array().astype('int8')

    # Select dust pixels outside of sun-glint areas using "PQI2" bit 1
    # outside sun-glint = 0, within sun-glint = 2   
    dust_detection = np.ma.masked_where(pqi2 & 2 == 2, dust_detection)

    # latitude and longitude maps
    latitude_array = ds.Dust.Latitude.to_numpy()
    longitude_array = ds.Dust.Longitude.to_numpy()

    return dust_detection, latitude_array, longitude_array







def process_viirs_adp_saai(ds):

    # Convert xarray Data Arrays to NumPy masked arrays w/correct dtype
    dust = ds.Dust.to_masked_array().astype('int8')
    pqi2 = ds.PQI2.to_masked_array().astype('int8')
    pqi4 = ds.PQI4.to_masked_array().astype('int8')
    saai = ds.SAAI.to_masked_array().astype('float32')
    qc_flag = ds.QC_Flag.to_masked_array().astype('int8')
    
    # Select dust pixels using "Dust" (mask pixels with dust absent)
    # dust present = 1, smoke absent = 0
    saai_dust = np.ma.masked_where(dust == 0, saai)
    
    # Select dust pixels outside of sun-glint areas using "PQI2" bit 1
    # outside sun-glint = 0, within sun-glint = 2
    saai_dust = np.ma.masked_where(pqi2 & 2 == 2, saai_dust)
    
    # Select deep-blue based algorithm dust pixels using "PQI4" bits 6-7
    # Mask missing and IR-visible path
    # deep blue (00): 0 + 0 = 0, missing (10): 64 + 0 = 64, IR-visible (01): 0 + 128 = 128, both (11): 64 + 128 = 128
    dust_algorithm_mask = ((pqi4 & 64 == 64) & (pqi4 & 128 != 128)) | ((pqi4 & 64 != 64) & (pqi4 & 128 == 128))
    saai_dust = np.ma.masked_where(dust_algorithm_mask, saai_dust)

    # latitude and longitude maps
    latitude_array = ds.Dust.Latitude.to_numpy()
    longitude_array = ds.Dust.Longitude.to_numpy()

    return saai_dust, latitude_array, longitude_array












# Function to run process_viirs_adp_detection on all files for a given day and
# extract the lat/lons of the dust pixels onto a uniform grid shapefile
def process_viirs_adp_detection_daily(gdf_grid, path_in, path_out):
    
    #
    # inputs
    # gdf_grid: a gridded shapefile with WGS84 datum lat/lon with grid cell numbers
    #      stored in a column
    # path_in: the full filepath of the folder containing the VIIRS .nc files
    # path_out: the full filepath of the directory where the dataframe will output
    #
    # outputs
    # df_intersections: a DataFrame with the grid cell numbers of gdf for grid
    #                   cells that contain any dust pixel on a given day
    #

    # Initialize empty arrays to store the latitudes/longitudes with dust
    lat_dust_all = np.array([])
    lon_dust_all = np.array([])
    
    for file_name in os.listdir(path_in):
        
        if file_name.endswith(".nc"):
            
            # Set full path for ADP data file
            file_id = os.path.join(path_in, file_name)
    
            # Open file using xarray (automatically closes file when done)
            with xr.open_dataset(file_id, engine='netcdf4') as ds:
                
                if product == "detection":
                    dust, lat, lon = process_viirs_adp_detection(ds)
                if product == "saai":
                    dust, lat, lon = process_viirs_adp_saai(ds)
            dust.fill_value = 0
            dust = dust.filled()
            
            
            # Convert SAAI to binary
            if product == "saai":
                dust[dust != 0] = 1
            
            # Get the latitudes and longitudes where there is dust
            lat_dust = lat[np.where(dust == 1)]
            lon_dust = lon[np.where(dust == 1)]
            assert len(lat_dust) == len(lon_dust)
            
            # Append these latitudes and longtidues to the lists
            if len(lat_dust) > 0:
                lat_dust_all = np.concatenate((lat_dust_all, lat_dust))
                lon_dust_all = np.concatenate((lon_dust_all, lon_dust))
    
    # Create a DataFrame from the latitude and longitude arrays
    df = pd.DataFrame({'Latitude': lat_dust_all, 'Longitude': lon_dust_all})
    df = df.drop_duplicates(subset=['Latitude', "Longitude"], keep='first')
    
    # Convert the DataFrame to a GeoDataFrame with Point geometries
    gdf_points = gpd.GeoDataFrame(df, geometry=gpd.points_from_xy(df.Longitude, df.Latitude))
    gdf_points.crs = 'EPSG:4326'
    
    # Perform the spatial join
    gdf_intersections = gpd.sjoin(gdf_points, gdf_grid, op='intersects')
    
    # Convert to a dataframe, keeping only the gdf_grid cell numbers that contain
    # dust
    df_intersections = pd.DataFrame(gdf_intersections)
    df_intersections = df_intersections.drop_duplicates(subset=['ID'], keep='first')
    df_intersections['dust'] = 1
    df_intersections = df_intersections[['ID', 'dust']]
    
    # Output
    filepath_out = path_out + "/grid_latlong_0p1_degree_dust_cells.csv"
    df_intersections.to_csv(filepath_out, index=False)
    
# Main function
if __name__ == "__main__":
 
    # List/download files
    process_viirs_adp_detection_daily(gdf_grid, path_int_temp, path_int_viirs)




