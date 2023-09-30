from orbnav_client import Client  # Module to access OrbNav API to get Boxtimes
from packaging.version import parse
import sys
from tqdm import tqdm  # Library to create progress bars for loops/functions
from datetime import datetime, date
import s3fs  # Module to interface with Amazon Simple Storage Service (S3)
import xarray as xr
import numpy as np
import os
import geopandas as gpd
import pandas as pd
import warnings

warnings.filterwarnings("ignore")
orbnav = Client()

# Boxtimes documentation: https://sips.ssec.wisc.edu/orbnav/api/v1
# NOAA code for downloading files from AWS: https://www.star.nesdis.noaa.gov/atmospheric-composition-training/python_viirs_level2_download.php
# AWS folder: https://noaa-jpss.s3.amazonaws.com/index.html#SNPP/VIIRS/SNPP_VIIRS_Aerosol_Detection_EDR_Reprocessed/
# Boxtimes online interface: https://sips.ssec.wisc.edu/orbnav#/tools/boxtimes
# JSTAR Mapper: https://www.star.nesdis.noaa.gov/jpss/mapper/
# SNPP satellite passing through bounding box visual: https://www.ssec.wisc.edu/datacenter/polar_orbit_tracks/data/NPP/

# Setup -----------------------------------------------------------------------

# Set filepaths
path = "/Users/garyschlauch/Library/CloudStorage/Box-Box/Dust-Pollution"
path_data = path + "/data"
path_int = path_data + "/generated/intermediate"
path_int_viirs = path_int + "/viirs"
path_int_grid = path_int + "/grid"
path_int_temp = path_int + "/temp"

# Set boxtimes parameters (don't change)
lonmin = -135
lonmax = -60
latmin = 5
latmax = 71
satname_boxtimes = "SUOMI NPP"
satname_aws = "SNPP"
product_aws = "ADP"
processing_aws = "reprocessed"

# Set the desired dust product
product = "detection"  # binary dust plume detection
# product = "saai" # dust plume detection/intensity

# Load the uniform grid shapefile
gdf_grid = gpd.read_file(path_int_grid + "/grid_latlong_0p1_degree.shp")

# Define functions ------------------------------------------------------------


# Create list of times the VIIRS satellite enters and exits the specified bounding box on a given day
def boxtimes_viirs_list(
    satname_boxtimes, year, month, day, lonmin, lonmax, latmin, latmax
):
    #
    # inputs
    # satname_boxtimes: (str) the Boxtimes satellite name
    # year: (int) the year
    # month: (int) the month of the year
    # day: (int) the day of the month
    # lonmin: (int) the minimum longitude in the bounding box
    # lonmax: (int) the maximum longitude in the bounding box
    # latmin: (int) the minimum latitude in the bounding box
    # latmin: (int) the maximum latitude in the bounding box
    #
    # outputs
    # df: (dataframe) dataframe where each row corresponds to the start/end hour and minute that the satellite enters/exits
    #     the bounding box defined by the lat/lon variables
    # n: (int) the number of separate times the satellite enters the bounding box
    #
    
    print("Getting boxtimes for " + str(year).zfill(2) + "/" + str(month).zfill(2) + "/" + str(day).zfill(2))
    dict_times = orbnav.boxtimes(
        sat=satname_boxtimes,
        start=datetime(year, month, day, 0, 0, 0),
        end=datetime(year, month, day, 23, 59, 59),
        ur=(latmax, lonmax),
        ll=(latmin, lonmin),
    )

    # Store the start-end times from Boxtimes
    data = dict_times["data"]
    n = len(data)

    # Loop through start-end time pairs from Boxtimes
    df = pd.DataFrame(
        columns=["start_hour", "start_min", "end_hour", "end_min"], index=range(n)
    )
    for i in range(n):
        # Get the start and end datetimes
        start_boxtime = data[i][0][0]
        end_boxtime = data[i][1][0]

        # Check that the start and end year, month, and day are the same
        assert (
            start_boxtime.year == end_boxtime.year
        ), "Boxtime start and end years don't match"
        assert (
            start_boxtime.month == end_boxtime.month
        ), "Boxtime start and end years don't match"
        assert (
            start_boxtime.day == end_boxtime.day
        ), "Boxtime start and end years don't match"

        # Get start and end hour and minute for a given pair
        df.loc[i].at["start_hour"] = str(start_boxtime.hour).zfill(2)
        df.loc[i].at["start_min"] = str(start_boxtime.minute).zfill(2)
        df.loc[i].at["end_hour"] = str(end_boxtime.hour).zfill(2)
        df.loc[i].at["end_min"] = str(end_boxtime.minute).zfill(2)

    return df, n


# Construct VIIRS aerosol product path for AWS
def get_product_path(product_aws, processing_aws, satname_aws):
    #
    # inputs
    # product_aws: (str) the VIIRS product name
    # processing_aws: (str) whether the operational or reprocessed data are to be used
    # satname_aws: (str) the VIIRS AWS satellite name
    #
    # outputs
    # product_path: (str) the folder path in AWS to the desired product
    #

    # Set AWS NODD product name
    if product_aws == "AOD":
        product_name = "Aerosol_Optical_Depth"
    elif product_aws == "ADP":
        product_name = "Aerosol_Detection"

    # Set AWS NODD processing suffix
    if processing_aws == "reprocessed":
        suffix = "_EDR_Reprocessed"
    elif processing_aws == "operational":
        suffix = "_EDR"

    # Create AWS NOD product path name
    product_path = satname_aws + "_VIIRS_" + product_name + suffix

    return product_path


# Create list of available VIIRS aerosol data file names for user-specified satellite/product/processing & observation date/time
def aws_viirs_list(
    year_str,
    month_str,
    day_str,
    start_hour,
    start_min,
    end_hour,
    end_min,
    satname_aws,
    product_aws,
    processing_aws,
):
    #
    # inputs
    # year: (str) the year
    # month: (str) the month of the year with a leading 0 if needed
    # day: (str) the day of the month with a leading 0 if needed
    # start_hour: (str) the hour the satellite enters the bounding box on a given pass
    # start_min: (str) the minute the satellite enters the bounding box on a given pass
    # end_min: (str) the minute the satellite exits the bounding box on a given pass
    # end_hour: (str) the hour the satellite exits the bounding box on a given pass
    # satname_aws: (str) the VIIRS AWS satellite name
    # product_aws: (str) the VIIRS product name
    # processing_aws: (str) whether the operational or reprocessed data are to be used
    #
    # outputs
    # data: (str) the list of files in AWS with start/end times that place them within the bounding box
    #

    # Construct aerosol product path for AWS NODD
    product_path = get_product_path(product_aws, processing_aws, satname_aws)

    # Access AWS using anonymous credentials
    aws = s3fs.S3FileSystem(anon=True)

    # Create list of file names for entire day (~550)
    day_files = aws.ls(
        "noaa-jpss/"
        + satname_aws
        + "/VIIRS/"
        + product_path
        + "/"
        + year_str
        + "/"
        + month_str
        + "/"
        + day_str
        + "/",
        refresh=True,
    )

    # Create list of subsetted file names that fall within specified time period(s)
    data = []
    for file in day_files:
        # Get the file start hour and minute
        file_time = file.split("_")[-3][9:13]
        # Filter the file start time based on the start and end hour of the satellite
        # entering/exiting the bounding box
        if file_time >= (start_hour + start_min) and file_time <= (end_hour + end_min):
            # Check that the file starts on the correct date
            assert file.split("_")[-3][1:9] == year_str + month_str + day_str
            data.append(file)
    return data


# Print available VIIRS aerosol data files that match user specifications, with option to download files
# "path_out": parameter variable assigned in main function
def get_viirs_files(
    year,
    month,
    day,
    satname_boxtimes,
    lonmin,
    lonmax,
    latmin,
    latmax,
    satname_aws,
    product_aws,
    processing_aws,
    path_out,
):
    #
    # inputs
    # year: (int) the year
    # month: (int) the month of the year
    # day: (int) the day of the month
    # satname_boxtimes: (str) the Boxtimes satellite name
    # lonmin: (int) the minimum longitude in the Boxtimes  bounding box
    # lonmax: (int) the maximum longitude in the Boxtimes bounding box
    # latmin: (int) the minimum latitude in the Boxtimes bounding box
    # latmin: (int) the maximum latitude in the Boxtimes bounding box
    # product_aws: (str) the VIIRS product name
    # processing_aws: (str) whether the operational or reprocessed data are to be used
    # satname_aws: (str) the VIIRS AWS satellite name
    # path_out: (str) the folder path where the files will be saved
    #
    # outputs
    # downloads files
    #
    
    # Assert that there are no files in the directory where the .nc files from
    # AWS will be temporarily stored
    assert len(os.listdir(path_out)) == 0, "Temporary directory should be empty"

    # Query Boxtimes to get start/end times for when satellite enters/exits the bounding box (can happen multiple times in a day)
    boxtimes_df, n = boxtimes_viirs_list(
        satname_boxtimes, year, month, day, lonmin, lonmax, latmin, latmax
    )

    # Convert the year, month, and day to strings for the aws_viirs_list function
    year_str = str(year).zfill(2)
    month_str = str(month).zfill(2)
    day_str = str(day).zfill(2)

    # Initialize list to store VIIRS AWS filenames
    file_list = []

    # Loop through rows of Boxtimes dataframe, which corresponds to separate periods when the satellite enters/exits the bounding box
    for i in range(n):
        # Get the Boxtimes start-end hours and minutes
        start_hour = boxtimes_df.loc[i].at["start_hour"]
        start_min = boxtimes_df.loc[i].at["start_min"]
        end_hour = boxtimes_df.loc[i].at["end_hour"]
        end_min = boxtimes_df.loc[i].at["end_min"]

        # Get the AWS file names for this start-end time period
        files = aws_viirs_list(
            year_str,
            month_str,
            day_str,
            start_hour,
            start_min,
            end_hour,
            end_min,
            satname_aws,
            product_aws,
            processing_aws,
        )

        # Add these file names to the list
        if files:
            file_list = file_list + files

    if len(file_list) > 0:
        # Access AWS using anonymous credentials
        aws = s3fs.S3FileSystem(anon=True)

        # Display progress bar using tqdm library
        # Flush buffer if Python version < v3.9 to avoid glitch in tqdm library
        if parse(sys.version.split(" ")[0]) < parse("3.9"):
            sys.stdout.flush()
        print("Downloading files for", year_str + "/" + month_str + "/" + day_str)
        for name in tqdm(
            file_list,
            unit="files",
            bar_format="{desc}Downloading:{percentage:3.0f}%|{bar}|{n_fmt}/{total_fmt} [{elapsed}<{remaining}]",
        ):
            # Set path_out + file_name as pathlib.Path object and convert to string
            full_path = path_out + "/" + name.split("/")[-1]

            # Download file from AWS archive
            aws.get(name, full_path)
        print("\nDownload complete!")


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
    dust_detection = ds.Dust.where(ds.Dust == 1).to_masked_array().astype("int8")
    pqi2 = ds.PQI2.to_masked_array().astype("int8")

    # Select dust pixels outside of sun-glint areas using "PQI2" bit 1
    # outside sun-glint = 0, within sun-glint = 2
    dust_detection = np.ma.masked_where(pqi2 & 2 == 2, dust_detection)

    # latitude and longitude maps
    latitude_array = ds.Dust.Latitude.to_numpy()
    longitude_array = ds.Dust.Longitude.to_numpy()

    return dust_detection, latitude_array, longitude_array


def process_viirs_adp_saai(ds):
    # Convert xarray Data Arrays to NumPy masked arrays w/correct dtype
    dust = ds.Dust.to_masked_array().astype("int8")
    pqi2 = ds.PQI2.to_masked_array().astype("int8")
    pqi4 = ds.PQI4.to_masked_array().astype("int8")
    saai = ds.SAAI.to_masked_array().astype("float32")
    # qc_flag = ds.QC_Flag.to_masked_array().astype("int8")

    # Select dust pixels using "Dust" (mask pixels with dust absent)
    # dust present = 1, smoke absent = 0
    saai_dust = np.ma.masked_where(dust == 0, saai)

    # Select dust pixels outside of sun-glint areas using "PQI2" bit 1
    # outside sun-glint = 0, within sun-glint = 2
    saai_dust = np.ma.masked_where(pqi2 & 2 == 2, saai_dust)

    # Select deep-blue based algorithm dust pixels using "PQI4" bits 6-7
    # Mask missing and IR-visible path
    # deep blue (00): 0 + 0 = 0, missing (10): 64 + 0 = 64, IR-visible (01): 0 + 128 = 128, both (11): 64 + 128 = 128
    dust_algorithm_mask = ((pqi4 & 64 == 64) & (pqi4 & 128 != 128)) | (
        (pqi4 & 64 != 64) & (pqi4 & 128 == 128)
    )
    saai_dust = np.ma.masked_where(dust_algorithm_mask, saai_dust)

    # latitude and longitude maps
    latitude_array = ds.Dust.Latitude.to_numpy()
    longitude_array = ds.Dust.Longitude.to_numpy()

    return saai_dust, latitude_array, longitude_array


# Function to run process_viirs_adp_detection on all files for a given day and
# extract the lat/lons of the dust pixels onto a uniform grid shapefile
def process_viirs_adp_detection_daily(gdf_grid, path_in, path_out, year, month, day):
    #
    # inputs
    # gdf_grid: a gridded shapefile with WGS84 datum lat/lon with grid cell numbers
    #      stored in a column
    # path_in: the full filepath of the folder containing the VIIRS .nc files
    # path_out: the full filepath of the directory where the dataframe will output
    # year: (int) the year
    # month: (int) the month of the year
    # day: (int) the day of the month
    #
    # outputs
    # df_intersections: a DataFrame with the grid cell numbers of gdf for grid
    #                   cells that contain any dust pixel on a given day
    #
    
    # Store the ymd as strings
    year_str = str(year).zfill(2)
    month_str = str(month).zfill(2)
    day_str = str(day).zfill(2)
    print("Processing files for " + year_str + "/" + month_str + "/" + day_str, end = "")

    # Initialize empty arrays to store the latitudes/longitudes with dust
    lat_dust_all = np.array([])
    lon_dust_all = np.array([])

    for file_name in os.listdir(path_in):
        if file_name.endswith(".nc"):
            # Set full path for ADP data file
            file_id = os.path.join(path_in, file_name)

            # Open file using xarray (automatically closes file when done)
            with xr.open_dataset(file_id, engine="netcdf4") as ds:
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
    df = pd.DataFrame({"Latitude": lat_dust_all, "Longitude": lon_dust_all})
    df = df.drop_duplicates(subset=["Latitude", "Longitude"], keep="first")

    # Convert the DataFrame to a GeoDataFrame with Point geometries
    gdf_points = gpd.GeoDataFrame(
        df, geometry=gpd.points_from_xy(df.Longitude, df.Latitude)
    )
    gdf_points.crs = "EPSG:4326"

    # Perform the spatial join
    gdf_intersections = gpd.sjoin(gdf_points, gdf_grid, op="intersects")

    # Convert to a dataframe, keeping only the gdf_grid cell numbers that contain
    # dust
    df_intersections = pd.DataFrame(gdf_intersections)
    df_intersections = df_intersections.drop_duplicates(subset=["ID"], keep="first")
    df_intersections["dust"] = 1
    df_intersections = df_intersections[["ID", "dust"]]

    # Output
    filepath_out = path_out + "/" + year_str + "/" + "grid_latlong_0p1_degree_dust_" + year_str + month_str + day_str + ".csv"
    df_intersections.to_csv(filepath_out, index=False)
    print("Processing complete!\n")

def delete_files_in_directory(dirpath):
    filelist = [f for f in os.listdir(dirpath)]
    for f in filelist:
        os.remove(os.path.join(dirpath, f))


# Run script ------------------------------------------------------------------

# Set the start and end date (inclusive)
start_date = date(2022, 7, 2)
end_date = date(2022, 12, 31)
daterange = pd.date_range(start_date, end_date)

# Main function
if __name__ == "__main__":
    
    # Loop through dates from start_date to end_date (inclusive)
    for single_date in daterange:
            
        # Delete all files in the temporary directory where the .nc files are stored
        delete_files_in_directory(path_int_temp)
        
        # Get the year, month, and day
        year = single_date.year
        month = single_date.month
        day = single_date.day
        
        # Download .nc files for a given day
        get_viirs_files(
            year,
            month,
            day,
            satname_boxtimes,
            lonmin,
            lonmax,
            latmin,
            latmax,
            satname_aws,
            product_aws,
            processing_aws,
            path_int_temp,
        )
    
        # Process files for that day and output a single .csv containing the 
        # grid cells of gdf_grid containing any dust on that day
        process_viirs_adp_detection_daily(gdf_grid, path_int_temp, path_int_viirs, year, month, day)
    
    # Delete the files in the temporary directory one last time
    delete_files_in_directory(path_int_temp)
    
    
    
    
    
