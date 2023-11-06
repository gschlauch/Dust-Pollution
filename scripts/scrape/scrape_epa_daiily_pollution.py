from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.support.ui import Select
from selenium.webdriver.chrome.service import Service
import os
import time
import shutil
import glob

# Set default download location
download_dir = "/users/garyschlauch/downloads"

# Initialize the WebDriver with the custom options
service = Service("/Users/garyschlauch/Documents/webdrivers/geckodriver")
driver = webdriver.Firefox(service=service)

# Set the URL of the EPA website
url = "https://www.epa.gov/outdoor-air-quality-data/download-daily-data"

# List of pollutants
pollutants = ["PM2.5", "PM10"]

# List of years
years = [str(year) for year in range(2000, 2005)]

# List of states to skip
states_to_skip = ["Alaska", "Hawaii", "Virgin Islands", "Guam", "Puerto Rico"]


# Loop through pollutants
for pollutant in pollutants:
    
    pollutant_name = pollutant.replace(".", "")
    destination_dir = f"/Users/garyschlauch/Library/CloudStorage/Box-Box/Dust-Pollution/data/raw/pollution/epa/concentrations/{pollutant_name}/daily"
    
    # Navigate to the URL
    driver.get(url)
    time.sleep(5)
    
    # Click the pollutant dropdown menu
    element = driver.find_element(By.ID, 'poll')
    driver.execute_script("arguments[0].scrollIntoView();", element)
    pollutant_dropdown = WebDriverWait(driver, 10).until(
        EC.element_to_be_clickable(element)
    )
    pollutant_dropdown.click()
    time.sleep(1)

    # Select <pollutant> from this dropdown menu
    xpath_pollutant = '//*[@id="poll"]/option[text()="' + pollutant + '"]'
    pollutant_option = WebDriverWait(driver, 10).until(
        EC.element_to_be_clickable((By.XPATH, xpath_pollutant))
    )
    pollutant_option.click()
    time.sleep(3)
    
    # Click the title text to make sure the dropdown is closed
    title = driver.find_element(By.XPATH, '//*[@id="main"]/div/div[1]/div[2]/div[1]/article/h1')
    driver.execute_script("arguments[0].scrollIntoView();", title)
    title.click()

    # Loop through years
    for year in years:
        # Click the year dropdown menu
        element = driver.find_element(By.ID, 'year')
        driver.execute_script("arguments[0].scrollIntoView();", element)
        year_dropdown = WebDriverWait(driver, 10).until(
            EC.element_to_be_clickable(element)
        )
        year_dropdown.click()
        time.sleep(1)

        # Select <year> from this dropdown menu
        xpath_year = '//*[@id="year"]/option[text()="' + year + '"]'
        year_option = WebDriverWait(driver, 10).until(
            EC.element_to_be_clickable((By.XPATH, xpath_year))
        )
        year_option.click()
        time.sleep(1)
        driver.execute_script("arguments[0].scrollIntoView();", title)
        title.click()
        time.sleep(4)
        
        # Get the available states
        dropdown = driver.find_element(By.ID, 'state')
        selector = Select(dropdown)
        options = selector.options

        # Loop through states
        for i in range(1, len(options)):
            
            # Get the state name
            state = options[i].text
            
            if (state in states_to_skip):
                continue
            else:
                
                print(f'Downloading for {pollutant}, {year}, {state}')
                
                # Click the state dropdown menu
                element = driver.find_element(By.ID, 'state')
                driver.execute_script("arguments[0].scrollIntoView();", element)
                state_dropdown = WebDriverWait(driver, 10).until(
                    EC.element_to_be_clickable(element)
                )
                state_dropdown.click()
                time.sleep(1)

                # Select <state> from this dropdown menu
                xpath_state = '//*[@id="state"]/option[text()="' + state + '"]'
                state_option = WebDriverWait(driver, 10).until(
                    EC.element_to_be_clickable((By.XPATH, xpath_state))
                )
                state_option.click()
                time.sleep(1)
                title.click()
                time.sleep(4)

                # Click "Get Data" to generate the .csv
                xpath = "/html/body/div[2]/main/div/div[1]/div[2]/div[1]/article/div[2]/div/div[2]/input"
                element = driver.find_element(By.XPATH, xpath)
                driver.execute_script("arguments[0].scrollIntoView();", element)
                get_data_button = WebDriverWait(driver, 10).until(
                    EC.element_to_be_clickable((By.XPATH, xpath))
                )
                get_data_button.click()
                time.sleep(5)

                # Download the .csv
                download_button = WebDriverWait(driver, 45).until(
                    EC.element_to_be_clickable(
                        (
                            By.XPATH,
                            "/html/body/div[2]/main/div/div[1]/div[2]/div[1]/article/div[2]/div/div[3]/p/a",
                        )
                    )
                )
                download_button.click()

                # Wait until the file is downloaded
                time.sleep(15)
                while any(glob.glob("/users/garyschlauch/downloads/*.csv.part")):
                    time.sleep(1)
                time.sleep(1)

                # Rename the file based on above parameters
                new_file_name = f"{pollutant_name}_daily_{state}_{year}.csv"
                os.rename(
                    os.path.join(download_dir, "ad_viz_plotval_data.csv"),
                    os.path.join(download_dir, new_file_name),
                )
                while not os.path.exists(os.path.join(download_dir, new_file_name)):
                    time.sleep(0.05)

                # Move the renamed file to the appropriate downloads folder location
                shutil.move(
                    os.path.join(download_dir, new_file_name),
                    os.path.join(destination_dir, new_file_name),
                )
                while not os.path.exists(os.path.join(destination_dir, new_file_name)):
                    time.sleep(0.05)


# Close the browser
driver.quit()         
                    
                    