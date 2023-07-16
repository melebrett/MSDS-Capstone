import time
import pandas as pd
import numpy as np
import re
import datetime
import json
import random
import sys

import bs4 as bs
from bs4 import BeautifulSoup
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from splinter import Browser
from webdriver_manager.chrome import ChromeDriverManager

# WSL Set Up (Only use this if running from WSL)
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.chrome.service import Service
import os.path

import utils

def launchBrowser():

    chrome_options = Options()
    chrome_options.add_argument("--headless") # Ensure GUI is off
    chrome_options.add_argument("--no-sandbox")
    chrome_options.add_argument("--disable-dev-shm-usage")
    chrome_prefs = {}
    chrome_options.experimental_options["prefs"] = chrome_prefs
    chrome_prefs["profile.default_content_settings"] = {"images": 2}
    # homedir = os.path.expanduser("~")
    # webdriver_service = Service(f"{homedir}/chromedriver/stable/chromedriver")
    webdriver_service = Service("/usr/local/bin/chromedriver")
    browser = Browser('chrome',service=webdriver_service, options=chrome_options)

def GetTableFromSoup(table_html, headers_html, include_href = True):

    colnames = []
    headers = headers_html.find_all("th")
    for header in headers:
        colnames.append(header.text)
    if include_href:
        colnames.append("href")

    data = []
    rows = table_html.find_all("tr")
    for row in rows:
        row.find_all("td")
        # get the href
        cell_data = [ele.text.strip() for ele in row.find_all("td")]
        if include_href:
            
            try:
                href = row.find("a").get("href")
                cell_data.append(href)
            except:
                cell_data.append(None)

        data.append(cell_data)

    data = pd.DataFrame(data, columns = colnames)

    return data

def main():
    browser = launchBrowser()

    ## PLAYER STATS ##
    all_seasons = pd.DataFrame()
    for season in range(2015,2024):
        url = f"https://www.espn.com/golf/stats/player/_/season/{season}"
        browser.visit(url)
        time.sleep(2.5)

        while True:
            # click show more
            try:
                browser.find_by_text('Show More').click()
                time.sleep(1)
            except:
                break

        soup = BeautifulSoup(browser.html, 'html.parser')

        tabs = soup.find_all("table")
        headers = soup.find_all("thead")
        a = GetTableFromSoup(tabs[0], headers[0], include_href=True)
        b = GetTableFromSoup(tabs[1], headers[1], include_href=False)
        # combine a and b into a dataframe
        df = pd.concat([a, b], axis = 1)
        # drop row with none values
        df = df.dropna()
        # add season column
        df['season'] = season

        # concat
        all_seasons = pd.concat([all_seasons, df], axis = 0)

    # convert earnings to numeric
    all_seasons['earnings'] = all_seasons['earnings'].str.replace('$', '').str.replace(',', '').astype(float)
    # rename all to snake case
    all_seasons.columns = [col.lower().replace(' ', '_') for col in all_seasons.columns]

    ## PLAYER BIO ##
    # unique hrefs:
    hrefs = all_seasons['href'].unique()

    player_bio = pd.DataFrame()
    for href in hrefs:
        browser.visit(href)
        try:
            time.sleep(0.2)
            soup = BeautifulSoup(browser.html, 'html.parser')
            # find ul with player header in the class
            bio_elems = soup.find('ul', class_ = re.compile('PlayerHeader__Bio')).find_all('li')
            info = []
            names = []
            for elem in bio_elems:
                divs = elem.find_all('div')
                names.append(divs[0].text.strip())
                info.append(divs[1].text.strip())

            df = pd.DataFrame(info, index = names).T
            df['href'] = href
            player_bio = pd.concat([player_bio, df], axis = 0)

        except:
            pass

    # clean up
    birthdates = player_bio['Birthdate'].str.split(' ')
    cleaned = []
    for birthdate in birthdates:
        try:
            cleaned.append(birthdate[0])
        except:
            cleaned.append(None)
            
    player_bio['Birthdate'] = cleaned
    # extract id from href
    player_bio['espn_id'] = player_bio['href'].str.split('/').str[-2]
    # drop height and weight
    player_bio = player_bio.drop(['Height', 'Weight', 'HT/WT'], axis = 1)
    # rename all to snake case
    player_bio.columns = [col.lower().replace(' ', '_') for col in player_bio.columns]

    # merge all_seasons and player_bio
    all_seasons = all_seasons.merge(player_bio[['href','espn_id']], on = 'href', how = 'left')
    # drop href
    all_seasons = all_seasons.drop('href', axis = 1)

    # write to db
    utils.write_to_db(all_seasons, "espn_stats", append=False)
    utils.write_to_db(player_bio, "espn_bio", append=False)

if __name__ == "__main__":
    try:
        main()
    except Exception as e:
        print(e)
        sys.exit(1)