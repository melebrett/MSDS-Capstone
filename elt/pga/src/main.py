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

URL = "https://www.pgatour.com/stats/detail/109"

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
    # set up browser (use headless when deployed)

def GoToPage(browser):

    browser.visit(URL)
    time.sleep(1)
    browser.find_by_value('Time Period').click()
    time.sleep(1)
    browser.find_by_value('Tournament Only').click()
    time.sleep(1)

def GetTourneyMoneyFromSoup(soup):
    # find the table in the html
    tab = soup.find("table")

    # get the table headers
    cols = []
    headers = tab.find("thead").find_all("th")
    for header in headers:
        cols.append(header.text)

    # get the table rows
    tab_rows = tab.find_all("tr")

    # collect the data
    all_data = []
    for row in tab_rows:

        # get the table data tags
        tds = row.find_all("td")
        # if no data, skip
        if len(tds) == 0:
            continue
        else:
            try:
                row_data = []
                for col in range(len(cols)):
                    row_data.append(tds[col].text)
                # create dictionary with cols mapped to row_data
                row_dict = dict(zip(cols, row_data))
                all_data.append(row_dict)
            except:
                pass

    df = pd.DataFrame(all_data)

    tourney_tag = soup.find("p", text = lambda text: text and text == "Tournament")
    tourney_tags = tourney_tag.find_parent("div").find_parent("div").find_all("p")
    tournament = tourney_tags[1].text

    season_tag = soup.find("p", text = lambda text: text and text == "Season")
    season_tags = season_tag.find_parent("div").find_parent("div").find_all("p")
    season = season_tags[1].text

    df["Tournament"] = tournament
    df["Season"] = season

    return df

def ScrapeMoney(browser):

    # scrape
    soup = BeautifulSoup(browser.html, 'html.parser')
    # find the div with class like "menu-list"
    menu_lists = soup.find_all("div", class_ = lambda class_: class_ and "menu-list" in class_)

    seasons = []
    season_list = menu_lists[0].find_all("button")
    for season in season_list:
        seasons.append(season.text)

    seasons = seasons[:13]
    all_dfs = []
    counter = 0
    for season in seasons:
        print(season)

        try:
            if counter > 0:
                # click the dropdown for the desired season
                browser.find_by_value('Season').click()
                browser.find_by_value(season).click()
                time.sleep(5)
                
                # scrape the page
                soup = BeautifulSoup(browser.html, 'html.parser')
                menu_lists = soup.find_all("div", class_ = lambda class_: class_ and "menu-list" in class_)
                
            # find all the tournaments
            tournaments = []
            tourney_list = menu_lists[2].find_all("button")
            for element in tourney_list:
                tournaments.append(element.text)
                
            # loop through each tournament and scrape the table
            for tourney in tournaments:
                try:
                    # print(f"getting data for {tourney.upper()}...")
                    
                    browser.find_by_value('Tournament').click()
                    browser.find_by_value(tourney).click()
                    time.sleep(5)

                    soup = BeautifulSoup(browser.html, 'html.parser')

                    df = GetTourneyMoneyFromSoup(soup)

                    all_dfs.append(df)
                except:
                    print(f"ERROR: could not get data for {tourney.upper()}")
        except:
            print(f"ERROR: could not scrape {season}")
        
        counter += 1

    result = pd.concat(all_dfs)
    result["Money"] = result["Money"].str.replace("$", "").str.replace(",", "").astype(int)

    return result


def main():
    # launch the browser
    browser = launchBrowser()

    # go to the page
    GoToPage(browser)

    # scrape the data
    result_df = ScrapeMoney(browser)

    # close the browser
    browser.quit()

    # write to db
    utils.writeToDB(result_df, "earnings")

    print("Done!")

if __name__ == "__main__":
    try:
        main()
    except Exception as e:
        print(e)
        sys.exit(1)