# Web scrape images of cats from the internet

import requests # pip install requests
from bs4 import BeautifulSoup
import os
import selenium # pip install selenium
from selenium import webdriver

driver = webdriver.Chrome()
driver.get('https://your.url/here?yes=brilliant')