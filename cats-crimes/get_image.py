import time
from selenium import webdriver
from selenium.webdriver.support.ui import WebDriverWait
from bs4 import BeautifulSoup


cat_images = []

baseurl = "https://pixabay.com/photos/search/house%20cat/"


driver = webdriver.Chrome()
driver.get(baseurl)
time.sleep(1)
html = driver.page_source


soup = BeautifulSoup(html, 'html.parser') 

for item in soup.find_all('img'):
    cat_images.append(item['src'])

#Remove all gifs, they are not images
cat_images = [x for x in cat_images if x.endswith('.jpg')]

# Print the list of cat images
print(cat_images)

