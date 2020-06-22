

from bs4 import BeautifulSoup
import re, time, requests, getpass, datetime
from selenium import webdriver
from selenium.webdriver.common.keys import Keys
import os, time

user = getpass.getuser()


driver = webdriver.Chrome('/Users/' + user + '/Desktop/chromedriver')

driver.get('https://hkmap.live/')
time.sleep(5)

starttime = time.time()
while True:
    soup = BeautifulSoup(driver.page_source, 'html.parser')

    f = open(f"/Users/mariamilosh/Dropbox/HK/HTMLs/{datetime.datetime.fromtimestamp(time.time()).strftime('%Y-%m-%d-%H-%M')}.txt",'w')
    f.write(str(soup))
    f.close()
    
    time.sleep(60.0 - ((time.time() - starttime) % 60.0))


