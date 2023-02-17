# Scraping-a-Bandcamp-collection
The code here uses RSelenium to scrape data from a Bandcamp collection. 

This was built on Chrome. Chrome drivers/versions may need to be updated (I get mine from here https://chromedriver.chromium.org/downloads), file paths will need to be carefully evaluated (I tried making anything needing to be changed prefixed and suffixed with <>). My Bandcamp collection is ~600 albums and this scaled pretty well with that (no timeouts from the site, good performance), but I don't know how it'd work with very large collections. 
