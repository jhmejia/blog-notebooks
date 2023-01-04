import requests, json

def fetchImages(base_url, maximum, res):
  #create an empty list that will contain the urls
  url_list = []
  #amounts of photos per page, it seemed to be capped at 30
  chunk_size = 30
  #fetch images on a given page index using requests
  def fetchChunk(idx):
    #response
    url = '%s?page=%d&per_page=%d' % (base_url, idx, chunk_size)
    #response text
    return requests.get(url).text
  #parse the received chunk from a string to a dictionnary
  def parseChunk(chunk):
    #the json library does the actual parsing
    data = json.loads(chunk)
    #'photos' is the sub-dictionnary containing the images
    images = data['photos']
    #loop through each photo from the page and keep only the url
    for img in images:
      #returns 5 urls, one for each resolution
      img_url = img['urls'][res]
      #add the url to the list
      url_list.append(img_url)
  #the current page index 
  #although negative indices are valid with that api, I will stick to positive ones by convention
  idx = 0
  #continue fetching pages until there's as many or more images than the max amount
  while len(url_list) < maximum:
    #fetch the chunk
    chunk = fetchChunk(idx)
    #parse it
    parseChunk(chunk)
    #increase the index
    idx += 1
  #trim the list so it contains the maximum amount
  url_list = url_list[:maximum]
  return url_list

#you can set that to 'cat' in order to fetch pictures of cat instead
animal = 'dog'

#api endpoint for image list
base = 'https://unsplash.com/napi/landing_pages/images/animals/'
url = base + animal

#resolution can be 'full', 'raw', 'regular', 'small' or 'thumb'
resolution = 'regular'

#the number of images to fetch, the website has a seemingly endless amount of dog pictures, but I would recommend not setting that number to high
#from what I've seen, fetching 2500 takes about 20 seconds, so if you plan on fetching a whole lot of photos, I would recommend using a specialized API for that
maximum = 60

#prints array of urls
print(fetchImages(url, maximum, resolution))