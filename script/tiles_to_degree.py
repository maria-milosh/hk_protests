import math, os, re

def num2deg(zoom, xtile, ytile):
  n = 2.0 ** zoom
  lon_deg = xtile / n * 360.0 - 180.0
  lat_rad = math.atan(math.sinh(math.pi * (1 - 2 * ytile / n)))
  lat_deg = math.degrees(lat_rad)
  return (lat_deg, lon_deg)
# Use the function with xtile+1 and/or ytile+1 to get the other corners

tiles = {}
tiles['tileinfo'] = []

for map in os.listdir("/Users/mariamilosh/Dropbox/HK/MapTiles/"):

	print(map)

	coords = re.sub('^(.*?)_|.png', '', map).split('_')
	coords = [int(x) for x in coords]


	tiles['tileinfo'].append(
		{'filename': map,
		'zxy': coords, 
		'topleft': num2deg(coords[0], coords[1], coords[2]),
		'topright': num2deg(coords[0], coords[1] + 1, coords[2]),
		'botleft': num2deg(coords[0], coords[1], coords[2] + 1),
		'botright': num2deg(coords[0], coords[1] + 1, coords[2] + 1) })

# check above 

with open('/Users/mariamilosh/Dropbox/HK/MapTilesInfo/MapTilesInfo_full.txt', 'w') as outfile:
	json.dump(tiles, outfile)


