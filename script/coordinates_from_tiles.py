import mercantile
import json
import os
import re
import pandas as pd


tiles = {}
tiles['tileinfo'] = []

for map in os.listdir("HK/MapTiles/"):

	print(map)

	coords = re.sub('^(.*?)_|.png', '', map).split('_')
	coords = [int(x) for x in coords]

	tiles['tileinfo'].append(
		{'filename': map,
		'zxy': coords, 
		'topleft': mercantile.ul(coords[1], coords[2], coords[0]),
		'bottomleft': mercantile.ul(coords[1], coords[2] + 1, coords[0]),
		'topright': mercantile.ul(coords[1] + 1, coords[2], coords[0]),
		'botright': mercantile.ul(coords[1] + 1, coords[2] + 1, coords[0]),
		'bbox': mercantile.xy_bounds(coords[1], coords[2], coords[0])})

with open('HK/MapTilesInfo/MapTilesInfo.txt', 'w') as outfile:
	json.dump(tiles, outfile)