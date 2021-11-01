import pandas
import geopandas
import os
import csv

shapeDir = "covidcast/Python-packages/covidcast-py/covidcast/shapefiles/county"
shapeFilename = "cb_2019_us_county_5m.dbf"
popdataCsvFilename = 'counties.csv'

popdata = []
with open(popdataCsvFilename) as f:
    reader = csv.DictReader(f)
    for row in reader:
        popdata.append(row)

df = geopandas.read_file(os.path.join(shapeDir, shapeFilename))
massData = df.query('STATEFP == "25"')
nameAndGeomData = dict(zip(massData['NAME'], zip(massData['COUNTYFP'], massData['geometry'])))
print(massData['COUNTYFP'])
print('\n'.join([f'{n}: {str(s[1].centroid)}' for n, s in nameAndGeomData.items()]))

gravity = {}
for county1 in nameAndGeomData:
    for county2 in nameAndGeomData:
        if county1 == county2 or (county2, county1) in gravity:
            continue
        countyFips1, shape1 = nameAndGeomData[county1]
        countyFips2, shape2 = nameAndGeomData[county2]
        c1 = shape1.centroid
        c2 = shape2.centroid
        # 25 is the FIPS prefix for massachusetts
        pop1Matches = list(filter(lambda r: r['fips'] == "25"+countyFips1, popdata))
        pop2Matches = list(filter(lambda r: r['fips'] == "25"+countyFips2, popdata))
        assert len(pop1Matches) == 1, f'Should only be 1; len is {len(pop1)}'
        assert len(pop2Matches) == 1, f'Should only be 1; len is {len(pop2)}'
        pop1 = pop1Matches[0]['population']
        pop2 = pop2Matches[0]['population']
        # EPSG:4326 is a metric for distance based on latitude/longitude
        points_df = geopandas.GeoDataFrame({'geometry': [c1, c2]}, crs='EPSG:4326')
        # 26986 has something to do with massachusetts
        points_df = points_df.to_crs('EPSG:26986')
        points_df2 = points_df.shift() #We shift the dataframe by 1 to align pnt1 with pnt2
        dist = points_df.distance(points_df2)
        #print(f"Distance between {county1} ({str(c1)}) county and {county2} ({str(c2)}) county is {dist[1]}")
        gravity[(county1, county2)] = (float(pop1)*float(pop2))/(dist[1]**2)

#Normalize
maxGravity = max(gravity.values())
for counties in gravity:
    gravity[counties] *= 1/maxGravity

gravity = sorted(gravity.items(), key=lambda item: item[1], reverse=True)
print('\n'.join([str(s) for s in gravity]))

