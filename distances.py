import requests 
import csv
import sqlite3
import time 

key = 'get ORS key and put here'

file = 'data/SNOW_TAG_TOW_TYPES.csv'

def get_driving(start):  # example start = '-95.4545,45.343'

    # https://api.openrouteservice.org/v2/directions/driving-car?api_key=KEY&start=-93.681495,45.41461&end=-94.687872,46.420318
    distance_url = 'https://api.openrouteservice.org/v2/directions/driving-car'

    end = '-93.291796,44.977125'  # impound lot 

    params = { 'api_key' : key , 'start': start, 'end': end }

    try: 
        response = requests.get( distance_url, params = params).json()
        return response['features'][0]['properties']['summary']

    except Exception as e:
        print(e)
        return None

    print(response)


# data = get_driving('-93.000,45')
# print(data)


# Read in the whole thing

counter = 1

with open(file) as csvfile:
    reader = csv.reader(csvfile, delimiter=',')
    header = reader.__next__() 
    rows = list(reader)
    print(rows)

    try:

        for row in rows:

            print()
            distance = row[15]
            drivetime = row[16]
            if distance and drivetime:
                print('has distance and time', row)
                continue

            # Throttle requests or get blocked after ~40 requests. 
            time.sleep(2)
            counter += 1
            if counter == 30:
                print('sleeping')
                time.sleep(10)
                counter = 0

            lon = row[1]
            lat = row[2]
            loc = f'{lon},{lat}'

            print(loc)
            driving = get_driving(loc)
            print(driving)
            if driving:
                row[15] = str(driving['distance'])
                row[16] = str(driving['duration'])
            
            else:
                print("NO MORE RESULTS")
                break 

    except Exception as e:
        print("error", e)
        pass 
        

# print(rows)


with open(file, 'w') as csvfile:
    print('writing all rows')
    writer = csv.writer(csvfile, delimiter=',')
    writer.writerow(header)
    for row in rows:
        writer.writerow(row)

    
