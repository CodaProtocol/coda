import requests
import json
import time

query = """query {
  version
}"""

url = 'http://localhost:3085/graphql'

while True:
    time.sleep(0.1)
    start_time = time.time()
    r = requests.post(url, json={'query': query})
    if r.status_code != 200:
        print "request failed:", r.text
    else:
        end_time = time.time()
        duration = end_time - start_time
        print "graphql version call took {} seconds".format(duration)
