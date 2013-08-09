import json

ps = json.load(open("problems.json"))

# sort on size
print json.dumps( sorted(ps, lambda x,y: cmp(x["size"], y["size"])), indent=4)

# lsit comprehension
#print [ p for p in ps if "plus" in p["operators"] ]
