import random
import os
import sys

from server import Server


if (len(sys.argv) != 2):
	print "usage: %s <size>", sys.argv[0]
	exit (1)

sz = sys.argv[1]

s = Server()
prob = s.get_training_problem(int(sz))
print prob["id"]

inputs = []
while len(inputs) < 256: inputs.append(random.getrandbits(64))
outputs = s.evaluate(problem = prob['id'], arguments = inputs)

if not os.path.exists("tests"):
	os.makedirs("tests")

in_file = open("tests/%s.in" % prob['id'], 'w')
in_file.write("%s\n" % prob['challenge'])
for i in inputs:
	in_file.write("%d\n" % i)

out_file = open("tests/%s.expected" % prob['id'], 'w')
for o in outputs:
	out_file.write("%d\n" % o)
