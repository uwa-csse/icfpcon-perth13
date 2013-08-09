import random
import os
import sys

from server import Server


if (sys.argv < 3):
	print "usage: %s <size> <op> [<op> [...]]", sys.arg[0]
	exit (1)

size = sys.argv[1]
operators = sys.argv[2:]

s = Server()
prob = s.get_training_problem(size = size, operators = operators)

inputs = []
while len(inputs) < 256: inputs.append(random.getrandbits(64))
outputs = s.evaluate(problem = prob.id, arguments = inputs)

if not os.path.exists("tests"):
	os.makedirs("tests")

in_file = open("tests/%s.in" % prob.id, 'w')
in_file.write("%s\n" % prob.challenge)
for i in inputs:
	in_file.write("%d\n" % i)

out_file = open("tests/%s.expected" % prob.id, 'w')
for o in outputs:
	out_file.write("%d\n" % o)
