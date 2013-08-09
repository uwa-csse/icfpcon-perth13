
import random
import os
import sys

from server import Server


if (len(sys.argv) != 3):
	print "usage: %s <size>", sys.argv[0]
	exit (1)

sz = sys.argv[1]
nprobs = int(sys.argv[2])

s = Server()

for probi in range(nprobs):
    prob = s.get_training_problem(int(sz))
    print prob["id"]

    if not os.path.exists("tests"):
	    os.makedirs("tests")

    opstr1 = "_".join(prob['operators'])
    opstr2 = " ".join(prob['operators'])

    inout_file = open("tests/%02d-%s-%s.inout" % (prob['size'], opstr1, prob['id']), 'w')
    inout_file.write("# %s\n" % prob['challenge'])

    inout_file.write("%d %s\n" % (prob['size'], opstr2))

    inputs = []
    while len(inputs) < 256: inputs.append(random.getrandbits(64))
    outputs = s.evaluate(problem = prob['id'], arguments = inputs)

    inouts = zip(inputs, outputs)

    for (i,o) in inouts:
	    inout_file.write("%d %d\n" % (i,o))
