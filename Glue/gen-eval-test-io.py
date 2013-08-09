import random
import os
import sys
import subprocess

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

if not os.path.exists("trains"):
	os.makedirs("trains")

############
opstr1 = "_".join(prob['operators'])
opstr2 = " ".join(prob['operators'])

inout_fname = "trains/%02d-%s-%s.inout" % (prob['size'], opstr1, prob['id'])
inout_file = open(inout_fname, 'w')
inout_file.write("# %s\n" % prob['challenge'])

inout_file.write("%d %s\n" % (prob['size'], opstr2))

inouts = zip(inputs, outputs)

for (i,o) in inouts:
   inout_file.write("%d %d\n" % (i,o))

inout_file.close()

#############
myinput = open(inout_fname)
myoutput = open(inout_fname + ".hbv", 'w')
hbv_out = subprocess.check_output(['../hbv/dist/build/hbv/hbv', '-b'], stdin=myinput)

print hbv_out

myoutput.write(hbv_out)

#myoutput.flush()

###########
