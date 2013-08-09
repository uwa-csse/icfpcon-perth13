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

if not os.path.exists("trains"):
	os.makedirs("trains")

opstr1 = "_".join(prob['operators'])
opstr2 = " ".join(prob['operators'])

inout_fname = "trains/%02d-%s-%s.inout" % (prob['size'], opstr1, prob['id'])

inout_file = open(inout_fname, 'w')
inout_file.write("# %s\n" % prob['challenge'])

inout_file.write("%d %s\n" % (prob['size'], opstr2))

in_sent = 0
inputs = [0, 1, 0xffffffff]   ## Some probably useful values

outputs = []

############


while True:

    while len(inputs) < in_sent + 256: inputs.append(random.getrandbits(64))
    new_outs = s.evaluate(problem = prob['id'], arguments = inputs[in_sent : in_sent+256] )

    outputs = outputs + new_outs

    inouts = zip(inputs[in_sent : in_sent+256], new_outs)
    in_sent = in_sent + 256

    for (i,o) in inouts:
       inout_file.write("%d %d\n" % (i,o))

    inout_file.flush()

    #############

    myinput = open(inout_fname)
    myoutput = open(inout_fname + ".hbv", 'w')
    hbv_out = subprocess.check_output(['../hbv/dist/build/hbv/hbv', '-b'], stdin=myinput)

    print hbv_out
    myoutput.write(hbv_out)

    hbv_lines = hbv_out.splitlines()

    if len(hbv_lines) == 0:
	    print "hbv gave 0 lines\n"
	    exit(1)

    resp = s.guess(prob['id'], hbv_lines[0], True)
    if resp['status'] == 'win':
	    print "We won!\n"
	    exit(0)

    if resp['status'] == 'mismatch':
	    inout_file.write("%d %d\n" % (resp['values'][0],resp['values'][1]))
	    inout_file.flush()
	    inputs.append(resp['values'][0])
	    outputs.append(resp['values'][1])

    if resp['status'] == 'error':
	    print (resp['message'])
        
    # status 'error' => still continue and add more test cases - maybe check that there aren't too many errors?            

#myoutput.flush()

###########
