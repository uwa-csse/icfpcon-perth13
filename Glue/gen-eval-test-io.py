import random
import os
import sys
import subprocess

from server import Server

#if (len(sys.argv) != 2):
#	print "usage: %s <size>", sys.argv[0]
#	exit (1)

#sz = sys.argv[1]

s = Server()

#probs = [ s.get_training_problem(int(sz)) ]
#probs = json.load(open("problems.json"))

probs = s.get_problems()    ## LIVE!!


# sort on size
probs = sorted(probs, lambda x,y: cmp(x["size"], y["size"]))

#probs = [ p for p in probs if p['size'] <= 8 and ((not 'solved' in p) or not p['solved']) ]

probs = [ p for p in probs if p['size'] > 8 and  p['size'] <= 11 and ((not 'solved' in p) or not p['solved']) and 'tfold' in p['operators'] ]


for prob in probs:

    print prob["id"]

    if not os.path.exists("actuals"):
	    os.makedirs("actuals")

    opstr1 = "_".join(prob['operators'])
    opstr2 = " ".join(prob['operators'])

    inout_fname = "actuals/%02d-%s-%s.inout" % (prob['size'], opstr1, prob['id'])

    inout_file = open(inout_fname, 'w')
#    inout_file.write("# %s\n" % prob['challenge'])

    inout_file.write("%d %s\n" % (prob['size'], opstr2))

    in_sent = 0
		    ## Some probably useful values
    inputs = [0, 1, 0xffffffffffffffff, 0x00000000ffffffff, 0xffffffff00000000, 0x5555555555555555, 0xaaaaaaaaaaaaaaaa, 
		    0x3333333333333333, 0xcccccccccccccccc, 0x0f0f0f0f0f0f0f0f, 0xf0f0f0f0f0f0f0f0, 
		    0x00ff00ff00ff00ff, 0xff00ff00ff00ff00, 0x0000ffff0000ffff, 0xffff0000ffff0000]

    outputs = []

    ############


    while True:

	while len(inputs) < in_sent + 256: inputs.append(random.getrandbits(64))
	new_outs = s.evaluate(problem = prob['id'], arguments = inputs[in_sent : in_sent+256], training = False )

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
		print "hbv gave 0 lines!\n"
		exit(1)

	resp = s.guess(prob['id'], hbv_lines[0], False)
    #    resp = s.guess(prob['id'], "(lambda (x) (plus x (if0 (shr16 x) (shl1 0) 1)))", True)
	if resp['status'] == 'win':
		print "We won!"
                break
#		exit(0)

	if resp['status'] == 'mismatch':
		inout_file.write("%s %s\n" % (resp['values'][0],resp['values'][1]))
		inout_file.flush()
		inputs.append( int(resp['values'][0],0) )
		outputs.append( int(resp['values'][1],0) )

	if resp['status'] == 'error':
		print (resp['message'])

	# status 'error' => still continue and add more test cases - maybe check that there aren't too many errors?            

    #myoutput.flush()

    ###########
