# ICFP 2013 Programming Contest

## Parts
### Solvers
Our solvers are the meaty bits, writen in F#, Haskell, etc. They will be given the metadata for a
problem and try to come up with a plan for solving it within 5 minutes. When they have a plan, they
will enact the plan by sending `eval` and `guess` requests to stdout and reading the responses
from stdin.

### Glue
We have a python script that translates between the API and our solvers.

### Glue <-> Solver interface
*Not yet final*

A running instance of a solver works on one problem. The problem description is passed as command
line arguments:

    solver size op [op ...]

where `size` is the size of the program and `op` is an op that is present in the program.

When a solver has a plan that will determine the answer to the problem it may start sending
requests, `eval` or `guess`, to stdout.

#### Eval
**Request**

    eval
    <input-list>

**Response**

	<output-list>
    done

where `input-list` and `output-list` are a space seperated list of 64-bit vectors in whatever format suits.

#### Guess
**Request**

    guess
    <program>
    
**Response**

    win|lose
    <input> <ouput>

where `program` is the program as a string, `input` is a counterexample and `output` is the correct
matching output.

