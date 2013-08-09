# ICFP 2013 Programming Contest

## Parts
### Glue
We have a python script that translates between the web API and our solvers.

### Solvers
Our solvers are the meaty bits, writen in F#, Haskell, etc. 

#### Eval mode
The solver expects the first line to be a program to compile, and every following line to be a set of arguments
to evaluate using the program.

#### Solve mode
They will be given the metadata for a problem and try to come up with a plan for solving it within 5
minutes. When they have a plan, they will enact the plan by sending `eval` and `guess` requests to
stdout and reading the responses from stdin.

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
    [<input> <ouput>]

where `program` is the program as a string, `input` is a counterexample and `output` is the correct
matching output. If the guess was correct, `input` and `output` will not be present

