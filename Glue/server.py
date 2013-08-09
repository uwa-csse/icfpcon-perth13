import json, requests

def _enum(**enums):
    return type('Enum', (), enums)

Operators = _enum(NO_FOLDS=0, TFOLD=1, FOLD=2)

class GameServerException(Exception):
    pass

class RequestInvalidException(GameServerException):
    pass

class ProblemNotFoundException(GameServerException):
    pass

class AuthorisationRequiredException(GameServerException):
    pass

class TryAgainLaterException(GameServerException):
    pass

class GameServer(object):
    _USER_ID = '0357TH6LX4rVbuVBCIBqeCcHznBDyL8ce5uduB69'
    _URL = 'http://icfpc2013.cloudapp.net/%s?auth=%svpsH1H'

    def get_problems(self):
        response = self._request('myproblems')
        return response

    def evaluate(self, problem = None, program = None, arguments = []):
        arguments = map(lambda x: '%#018x' % x, arguments)
        if problem:
            request_body = {'id': problem, 'arguments': arguments}
        elif program:
            request_body = {'program': program, 'arguments': arguments}
        else:
            raise GameServerException('Request must contain either a problem ID or a program')
        print request_body
        response = self._request('eval', request_body)
        if response['status'] == 'ok':
            outputs = map(lambda x: int(x, 16), response['outputs'])
            return outputs
        else:
            raise GameServerException(response['message'])

    def guess(self, problem, program):
        request_body = {'id': problem, 'program': program}
        response = self._request('guess', request_body)
        if response['status'] == 'win' or response['status'] == 'mismatch':
            return response
        else:
            raise GameServerException(response['message'])

    def get_training_problem(self, size = None, operators = None):
        if operators == Operators.NO_FOLDS:
            operators = []
        elif operators == Operators.TFOLD:
            operators = ['tfold']
        elif operators == Operators.FOLD:
            operators = ['fold']
        else:
            operators = None

        if not (type(size) is int and 3 <= size <= 30):
            size = None

        if size != None and operators != None:
            request_body = {'size': size, 'operators': operators}
        elif size != None:
            request_body = {'size': size}
        elif operators != None:
            request_body = {'operators': operators}
        else:
            request_body = {}
        response = self._request('train', request_body)
        return response
    
    def get_status(self):
        response = self._request('status')
        return response

    def _request(self, command, request_body = None):
        url = _URL % (command, _USER_ID)
        if request_body:
            r = requests.post(url, data = json.dumps(request_body))
        else:
            r = requests.post(url)

        if r.status_code == 200:
            return json.loads(r.text)
        elif r.status_code == 400:
            raise RequestInvalidException('Request not well-formed')
        elif r.status_code == 401:
            raise ProblemNotFoundException('Problem not requested by current user')
        elif r.status_code == 403:
            raise AuthorisationRequiredException('Authorisation required')
        elif r.status_code == 404:
            raise ProblemNotFoundException('Problem does not exist')
        elif r.status_code == 410:
            raise ProblemNotFoundException('Problem expired')
        elif r.status_code == 412:
            raise ProblemNotFoundException('Problem already solved')
        elif r.status_code == 413:
            raise RequestInvalidException('Request too big')
        elif r.status_code == 429:
            raise TryAgainLaterException('Try again later')
        else:
            raise TryAgainLaterException('Unexpected error')
