"""Local ACP fixture: log requests and implement initialize/new/load only."""
import json
import sys

for line in sys.stdin:
    request = json.loads(line)
    with open(sys.argv[1], 'a') as log:
        log.write(json.dumps(request) + '\n')
    if 'id' not in request:
        continue
    method = request.get('method')
    if method == 'initialize':
        result = {'protocolVersion': 1, 'agentCapabilities': {'loadSession': True},
                  'authMethods': [], 'agentInfo': {'name': 'sidebar-fixture', 'version': '1'}}
    elif method == 'session/load':
        result = {}
    elif method == 'session/new':
        result = {'sessionId': 'fixture-new-session'}
    else:
        print(json.dumps({'jsonrpc': '2.0', 'id': request['id'],
                          'error': {'code': -32601, 'message': 'Unsupported fixture method'}}), flush=True)
        continue
    print(json.dumps({'jsonrpc': '2.0', 'id': request['id'], 'result': result}), flush=True)
