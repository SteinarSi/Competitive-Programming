import re

input()
print(re.sub(r'0', '<]:=', str(eval(re.sub(r'/', '//', re.sub(r'<]:=', '0', input()))))))
