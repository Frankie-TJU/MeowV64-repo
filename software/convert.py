import sys
data = open(sys.argv[1], 'rb').read()
current = ''
for i in range(len(data)):
    current = '{:08b}'.format(data[i]) + current
    if (i + 1) % 16 == 0:
        print(current)
        current = ''
if len(current) > 0:
    print(current)
