import random

def save_values(value,  filename = 'values.dat'):
    save = open(filename, 'w')
    content = []
    for k, v in value.items():
        content.append(str(k) + ' ' + str(v) + '\n')
    save.writelines(content)
    save.close()

def read_values(filename = 'values.dat'):
    try:
        save = open(filename, 'r')
        lines = save.readlines()
        value = {}
        for i in lines:
            k, v = i.strip().split(' ')
            value[int(k)] = float(v)
        return value
    except:
        return initialize()

def rand_play(state, p):
    action = random.randint(1, state)
    if (random.random() <= p):
        s = state + action
    else:
        s = state - action
    if (s > 100):
        s = 100
    return s

# possiveis casos ponderados
def best_play(value, state, p):
    best = 0
    ind = 1
    for i in range(1, state+1):
        v = p * value[(i + state) % 101] + (1 - p) * value[state - i]
        if (v > best):
            best = v
            ind = i
    return ind

def generate_graph(value, p):
    aux = {}
    for i in range(1, 100):
        aux[i] = best_play(value, i, p)
    save_values(aux, 'coin.dat')

def initialize():
    v = {}
    for i in range(0, 100):
        v[i] = 0
    v[100] = 1
    return v

def main():
    alfa = 0.001
    p = 0.4
    value = read_values()
    count = 0
    try:
        while(True):
            states = range(1, 100)
            for state in states:
                next_state = rand_play(state, p)
                aux = value[next_state] - value[state]
                value[state] = value[state] + (alfa * aux)
            count += 1
    except:
        save_values(value)
        generate_graph(value, p)
        print(count)

main()
