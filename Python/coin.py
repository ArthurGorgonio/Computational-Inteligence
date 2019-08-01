from random import random, randint

# return a dictionary with start values for each key
def new_dict():
    value = {}
    for i in range(0,100):
        value[i] = 0
    value[100] = 1
    return value

def rand_play(state, p):
    action = randint(1, min(state, (100 - state)))
    if(random() <= p):
        s = state + action
    else:
        s = state - action
    return s

def save_values(value):
    save = open('values.csv', 'w')
    content = []
    for k, v in value.items():
        content.append(str(k) + ',' + str(v) + '\n')
    save.writelines(content)
    save.close()

def read_values(value):
    try:
        save = open('values.csv', 'r')
        lines = save.readlines()
        print(...)
    except:
        return initialize()

def main():
    alfa = 0.01
    p = 0.4
    value = read_values(value)
    count = 0
    try:
        while(True):
            for state in range(1,100):
                new_state = rand_play(state, p)
                aux = value[new_state] - value[state]
                value[state] = value[state] + (alfa * aux)
            count += 1
    except:
        #save_dict(value, "bet")
        print()
        print(value.items())
        print()
        print(count)

main()
