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

def save_dict(value, file_name):
    try:
        file = open(file_name, "w")
        file.write(value.items())
        file.close()
    except IOError:
        print("The file can not be open", file_name)

def main():
    alfa = 0.01
    p = 0.5
    value = new_dict()
    count = 0
    try:
        while(True):
            for state in range(1,100):
                new_state = rand_play(state, p)
                aux = value[new_state] - value[state]
                value[state] = value[state] + (alfa * abs(aux))
            count += 1
    except:
        #save_dict(value, "bet")
        print()
        print(value.items())
        print(count)

main()
