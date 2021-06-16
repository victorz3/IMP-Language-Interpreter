outputs = "outputs.txt"

def number_of_programs(filename):
    f = open(filename, "r")
    l = f.readlines()
    print(len(l))

def max_steps(filename):
    f = open(filename, "r")
    l = f.readlines()
    m = 0
    for line in l:
        line2 = line.split()
        steps = int(line2[3])
        if steps > m and steps < 10000:
            m = steps
    return m

def halted_programs():
    f = open(outputs, "r")
    l = f.readlines()
    m = 0
    for line in l:
        line2 = line.split()
        halted = line2[1]
        if halted != "err":
            m += 1
    return m

def find_repetitions():
    f = open(outputs, "r")
    l = f.readlines()
    last = ""
    for line in l:
        line2 = line.split()
        program = line2[0]
        if program == last:
            print("Repetition " + last)
        last = program
            
find_repetitions()
