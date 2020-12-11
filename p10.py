f = open("src/main/resources/y2020/day10.txt", "r+")
data = f.readlines()

# Prep work
adapters = [0]
for line in data:
    adapters.append(int(line))
adapters.append(max(adapters)+3)

# Sorting makes this much easier
adapters = sorted(adapters)

# Find the number of 1-jolt and 3-jolt jumps
def part1(list):
    single_j, triple_j = 0, 0
    for n, el in enumerate(list[:-1]):
        if list[n+1] - el == 1:
            single_j += 1
        elif list[n+1] - el == 3:
            triple_j += 1
    print(single_j*triple_j)

# Find the number of paths to max joltage in list
# This function starts from the *highest* value and works backward using a 
#    dictionary. The number of paths to the highest joltage is set as the value
#    to each adapter key. Traversing high-to-low, the number of "ways" from any
#    arbitrary joltage up to the highest joltage is equal to the ways-sum of
#    all adapters within reach (+3 jolts) of the current adapter.
def part2(list):
    paths = {}
    list = sorted(list, reverse=True)
    for n, el in enumerate(list):
        # Add the initial max joltage entry
        if len(paths) == 0:
            paths[el] = 1
        else:
            i, ways = 1, 0
            while (n-i >= 0) and (list[n-i] - el <= 3):
                ways += paths[list[n-i]]
                i += 1
            paths[el] = ways
    print(paths[min(paths)])

def main():
    part1(adapters)
    part2(adapters)

main()
