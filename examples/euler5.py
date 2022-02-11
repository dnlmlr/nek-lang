# 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
# What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
# 
# Correct Answer: 232_792_560

def gcd(x, y):
    while y:
        x, y = y, x % y
    return x

result = 1
for i in range(1, 21):
    result *= i // gcd(i, result)

print(result)
