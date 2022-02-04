# 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
# What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
# 
# Correct Answer: 232_792_560

num = 20

while True:
    for i in range(20, 2, -1):
        if num % i != 0:
            break
    else:
        break
    num = num + 20

print(num)
