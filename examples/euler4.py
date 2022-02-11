# A palindromic number reads the same both ways. The largest palindrome made from the product of 
# two 2-digit numbers is 9009 = 91 × 99.
# Find the largest palindrome made from the product of two 3-digit numbers.
# 
# Correct Answer: 906609

def reverse(n):
    rev = 0
    while n:
        rev = rev * 10 + n % 10
        n //= 10
    return rev

res = 0

for i in range(100, 1_000):
    for k in range(i, 1_000):
        num = i * k
        num_rev = reverse(num)
        
        if num == num_rev and num > res:
            res = num

print(res)
