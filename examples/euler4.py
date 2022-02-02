# A palindromic number reads the same both ways. The largest palindrome made from the product of 
# two 2-digit numbers is 9009 = 91 × 99.
# Find the largest palindrome made from the product of two 3-digit numbers.
# 
# Correct Answer: 906609


res = 0

for i in range(100, 999):
    for k in range(100, 999):

        num = i * k
        tmp = num

        num_rev = 0
        while tmp != 0:
            num_rev = num_rev*10 + tmp % 10
            tmp = tmp // 10
        
        if num == num_rev and num > res:
            res = num

print(res)
