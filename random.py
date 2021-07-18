
def fib(x):
  if x == 1:
    return 1
  elif x == 0:
    return 0
  return fib(x-1) + fib(x-2)

print(fib(10))




