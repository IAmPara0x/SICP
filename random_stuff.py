
# testing procedures in python before implementing in lisp

def fib(x):
  if x == 1:
    return 1
  elif x == 0:
    return 0
  return fib(x-1) + fib(x-2)


def triplets(n):
  for i in range(1,n+1):
    for j in range(1,n+1):
      for k in range(1,n+1):
        print(i,j,k)
triplets(3)

