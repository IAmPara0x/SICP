
class A:
    def __init__(self, a: int) -> None:
        self.a = a

    def __repr__(self):
        return f"A:{self.a}"

a = A(10)
print(a)


