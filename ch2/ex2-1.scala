object Ex2a1{
    def fib(n: Int): Int = {
	if (n <= 1) n
	else fib(n - 1) + fib(n - 2)
    }

    def main(args: Array[String]): Unit = {
	println("fib(0) is %d".format(fib(0)))
	println("fib(1) is %d".format(fib(1)))
	println("fib(2) is %d".format(fib(2)))
	println("fib(3) is %d".format(fib(3)))
	println("fib(4) is %d".format(fib(4)))
	println("fib(5) is %d".format(fib(5)))
	println("fib(6) is %d".format(fib(6)))
    }
}