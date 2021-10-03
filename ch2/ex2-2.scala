object Ex2a2 {
    def isSorted[A](as: Array[A], orderd: (A, A) => Boolean) = {
	def loop(n: Int): Boolean = 
	    if (n >= as.length - 1) true
	    else if (!orderd(as(n), as(n+1))) false
	    else loop(n + 1)

	loop(0)
    }

    def main(args: Array[String]): Unit ={
        println(isSorted(Array("aaa", "bbb", "ccc"), (a: String, b: String) => a < b))
        println(isSorted(Array("aaa", "ccc", "bbb"), (a: String, b: String) => a < b))
        println(isSorted(Array(1,2,3), (a: Int, b: Int) => a < b))
        println(isSorted(Array(2,1,3), (a: Int, b: Int) => a < b))
    }
}