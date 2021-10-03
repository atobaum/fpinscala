object Ex2a3 {
    def curry[A, B, C](f: (A, B) => C): A => (B => C) = 
	    a => b => f(a, b)
    
    def uncurry[A, B, C](f: A => B => C): (A, B) => C =
        (a, b) => f(a)(b)

    def main(args: Array[String]): Unit ={
        println(curry((a: Int, b: Int) => a + b)(2)(3))
        println(uncurry(curry((a: Int, b: Int) => a + b))(2, 3))
    }
}