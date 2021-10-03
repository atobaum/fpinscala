object Ex2a5 {
    def compose[A, B, C](f: B => C, g: A => B): A => C = 
        a => f(g(a))

    def main(args: Array[String]): Unit ={
        println(compose((a: Int) => a * 3, (a: Int) => a + 2)(2))
    }
}