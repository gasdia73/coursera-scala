object week1 {

  trait Generator[+T] {
    self =>

    def generate: T

    def map[S](f: T=>S) : Generator[S] = new Generator[S] {
      def generate = f(self.generate)
    }

    def flatMap[S](f: T=>Generator[S]) : Generator[S] = new Generator[S] {
      def generate = f(self.generate).generate
    }


  }

  val integers = new Generator[Int] {
    val rand = new java.util.Random
    def generate = rand.nextInt()
  }

  val booleans = new Generator[Boolean] {
    def generate = integers.generate > 0
  }

  val pairs = new Generator[(Int, Int)] {
    def generate: (Int, Int) = (integers.generate, integers.generate)
  }


//  val booleans = integers map (x => x > 0)
//
//  def pairs[T,U](t: Generator[T], u: Generator[U])
//    t flatMap (x => u map (y => (x,y)))

}