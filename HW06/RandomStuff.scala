trait RandomStuffTrait {
  def transform(list: List[Int], op: (Int) => Int): List[Int];
  def allValid(list: List[Int], op: (Int) => Boolean): Boolean;
  def executeWithRetry(retryCount: Int, op: => Int): Option[Int];
}
object RandomStuff extends RandomStuffTrait {

  def transform(list: List[Int], op: (Int) => Int): List[Int] = {
    var result: List[Int] = List();
    for (l <- list) {
      result = result :+ op(l)
    }
    result;
  }
  
  def allValid(list: List[Int], op: (Int) => Boolean): Boolean = {
    list.foreach(l => if (!op(l)) {
      return false;
    });
    true;
  }

  def executeWithRetry(retryCount: Int, op: => Int): Option[Int] = {
    for (i <- 1 to retryCount) {
      println(i);
      try {
        return Option(op)
      } catch {
        case ex: Exception => {
        }
      }
    }
    None;
  }
}
