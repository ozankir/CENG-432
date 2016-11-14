object Simple extends App {
  val values = Array(10,25,30)
 
  values.foreach((number : Int) =>   if(number % 2 == 0){println (number*2)}
   else{println(number*3)})
}