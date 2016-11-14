case class NewList(head: Int, rover: IyteImmutableList) extends IyteImmutableList{
	
	override def add(x : Int) : IyteImmutableList =  NewList(x,this)
	override def toString : String= {
		
		def join(init : IyteImmutableList) = init match {
			case Nul => Nul.toString
			case NewList(x,xs) => x.toString + "," + join(xs)
		}
		
		val string = join(this)
		string.substring(0,string.length-1)
	}
}

case object Nul extends IyteImmutableList{
	override def add(x : Int) : IyteImmutableList = NewList(x,Nul)
	override def toString = ""
}

sealed abstract class IyteImmutableList{
	def add(x : Int) :IyteImmutableList
}

object IyteImmutableList{
	def apply() = Nul
}