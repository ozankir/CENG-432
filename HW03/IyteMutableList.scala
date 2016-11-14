class IyteMutableList {
  class Node(var x: Int, var next: Node)
  private var head: Node = null
  private var rover: Node = null

  def add(x: Int) = {
    if(rover == null) {
      rover = new Node(x,null)
      head = rover
    } 
    else {
      rover.next = new Node(x,null)
      rover = rover.next
    }
  }

  override def toString: String = {
    var temp = head
    var result: String = ""
    
      while(temp != null){
      result= result+temp.x
      result= result+ ","
      temp = temp.next
    }
    return result.substring(0,result.length-1)
  }
}

object IyteMutableList {
  def apply: IyteMutableList = new IyteMutableList()
}