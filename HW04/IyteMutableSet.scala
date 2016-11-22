class IyteMutableSet {

  var elements = Array.fill(10){0}			
  var sorted = Array.fill(10){0}			
  var lastIndex=9							
  var emptyFirstIndex=0						
  
    def add(x:Int){
    
    if(!contains(x)){						
      
    	if(emptyFirstIndex <= lastIndex ){		
    	  
    		elements(emptyFirstIndex)=x			
    		sorted(emptyFirstIndex)=x			
    		emptyFirstIndex += 1				
    		sort()			
    	}
    	else{																	
    		var tempArray = Array.fill(lastIndex*(lastIndex + 1)){0}			
    		var tempSortedArray = Array.fill(lastIndex*(lastIndex+1)){0}
    		
    		for(i<-0 to lastIndex ){											
    			tempArray(i)=elements (i)
    			tempSortedArray(i)=sorted(i)
    		}
    		
    		elements=tempArray
    		sorted=tempSortedArray
    		lastIndex *= lastIndex
    		elements(emptyFirstIndex)=x				
    		sorted(emptyFirstIndex)=x
    		emptyFirstIndex += 1
    		sort()									
    	}
    }
  }
  def contains(x:Int):Boolean={				
    var first = 0
    var last = emptyFirstIndex-1
    var i=0
    
    while (first <= last){     
    	i = (first + last)/2;
    	if (sorted(i) == x){return true}
      else if (sorted(i) > x){last = i - 1;}
    	else{first = i + 1;
     }
    }
    return false
  }
    
  override def toString:String={
    var returned = "" 
      if(emptyFirstIndex != 0){
      
        for(i<-0 to emptyFirstIndex-1 ){
        	returned += elements(i).toString + ", "
        }
      }
      else{
        returned = "There are no elements in the set  "
      }
      returned.dropRight(2)
  }
 
  def sort(){					
    
    if(emptyFirstIndex>1){
    
      var first=0
      var last=emptyFirstIndex-2

      val value=sorted(emptyFirstIndex-1)
    
      if(value<sorted(first)){
      
    	  for(i<-(emptyFirstIndex-1) until first by -1){
    	  
    	    sorted(i)=sorted(i-1)
    	  
    	  }
   
      sorted(first)=value
   	  return
      }
      else if(value>sorted(last)){
    	  return
      }
      else{
          var first2=first+1
          var last2=last-1
    	
          while(first2 <= (last2+1)){
    		if(value>sorted(first) && value<sorted(first2) ){
    		  
    		  for(i<-(emptyFirstIndex-1) until first2 by -1){
    		    sorted(i)=sorted(i-1)
    		  }
    		  sorted(first2)=value
    		  return
    		}
    		else if(value>sorted(last2) && value<sorted(last)){
    		  
    		  for(i<-(emptyFirstIndex-1) until last by -1){
    		    sorted(i)=sorted(i-1)
    		  }
    		  sorted(last)=value
    		  return
    		}
    		first2 += 1
    		first += 1
    		last -= 1
    		last2 -= 1 
          }
      }
      return
    }
  }
}

object IyteMutableSet{
  
  def apply() = {
  new IyteMutableSet()
  }
}
