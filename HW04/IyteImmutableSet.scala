class IyteImmutableSet(val elements:Array[Int],val sorted:Array[Int],val lastIndex:Int,val emptyFirstIndex:Int) {
      
    		
  var nextElements = Array.fill(lastIndex+1){0}		
  var nextSorted = Array.fill(lastIndex+1){0}		
  for(i<-0 to lastIndex ){
    nextElements(i)=elements (i)
	nextSorted(i)=sorted(i)
  }
  
  var nextLastIndex = lastIndex					
  var nextEmptyFirstIndex = emptyFirstIndex		

  def add(x:Int):IyteImmutableSet={ 
    
    if(!contains(x)){								
      
    	if(emptyFirstIndex <= lastIndex ){				
    	  
    		nextElements(nextEmptyFirstIndex)=x			
    		nextSorted(nextEmptyFirstIndex)=x			
    		nextEmptyFirstIndex = emptyFirstIndex+1		
    		sort()										
    		
    	}
    	else{																
    		var tempArray = Array.fill(lastIndex*(lastIndex + 1)){0}		
    		var tempSortedArray = Array.fill(lastIndex*(lastIndex+1)){0}
    		for(i<-0 to lastIndex ){
    			tempArray(i)=elements (i)
    			tempSortedArray(i)=sorted(i)
    		}
    		
    		nextElements=tempArray
    		nextSorted=tempSortedArray
    		nextLastIndex = lastIndex * lastIndex
    		nextElements(nextEmptyFirstIndex)=x				
    		nextSorted(nextEmptyFirstIndex)=x
    		nextEmptyFirstIndex = emptyFirstIndex+1
    		sort()											
    	}
    }
    
    new IyteImmutableSet(nextElements,nextSorted,nextLastIndex,nextEmptyFirstIndex)	
  }
  
  
    
  def contains(x:Int):Boolean={		

    var first = 0
    var last = emptyFirstIndex-1
    var i=0
    
    while (first <= last){
      
    	i = (first + last)/2;
        if (sorted(i) == x){
    		return true
    	}
    	else if (sorted(i) > x){ last = i - 1;}
    	else{first = i + 1;}
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
    
   if(nextEmptyFirstIndex>1){
    var first=0
    var last=nextEmptyFirstIndex-2
    val value=nextSorted(nextEmptyFirstIndex-1)
    
    if(value<nextSorted(first)){
      
      for(i<-(nextEmptyFirstIndex-1) until first by -1){
    		    nextSorted(i)=nextSorted(i-1)
      }   
      nextSorted(first)=value
   	  return
    }
    else if(value>nextSorted(last)){
      return
    }
    else{
          var first2=first+1
          var last2=last-1
    	
        while(first2 <= (last2+1)){
    		  if(value>nextSorted(first) && value<nextSorted(first2) ){
    		  
    		    for(i<-(nextEmptyFirstIndex-1) until first2 by -1){
    		      nextSorted(i)=nextSorted(i-1)
    		    }
    		    nextSorted(first2)=value
    		    return
    		  }
    		  else if(value>nextSorted(last2) && value<nextSorted(last)){
    		  
    		    for(i<-(nextEmptyFirstIndex-1) until last by -1){
    		      nextSorted(i)=nextSorted(i-1)
    		    }
    		  
    		  nextSorted(last)=value
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

object IyteImmutableSet{
    
  def apply() = {
      
    val consElements = Array.fill(10){0}		
    val consSorted = Array.fill(10){0}
    val consLastIndex=9
    val consEmptyFirstIndex=0
    
    new IyteImmutableSet(consElements,consSorted,consLastIndex,consEmptyFirstIndex )
  }
}
