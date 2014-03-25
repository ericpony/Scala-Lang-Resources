object test {
  
  def main(args: Array[String]): Unit = {
    
    /**
     * Pattern matching & List manipulationss
     */
    def odd(list:List[Int]):List[Int]= list match {
      case Nil => Nil
      case a::b if a%2==1 => a::odd(b)
      case a::b => odd(b)
    }
    var list = List(1,2,3,4,5,6,7)
    // The following two commands yields the same result
    odd(list)
    list.filter(x => x%2==1)
  }

//  Frequency counting: functional style
def IFreqCount(in:List[Char]) = in.sorted[Char].foldLeft(List[(Char,Int)]()) {
    case ((prevchr,cnt)::tl,chr) if prevchr==chr => (prevchr,cnt+1)::tl
    case (tbl,chr) => (chr,1)::tbl
}
// Frequency counting: imperative style
def IFreqCount2(in:List[Char]):List[(Char,Int)] = {
  val list = in.sorted[Char]
  var Tbl=List[(Char,Int)]()
  if(list.isEmpty)
    Tbl
  else{
    var prevChr=list.head
    var nxt=list.tail
    var Cnt=1
    while (!nxt.isEmpty){
      if(nxt.head==prevChr)Cnt+=1
      else {
        Tbl=(prevChr,Cnt)::Tbl
        Cnt=1
        prevChr=nxt.head
        }
      nxt=nxt.tail
      }
    (prevChr,Cnt)::Tbl
  } 
}

}