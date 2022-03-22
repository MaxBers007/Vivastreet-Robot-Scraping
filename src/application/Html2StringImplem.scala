package application
import library._

object Html2StringImplem extends Html2String{
 
  
  override def process(h:Html):String = {
    h match {
      case Text(s) => " " +s
          case Tag(x,y,z) => z match {
          case List() => x match{
          case "meta" =>   " <" +x +" " + newToString(y) + ">"+" </"+x+">"  
          case "a" =>" <" +" "+x + newToString(y) + ">"+" </"+x+">"  
          case _ => " <" +x+ "> " + newToString(y) + "</"+x+">"  
          }
          case k::Nil => x match{
          case "meta" =>   " <" + " " +x + newToString(y) +process(k)+ ">"+" </"+x+">"  
          case "a" =>" <" +x +" " + newToString(y) +process(k)+ ">"+" </"+x+">"  
          case _ => " <" +x+ "> " + newToString(y) +process(k)+ " </"+x+">"  
          }
          case k::l =>  x match{
          case "meta" =>   " <" +x + " " + newToString(y) + process(k)+processList(l) +">"+" </"+x+">"  
          case "a" =>" <" +x +" " + newToString(y) + process(k)+processList(l) +">"+" </"+x+">"  
          case _ => " <" +x+ "> " + newToString(y) +process(k)+processList(l) + " </"+x+">"  
          }
      }

    }
  }
  
  def processList(l:List[Html]):String = {
    l match {
      case Nil => ""
      case k::l => process(k) + processList(l)
    }
  }
  
  def newToString(l:List[(String,String)]):String = {
    l match {
      case Nil => ""
      case (e,e1)::Nil => e + "=" + e1
      case (e,e1)::l => e + "=" + e1 + "; " + newToString(l)
    }
  }
  
}
