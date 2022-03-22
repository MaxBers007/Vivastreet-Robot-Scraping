package application
import library._

/*sealed trait Html
case class Tag(name:String,attributes:List[(String,String)],children:List[Html]) extends Html
case class Text(content:String) extends Html */

object produitHtml extends ProductionResultat {
  
  //Fonction qui produit un document Html  
  //en listant les solutions sous forme de liens
  //Grâce à une liste l de couple (titre, URL)
  
  def  resultat2html (l:List[(String,String)]) : Html = {
    
    val corptitre = Text("Ensemble des liens accessibles")
    val titre = Tag("title", List(), List())
    val meta = Tag("meta", List(("content", "text/html"), ("charset", "iso-8859-1")), List())
    val head = Tag("head", List(), List(meta, titre))
    var Nosref : List[Html] = List()

   if(!l.isEmpty){
     for(e <- l){
     
       val balise = Tag("a" , List(("href", e._2)), List(Text(e._1)))
       Nosref = balise::Nosref
     
     }
   }
    
    val body = Tag("body", List(), Nosref)
    val html = Tag("html", List(), head::body::Nil)
  
    html       
  }
}
