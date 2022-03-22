package application
import library._
import java.io.FileWriter

object Application extends App {
  
  println("Entrez votre requête")
  val requete: Expression = ExpressionParser.readExp
  val urlSearch = "https://search.vivastreet.com/annonces/fr?lb=new&search=1&start_field=1&keywords=" + UrlBuilder(requete) + "&cat_1=&geosearch_text=&searchGeoId=0"
 
  def UrlBuilder(e: Expression): String={
    var UrlSearch: String = ""
    e match{
      case And(a,b) => UrlSearch = UrlSearch + UrlBuilder(a) + "+" + UrlBuilder(b)
      case Or(a, b) => UrlSearch = UrlSearch + UrlBuilder(a) + "&" + UrlBuilder(b)
      case Word(a) => UrlSearch = UrlSearch + a
    }
    UrlSearch
  }
 
  println(urlSearch)
 
  val temps_debut = System.currentTimeMillis()
  val couplesUrls: List[(String, String)] = AnalysePageImp.resultats(urlSearch, requete)
  val temps_analyse = System.currentTimeMillis()
  println("Les couples: " + couplesUrls)
  val temps_affichage=System.currentTimeMillis()
  println("temps analyse = " + (temps_analyse - temps_debut) + "ms")
  println("temps affichage = " + (temps_affichage - temps_debut) + "ms")
 
  val htmlResultat = produitHtml.resultat2html(couplesUrls)
 
  val stringResultat = Html2StringImplem.process(htmlResultat)
 
  
  println(stringResultat)
  
}
