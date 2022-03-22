package application
import library._

object Test extends App {
  val Url : String = "https://search.vivastreet.com/immobilier-vente-maison/fr?lb=new&search=1&start_field=1&keywords=rennes+appart&cat_1=280&geosearch_text=&searchGeoId=&sp_common_price%5Bstart%5D=&sp_common_price%5Bend%5D=&sp_housing_nb_rooms%5Bstart%5D=&sp_housing_nb_rooms%5Bend%5D=&sp_housing_sq_ft%5Bstart%5D=&sp_housing_sq_ft%5Bend%5D=&sp_common_designation="
  val Url2 : String = "https://www.vivastreet.com/emploi-fonction-informatique-internet-telecom/paris-12eme-ardt-75012/pmo-f-h/215391971"

  val doc_html : Html = UrlProcessor.fetch(Url)
  val doc_html2 : Html = UrlProcessor.fetch(Url2)
  val exemple = Tag("html", List(),
      
    List(
      Tag("head", List(),
      List(
        Tag("meta", List(("content", "text/html"), ("charset", "iso-8859-1")), List()),
        Tag("title", List(), List(Text("MyPage"))))),
      Tag("a", List(("href", "http://www.vivastreet.com/123456789")),
        List(Text("Lien"), Tag("img", List(), List()))),
      Tag("body", List(), List(
        Text("&nbsp"),
        Tag("center", List(), List(
          Tag("a", List(("href", "http://www.irisa.fr")),
            List(Text("Lien"), Tag("img", List(), List())))))))))

  println(FiltrageURLimp.filtreAnnonce(exemple))
  println(Html2StringImplem.process(doc_html).contains("https://www.vivastreet.com/immobilier-vente-maison/belle-isle-en-terre-22810/dpt--22----a-vendre---belle-isle-en-terre---propri-t--avec---/215294399"))
  println(FiltrageURLimp.filtreAnnonce(doc_html))
  
  println(Html2StringImplem.process(doc_html2).contains("https://www.vivastreet.com/emploi-fonction-informatique-internet-telecom/paris-12eme-ardt-75012/pmo-f-h/215391971"))
  println(FiltrageURLimp.filtreAnnonce(doc_html2))
  //val exemple2 = 
}