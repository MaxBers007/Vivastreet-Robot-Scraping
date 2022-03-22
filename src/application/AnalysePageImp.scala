package application

import library._

object AnalysePageImp extends AnalysePage {
  val objFiltrageUrls:FiltrageURLs = FiltrageURLimp
  val objFiltrageHtml:FiltrageHtml = FiltrageHtmlImp
  
  def resultats(url:String,exp:Expression):List[(String,String)] = {
    val doc_url:Html= UrlProcessor.fetch(url)
    val lURLs:List[String]=objFiltrageUrls.filtreAnnonce(doc_url)
    //println(lURLs) -> c'est OK
    var lHtml:List[Html] = Nil
    var l_couples:List[(String,Html)] = Nil
    var res:List[(String,String)] = Nil
    for(lien <- lURLs) {
      lHtml = lHtml ++ List(UrlProcessor.fetch(lien))
    }
    //println(lHtml) -> c'est OK
    for(doc_html <- lHtml) {
      if(objFiltrageHtml.filtreHtml(doc_html, exp)) {
        for(lien <- lURLs) {
          if( Html2StringImplem.process(UrlProcessor.fetch(lien)).equals(Html2StringImplem.process(doc_html))) {
            l_couples = l_couples ++ List((lien , doc_html))
          }
        }
      }
    }
    
    for(cpl <- l_couples) {
      res = res ++ List((chercheTitre(cpl._2) , cpl._1))
    }
    res
  }
  
  def chercheTitre(doc_html:Html):String = {
    doc_html match {
      case Text(_) => ""
      case Tag(c1 , _ , c3) => 
        (c1 , c3) match {
          case ("title" , List(Text(a))) => a
          //case ("head" , liste) => var res:String=""; for(e<-liste) { res+=chercheTitre(e) } ; res
          case (_ , liste) => var res:String=""; for(e<-liste) { res+=chercheTitre(e) } ; res
      }
      case _ => ""
    }
  }
  
}