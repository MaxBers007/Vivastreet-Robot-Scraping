package application
import library._

object FiltrageURLimp extends FiltrageURLs {

  
    def listeUrl(l: List[(String, String)]): List[String] = {
        l match {
            case Nil        => Nil
            case x :: suite => if (x._1 == "href") x._2 :: listeUrl(suite) else listeUrl(suite)
        }
    }

    def filtrage(l: List[String]): List[String] = {
        l match {
            case Nil    => Nil
            case x :: y => if (x.contains("https://www.vivastreet.com/") && contient9Chiffres(x)) { x :: filtrage(y) } else { filtrage(y) }

        }

    }
    
     def contient9Chiffres(s: String): Boolean = {
        var count: Int = 0;
        for (n <- s) {

            if (n <= '9' && n >= '0') {
                count += 1
            }
        }
        /* return true ssi s contient au moins 9 chiffres */
        count >= 9
    }
    
    /*  def toIntToBool(s: String): Boolean = {
    try {
      s.toInt.isValidInt
    } catch {
      case e: Exception => false
    }
      }*/

    def filtreAnnonce(h: Html): List[String] = {
        h match {
            case Text(_)      => Nil
            case Tag(_, x, y) => filtrage(listeUrl(x)) ++ filtreAnnonceBis(y)
        }
    }

    def filtreAnnonceBis(h: List[Html]): List[String] = {
        h match {
            case Nil    => Nil
            case x :: r => filtreAnnonce(x) ++ filtreAnnonceBis(r)
        }

    } 
}