package application

import library._

object FiltrageHtmlImp extends FiltrageHtml {

  /**Fonction qui vérifie touts les sous-parties de l'expression e
  *@param h le fichier Html
  *@param e l'expression
  **/
  def filtreHtml(h:Html,e:Expression):Boolean={
    e match{
      case And(a,b)=> filtreHtml(h,a)&& filtreHtml(h,b)
      case Or(a,b)=> filtreHtml(h,a)|| filtreHtml(h,b)
      case Word(_) => query(h,e)
      }
  }
  
  /**Fonction qui cherche le text dans h pour comparer avec l'expression e
  *@param h le fichier Html
  *@param e l'expression
  **/
  def query(h:Html,e:Expression):Boolean={
     h match{
      case Tag(_,_,a)=>{
        var res = false
        for(x<-a){
          if(query(x,e)) res=true
        }
        res
      } 
      case Text(_)=> contains(h,e)
    }
  }
  
  /* Fonction qui compare la valuer Word() de l'expression avec la valeur Text de l'Html
  *@param h le fichier Html
  *@param e l'expression
  **/
  def contains(h:Html,f:Expression):Boolean={
    val str = h match{case Text(t)=> t}
		val e   = f match{case Word(a)=> a}
    str.toLowerCase().contains(e.toLowerCase())
  }

  /* Ancienne Fonction "à la main" qui compare la valuer Word() de l'expression avec la valeur Text de l'Html
  *@param h le fichier Html
  *@param e l'expression
  **/
  def contains2(h:Html,f:Expression):Boolean={
		var res = false
				val str = h match{
				case Text(t)=> t
		}
		val e = f match{
		case Word(a)=>a
		}
		var it=0;
		var k = 0;
		while (it < str.length() & k<e.length()) {
			if(str.charAt(it).toLower == e.charAt(k).toLower){
				k  +=1
				it +=1
				var tempo =it
				while(k<e.length() && it<str.length() && str.charAt(it).toLower == e.charAt(k).toLower){
					k+=1
					it+=1
						}

				if(k==(e.length())) {res=true}
				else {
					k=0
					it=tempo
				}
			}
			else it+=1
		}
		res
	}
}
