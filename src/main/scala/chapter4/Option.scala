package chapter4
import scala.List


  enum Option[+A]:
    case Some(get: A)
    case None

    //Ex 4.1 red book 2ed
    def map[B](f: A => B) : Option[B] = this match
      case None => None
      case Some(x) => Some(f(x))

    def getOrElse[B >: A](default: => B): B = this match
      case None => default
      case Some(x) => x

    def flatMap[B](f: A => Option[B]): Option[B] =
      map(f).getOrElse(None)
    /*
    def orElse[B >: A](ob: => Option[B]): Option[B] = this match
      case None => ob
      case Some(_) => this
    */
    def orElse[B >: A](ob: => Option[B]): Option[B] =
      map(Some(_)).getOrElse(ob)
    /*
      def filter(f: A => Boolean): Option[A] = this match
      case Some(x) if f(x) => this
      case _ => None
    */
    def filter(f: A => Boolean): Option[A] =
      flatMap(a => if f(a) then Some(a) else None)
  object Option:
    //Ex4.2 red book 2ed
    def mean(xs: Seq[Double]): Option[Double] =
      if xs.isEmpty then None
      else Some(xs.sum / xs.length)

    /**
     * Spiegazione:
     * 1) Calcolo la media mean(xs).
     * 2) La varianza è una FUNZIONE della media mean(xs) la quale media viene espressa con m
     * 3) La funzione che calcola la varianza sarà m => mean(xs.map(x => math.pow(x -m,2)))
     * 4) Questa funzione, lavorando sulla media m (che è un Option), restituisce una Option[Option[Double]]
     * 5) flatMap a questo punto viene utilizzata per "appiattire" il risultato della funzione varianza a un solo Option[Double]
     *    Se utilizzassi map invece di flatMap otterrei Option[Option]]
     * In altre parole io "mappo" la funzione che calcola la varianza sulla media, ma non posso usare la classica map perché sfrutto gli Option e
     * mi tornerebbe Option[Option[Double]]. Per questo motivo utilizzo flatMap
     *
     * flatMap viene utilizzato per costruire una COMPUTAZIONE IN PIù PASSI. Ognuno di questi passi può fallire (per un errore, un valore inammissibile, etc) e
     * tornare "None" fermando l'intera computazione.
     *
     */
    def variance(xs: Seq[Double]) : Option[Double] =
      mean(xs).flatMap(m =>
        mean(xs.map(x => math.pow(x - m, 2))))

    //Ex 4.3 Red book Ed 2: My solution
    def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] = (a,b) match
      case (None,_) | (_, None) => None
      case (Some(x),Some(y)) => Some(f(x,y))

    //Book solution (the first; the second is the same as map2)
    def map2v2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] =
      a.flatMap(aa => b.map(bb => f(aa,bb)))

    //Ex 4.4
/*    def sequence[A](as: List[Option[A]]): Option[List[A]] =
      foldRight(as, Some(Nil:List[Option[A]]):Option[List[A]],(x, acc) => map2(x,acc)(Cons(_, _)))
      //case Cons(x,xs) if x != None => Some(Cons(x,sequence(xs)))

 */

/*
    def map[B](f: A => B): Option[B] = this match
      case None => None
      case Some(x) => Some(f(x))
*/