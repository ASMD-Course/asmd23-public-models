package scala.lab.u07.task2

import u07.modelling.SPN.{SPN, Trn}

trait SPNAnalyzer[P]:
  def configurations: LazyList[SPN[P]]

object SPNAnalyzer:

  enum TrnGen[P]:
    case Fixed(trn: Trn[P])
    case Variable(trn: Trn[P], gen: Trn[P] => LazyList[Trn[P]])

  export TrnGen.*

  extension [P](trn: Trn[P])

    def rateVariation(from: Double, to: Double, step: Double): TrnGen[P] =
      Variable(
        trn, 
        base =>
          LazyList.iterate(from)(_ + step)
            .takeWhile(_ <= to)
            .map(r => base.copy(rate = _ => r))
      )

    def rateMultipliers(factors: Double*): TrnGen[P] =
      Variable(
        trn, 
        base =>
          LazyList.from(factors).map(f => base.copy(rate = m => base.rate(m) * f))
      )

    def rateOffsets(offsets: Double*): TrnGen[P] =
      Variable(
        trn, 
        base =>
          LazyList.from(offsets)
            .map(o => base.copy(rate = m => base.rate(m) + o))
      )

    def rateValues(values: Double*): TrnGen[P] =
      Variable(
        trn, 
        base =>
          LazyList.from(values).map(v => base.copy(rate = _ => v))
      )
      
    def fixed: TrnGen[P] = Fixed(trn)

  import u07.modelling.SPN.*

  private case class SPNAnalyzerImpl[P](specs: TrnGen[P]*) extends SPNAnalyzer[P]:

    override def configurations: LazyList[SPN[P]] =
      val variations = specs.map:
        case Fixed(trn) => LazyList(trn)
        case Variable(trn, gen) => gen(trn)

      combinations(variations.toList).map(_.toSet)

    private def combinations(lists: List[LazyList[Trn[P]]]): LazyList[List[Trn[P]]] =
      lists match
        case Nil => LazyList(Nil)
        case head :: tail =>
          for
            h <- head
            t <- combinations(tail)
          yield h :: t


  def apply[P](items: (Trn[P] | TrnGen[P])*): SPNAnalyzer[P] =
    val specs = items.map:
      case t: Trn[P] => Fixed(t)
      case s: TrnGen[P] => s

    SPNAnalyzerImpl(specs*)






