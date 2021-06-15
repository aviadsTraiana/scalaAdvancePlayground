package playground

import java.time.ZonedDateTime


object ADT extends App {

    sealed trait Filter
  object Filter {

      final case class Date(value: ZonedDateTime) extends Filter
      final case class Category(value: Int) extends Filter

      final case class InStock(value: Boolean) extends Filter

      final case class Price(from: Double, to: Double) extends Filter

  }

    /*def filterValues(filter: Filter) : String = filter match {
        case Filter.Date(value) => "1"
        case Filter.Category(value) =>"2"
        case Price(from, to) =>"4"
    }
    println(filterValues(Price(1,2)))*/




}
