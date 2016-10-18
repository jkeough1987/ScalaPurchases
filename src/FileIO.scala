import java.io._
import java.util

import scala.collection.mutable
import scala.io.Source


/**
  * Created by joshuakeough on 10/18/16.
  */
object FileIO {

  val purchases = mutable.MutableList[Purchase]()

  def main(args: Array[String]): Unit = {


    Source.fromFile("purchases.csv").getLines.drop(1).foreach(line => {
      val Array(customer_id, date, credit_card, cvv, category) = line.split(",").map(_.trim)
      purchases += Purchase(customer_id, date, credit_card, cvv, category)
    })
    var resp = ""
    while (resp != "Q") {
      resp = menu match {
        case "FURNITURE" => sort("furniture")
        case "ALCOHOL" => sort("alcohol")
        case "TOILETRIES" => sort("toiletries")
        case "SHOES" => sort("shoes")
        case "FOOD" => sort("food")
        case "JEWELRY" => sort("jewelry")
      }
    }
  }


  case class Purchase(customer_id: String, date: String, credit_card: String, cvv: String, category: String) {
    override def toString: String = s"Customer:${customer_id} Date: ${date} "
  }

  def prompt(s: String) = {
    println(s);
    io.StdIn.readLine()
  }

  def menu = {
    val seq = Seq("Furniture", "Alcohol", "Toiletries", "Shoes", "Food", "Jewelry").mkString("\n")
    prompt(s"\nPlease pick a category:\n${seq}\n").toUpperCase()
  }

  def sort(category: String) = {
    val bw = new PrintWriter(new FileOutputStream(new File("filtered_purchases.prn"), true))
    purchases.foreach(x =>
      if (x.category.toUpperCase.equalsIgnoreCase(category)) {
        bw.append(s"Customer:${x.customer_id} Date: ${x.date}")
        println(s"Customer:${x.customer_id} Date: ${x.date} \n")

      })
    bw.close()
    ""
  }


}




