package Apriori

import Apriori.Frequentitemset.Itemset

import scala.collection.immutable.SortedMap
import scala.io.Source

object Util {

  var replicateNTimes: Int = 1

  def absoluteSupport(minSupport: Double, numTransactions: Int) = (numTransactions * minSupport + 0.5).toInt

  def parseTransactions(lines: List[String], separator: String): List[Itemset] = {
    lines.filter(l => !l.startsWith("#"))
      .filter(!_.trim.isEmpty)
      .map(l => l.split(separator + "+"))
      .map(l => l.map(item => item.trim).toList)
  }

  def parseTransactions(fileName: String, separator: String = ","): List[Itemset] = {

    parseTransactions(
      (1 to replicateNTimes).flatMap(_ => {
        val file = Source.fromFile(fileName, "UTF-8")
        file.getLines
      }).toList, separator)
  }

  def parseTransactionsByText(text: String): List[Itemset] = {
    parseTransactions(text.split("\n").toList, ",")
  }

  def printItemsets(itemsets: List[Itemset]) = {
    println(s"Found ${itemsets.size} itemsets")
    SortedMap(itemsets.groupBy(itemset => itemset.size).toSeq: _*)
      .mapValues(i => i.map(set => s"{${set.sorted.mkString(", ")}}").sorted.mkString(", "))
      .foreach(t => println(s"[${t._1}] ${t._2}"))
  }
}
