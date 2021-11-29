import sun.font.TrueTypeFont

import java.io.{BufferedWriter, File, FileWriter}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.language.postfixOps
import scala.util.control.Breaks.break

class CMS_basic_Dpll(name: String) {
  val source = scala.io.Source.fromFile(name)
  val lines = source.mkString.split("\n")
  source.close
  val CNF = stringToCNF(lines.toList)
  val length = getSizeForInit()
  val model = mutable.Map.empty[Int, Option[Boolean]]
  var assignmentModel: ListBuffer[Int] = new ListBuffer[Int]()
  var assignmentSet: mutable.HashSet[Int] = new mutable.HashSet[Int]()
  var unitSet: mutable.HashSet[Int] = new mutable.HashSet[Int]()
  var n_unit_prop = 0
  var n_pure_literal_prop = 0
  var n_recursion = 0
  var n_true = 0
  var keepSearching = true
  var n_assignments= 0
  def initAssignment(length: Int): Unit = {
    if (length > 0) {
      model(length) = None;
      model(-1*length) = None;
      assignmentModel =  new ListBuffer[Int]()
      initAssignment(length - 1)
    }
  }

  initAssignment(length)


  def setTrue(index: Int) = {
    model(index) = Some(true)
    model(-1*index) = Some(false)
    assignmentSet. add(index)

  }

  def setFalse(index: Int) = {
    model(index) = Some(false)
    model(-1*index) = Some(true)
    assignmentSet.add(-index)

  }

  def parseCNF(value: List[String]): List[List[Int]]={
    val ret =value.map(cls => {
      (cls.split(" ").toList map (_.toInt) filter (0 !=))

    })
//    println("cms debug"+ ret.toString())
    ret
  }
  def stringToCNF(lines: List[String]): List[List[Int]] = {

    if (lines == List()){
      List()
    }
    else if(lines.head(0) == 'p' || lines.head.contains("p")){
      parseCNF(lines.tail)
    }
    else {
      parseCNF(lines)
    }
  }


  def printCNF() : Unit = {
    CNF foreach (x => {x foreach { y => print(y + " ")}; print("\n")})
  }

  def getSizeForInit () :Int= {
    val m = (CNF map {x => x.max}).max
    val n = -1* (CNF map {x => x.min}).min
    if(m > n) m else n
  }

  def satisfy(): Boolean = {
    CNF.foldRight(true)((x , acc) => acc && x.foldRight(false)((y, acc) => acc || model(y).contains(true)))
  }
  def findLiterals(all_clauses: List[List[Int]]) = {
      var map = Map[Int, Int]()
      for (x <- all_clauses; y <- x) {
        map += (Math.abs(y) -> (map.getOrElse(Math.abs(y), 0) + 1));
      }
      for (key <- map.keySet if map(key) == 1) yield key
    }


  def pureLiterals(all_clauses: List[List[Int]]): List[List[Int]] = {
    n_pure_literal_prop = n_pure_literal_prop +1
      val pures = findLiterals(all_clauses)
      var k = all_clauses
      for (x <- pures)
        k = k filter (!_.exists(y => y == x || y == -x))
      k

    }

    def getUnits(all_clauses: List[List[Int]]): List[Int] = {
      all_clauses filter (_.length == 1) map (_.head)
    }

    def assignmentAndDe(unit: Int, all_clauses: List[List[Int]]) = {
      (all_clauses filter (!_.contains(unit))) map ((x) => x filter ((y) => y != -unit))
    }

    def delAllUnits(units: List[Int], all_clauses: List[List[Int]]): List[List[Int]] = {
      var k = all_clauses
      for (x <- units) {
        k = assignmentAndDe(x, k)
        //println(k)
      }
      k
    }

  def unitpropagation(all_clauses: List[List[Int]]): List[List[Int]] = {
      //add for printing sol
      val unit_propogated_clauses= delAllUnits(getUnits(all_clauses), all_clauses)
      if(unit_propogated_clauses.nonEmpty && unit_propogated_clauses.head.nonEmpty ) {
        n_unit_prop = n_unit_prop + 1
        this.unitSet.add(unit_propogated_clauses.head.head)
      }
      unit_propogated_clauses
    }


def writeFile(filename: String, s: String): Unit = {
  val file = new File(filename)
  val bw = new BufferedWriter(new FileWriter(file))
  bw.write(s)
  bw.close()
}

  def getUpdatedMetrics(metrics:mutable.HashMap[String,String]): mutable.HashMap[String,String]={
    metrics.addOne("dpll_pure_literal_prop",n_pure_literal_prop.toString)
    metrics.addOne("dpll_unit_prop",n_unit_prop.toString)
    metrics.addOne("dpll_recursion",n_recursion.toString)
    metrics.addOne("dpll_assignments",n_assignments.toString)
  }
def dpll(all_clauses: List[List[Int]]): Boolean = {

  n_recursion = n_recursion + 1

      if (all_clauses.isEmpty) return true

      if (isFalse(all_clauses)) {
        return false;
      }
      val temp = pureLiterals(all_clauses)
      val reduced_clauses_after_unit_prop = unitpropagation(temp)

      if (isFalse(reduced_clauses_after_unit_prop)) {
        return false;
      }

  if (isTrue(reduced_clauses_after_unit_prop)) {
        reduced_clauses_after_unit_prop.foreach( l => {
          this.setTrue(l.head)
        })
        return true
      }
       val variable = reduced_clauses_after_unit_prop.head.head;
        n_assignments = n_assignments + 1

  dpll(assignmentAndDe(variable, reduced_clauses_after_unit_prop)) || dpll(assignmentAndDe(-variable, reduced_clauses_after_unit_prop));
    }


    def isFalse(all_clauses: List[List[Int]]): Boolean = {
       all_clauses contains (List[Int]())
    }

    def isTrue(all_clauses: List[List[Int]]): Boolean = {
      all_clauses.isEmpty
    }
  
}