import java.io.{FileWriter, PrintWriter}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.control.Breaks.break

object CMS_SAT {
  var metrics: mutable.HashMap[String, String] = new mutable.HashMap[String, String]()

  def main(args: Array[String]) = {
    print("CMS_SAT_Solver SCALA")
    val base_file = "sudokus1000.txt"
    //    val base_file="damnhardsudokus.txt"
    val temp_file = Utility.getListOfGames("src/main/scala/" + base_file)
    //    val temp_file = Utility.getListOfGames("src/main/scala/damnhardsudokus.txt")

    //    val solver =  new CMS_basic_Dpll("src/main/scala/test_cnf_m.txt")
    //    val solver =  new CMS_basic_Dpll("src/main/scala/test_cnf_m_hard.txt")
    //    runDPLLBASIC("src/main/scala/test_cnf_m_hard.txt")
    runBulkDPLLForMetrics(temp_file)

  }

  def updateMetricsInCSV(metrics: mutable.HashMap[String, String], file_suffix: String) = {
    //    val headers= Seq("source_identifier","run_time_in_ms","t_literal_len","n_values_in_first_row","n_already_filled","dpll_pure_literal_prop","dpll_recursion","dpll_unit_prop","dpll_assignments","dpll_pure_literal_prop")
    var mapKey: List[String] = metrics.keys.toList
    val headers: List[String] = metrics.keys.toList
    var records:ListBuffer[String] = new ListBuffer[String]()
    metrics.foreach(met=>{
      println("\n"+met._1)
      records.addOne(met._2)
    })
    var rrecords: Seq[Seq[String]] = List(records.toList)
    //    val records: Seq[Seq[String]] = mapKey.map(key => Seq(
    //      key,
    //      metrics.getOrElse(key, "")
    //    ))
//    val allRows: Seq[Seq[String]] = headers +: rrecords
    val allRows: Seq[Seq[String]] =  rrecords
//    val allRows: Seq[String] =  records.toList
    var csv : StringBuilder = new StringBuilder()
    allRows.foreach(row=>{
      csv.append(row.mkString(","))
      csv.append("\n")
    })

//    val csv: String = allRows.map(_.mkString(",")).toList.mkString("|LINEBREAK|")
    val fw = new FileWriter("src/main/scala/temp_metrics.csv", true)
    //    new FileWriter("src/main/scala/temp_metrics"+file_suffix+".csv") { append(csv); close() }
    fw.write(csv.toString())
    fw.close()

  }
  def runBulkDPLLForMetrics(problems: mutable.HashMap[Int, mutable.HashSet[List[Int]]])={
    val devStop = 10002
    var itr= 1
    problems.foreach(problem => {
      if(itr>devStop) break
      val _temp_sudoku_file = createMergedWithRules(problem._2,itr)
      runDPLLBASIC(_temp_sudoku_file)
      itr= itr+1
    })
  }
  def createMergedWithRules(value: mutable.HashSet[List[Int]], file_suffix:Int) :String={
    var stringBuilder = new StringBuilder()
    var vFR = 0
    metrics= metrics.addOne("n_already_filled",value.size.toString)
    value.foreach(p => if(p.head == 1) vFR= vFR + 1)
    metrics= metrics.addOne("n_values_in_first_row",vFR.toString)

    value.foreach(given=>{
      stringBuilder.append(`given`.mkString(""))
      stringBuilder.append(" 0\n")
    })

    val finalS = stringBuilder.prepended(Source.fromFile("src/main/scala/rules.txt").getLines.toList.toString()).mkString("")
    val finalCleaned = finalS.replace("List(","").replace(")","\n").replace(", ","\n")
//    print(finalCleaned)
    import java.io._
    val fileNametemp= "src/main/scala/temp_sudokuTest" + file_suffix +".txt"
    val pw = new PrintWriter(new File(fileNametemp ))
    pw.write(finalCleaned)
    pw.close
    fileNametemp
  }

  def runDPLLBASIC(cnf_sudoku:String)={
    val cms_dpll_basic =  new CMS_basic_Dpll(cnf_sudoku)
    metrics= metrics.addOne("source_identifier",cnf_sudoku)
    metrics=  Utility.printSudokuCNFMetrics(cms_dpll_basic.CNF,metrics)

    val st= System.currentTimeMillis()
    val success = cms_dpll_basic.dpll(cms_dpll_basic.CNF)
    val tt= (System.currentTimeMillis() - st).longValue()
    println("\n\ntime taken in milliSecond : " + tt)
    if (success) {
      metrics = cms_dpll_basic.getUpdatedMetrics(metrics)
      print("\nSAT")
    }
    else {
      print ("UNSAT")
    }
    metrics = metrics.addOne("run_time_in_ms",tt.toString)
    metrics.foreach(k=> {
      println(k._1 +" : "+ k._2)
    })
    updateMetricsInCSV(metrics,metrics.getOrElse("source_identifier","____1").replace(".txt","").substring(16))
  }
}
