import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

object Utility {
  def parseThisGameToDimac(game: String, value: 9):mutable.HashSet[List[Int]]={
    var n_lines = value
    var c_row = 1
    var c_col = 1
    var itr = 1
    val gameDimac: mutable.HashSet[List[Int]] = new mutable.HashSet[List[Int]]()
    game.foreach(cell => {
      if (itr == 10) // move next line
       {
         c_row = c_row +1
         c_col = 1
         itr = 1
       }
      if(cell != '.' && Integer.parseInt(cell.toString)!=0){
        val fill_value = {
          List(c_row, c_col, Integer.parseInt(cell.toString))
        }
        gameDimac.add(fill_value)
      }

      c_col = c_col + 1
      itr= itr + 1
    })
    gameDimac
  }
  def getListOfGames(sudoku_game_collection:String):mutable.HashMap[Int, mutable.HashSet[List[Int]]]={
    // for 9x9 for now
    val allGames:List[String] = Source.fromFile(sudoku_game_collection).getLines.toList
    val AllGameDimac : mutable.HashMap[Int, mutable.HashSet[List[Int]]] = new mutable.HashMap[Int, mutable.HashSet[List[Int]]]()
    var itr = 1
    allGames.foreach(game => {
      // for each game, we parse into dimac
      val dimac_game = parseThisGameToDimac(game, 9)
//      println("cms_debug_printing game number "+ itr + " : " + dimac_game.mkString("|"))
      AllGameDimac.addOne(itr,dimac_game)
      itr = itr +1
    })
    AllGameDimac
  }
  def preprocessInput(input_file:String)={
    val lines = {
      Source.fromFile(input_file).getLines.toList
    }

   lines
  }
  def printSudokuCNFMetrics(cnf: List[List[Int]], metrics : mutable.HashMap[String,String]) : mutable.HashMap[String,String]={
    var n_positive = 0
    var n_unit = 0
    var n_negative = 0
    var n_literals = 0
    val total1= cnf.length
    var total2 :Int = 0
    cnf.foreach( c=> {
      total2 = total2 + c.length
      if( c.length == 1 ){ n_unit = n_unit +1 }
      c.foreach(literal => {
        n_literals= n_literals + 1
        if (literal>0){n_positive = n_positive + 1}
        else {n_negative = n_negative + 1}

      } )
    })
    metrics.addOne("t_cnf_len",total1.toString)
    metrics.addOne("t_clauses_len",total2.toString)
    metrics.addOne("t_literal_len",n_literals.toString)
    metrics.addOne("t_unit_clauses",n_unit.toString)
    metrics.addOne("t_positive_literals",n_positive.toString)
    metrics.addOne("t_negative_literals",n_negative.toString)
    println("\nMetricsSudokuCNF:" + "n_positive.toString "+ " | " + "n_unit.toString" +" | " + "n_negative.toString" + " | " +"total1.toString" + " | " +"total2.toString")
    println("\nMetricsSudokuCNF:" + n_positive.toString + " | " + n_unit.toString +" | " + n_negative.toString + " | " +total1.toString + " | " +total2.toString)
    metrics
  }


}
