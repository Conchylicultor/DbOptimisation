package relation
package compiler

import scala.collection.mutable

import ch.epfl.data.sc.pardis
import pardis.optimization.RecursiveRuleBasedTransformer
import pardis.quasi.TypeParameters._
import pardis.types._
import PardisTypeImplicits._
import pardis.ir._

import relation.deep.RelationDSLOpsPackaged
import relation.shallow._  

class ColumnStoreLowering(override val IR: RelationDSLOpsPackaged, override val schemaAnalysis: SchemaAnalysis) extends RelationLowering(IR, schemaAnalysis) {
  import IR.Predef._
  
  type LoweredRelation = Rep[Array[Array[String]]] // [ColumnId] [ColumnContent]
  
  def relationScan(scanner: Rep[RelationScanner], schema: Schema, size: Rep[Int], resultSchema: Schema): LoweredRelation = {
    val nbColumn = schema.size
    val nbRow = size
    dsl"""
        // Initialisation of the array
        var arrResult = new Array[Array[String]]($nbColumn)
        for (i <- 0 until $nbColumn) {
            arrResult(i) = new Array[String]($nbRow)
        }
        
        // We fill the array
        var i = 0
        while($scanner.hasNext) {
            for(j <- 0 until $nbColumn) {
                arrResult(j)(i) = $scanner.next_string()
            }
            i = i + 1
        }
        arrResult
    """
  }
  
  def relationProject(relation: Rep[Relation], schema: Schema, resultSchema: Schema): LoweredRelation = {
    //
    val nbColumnResult = resultSchema.size
    val originSchema = getRelationSchema(relation)
    
    // println("Start projection dim " + originSchema.size + " on dim " + nbColumnResult)
    
    // Get the column idx
    var arrIdx = new Array[Int](nbColumnResult)
    var i = 0
    resultSchema.columns.foreach {resultColStr =>
        originSchema.columns.foreach {colStr =>
            if (resultColStr == colStr) { // Column match (one by resultColStr)
                //println("Match for index i " + i)
                //println(colStr + " projected on " + originSchema.indexOf(colStr))
                arrIdx(i) = originSchema.indexOf(colStr)
                //println("Verif: " + i + " -> " + arrIdx(i))
                i = i+1
            }
        }
    }
    
    /*println("Print array of length " + arrIdx.length)
    for(i <- 0 until nbColumnResult) {
        println(i + " -> " + arrIdx(i))
    }*/
    
    // Generate code
    //println("Generate associate code")
    
    val arr = getRelationLowered(relation)
    // Initialisation
    var arrResult = dsl"""
        new Array[Array[String]]($nbColumnResult)
    """
    // We only keep some of te column
    for(i <- 0 until nbColumnResult) {
        dsl"""
            $arrResult($i) = $arr(${arrIdx(i)}) // We copy th column (by reference)
        """
    }
    // We return the result
    dsl"""
        $arrResult
    """
  }
  
  def relationSelect(relation: Rep[Relation], field: String, value: Rep[String], resultSchema: Schema): LoweredRelation = {
    val arr = getRelationLowered(relation)
    val idxField = resultSchema.indexOf(field)
    
    val nbColumn = resultSchema.size // Or arr.length
    
    dsl"""
        // Could probably optimize this part (iterate just one time instead of twice)
        //println($idxField)
        
        // Count nb of occurence
        var size = 0
        for (i <- 0 until $arr($idxField).length) {
            if($arr($idxField)(i) == $value) {
                size = size + 1
            }
        }
        //println("Found " + size)
        
        // Initialisation of the array
        var arrResult = new Array[Array[String]]($nbColumn)
        for (i <- 0 until $nbColumn) {
            arrResult(i) = new Array[String](size)
        }
        
        var currentIdx = 0
        for (i <- 0 until $arr($idxField).length) { // For each match in the column
            if($arr($idxField)(i) == $value) {
                for (j <- 0 until $nbColumn) { // We copy all columns
                    arrResult(j)(currentIdx) = $arr(j)(i)
                }
                currentIdx = currentIdx + 1
            }
        }
        
        arrResult
    """
  }
  
  def relationJoin(leftRelation: Rep[Relation], rightRelation: Rep[Relation], leftKey: String, rightKey: String, resultSchema: Schema): LoweredRelation = {
    val leftSchema = getRelationSchema(leftRelation)
    val rightSchema = getRelationSchema(rightRelation)
    
    val idxKeyLeft  = leftSchema.indexOf(leftKey)
    val idxKeyRight = rightSchema.indexOf(rightKey)
    
    val nbColumnResult = leftSchema.size + rightSchema.size - 1 // The joint belong to both of the schemas
    
    val arrLeft = getRelationLowered(leftRelation)
    val arrRight = getRelationLowered(rightRelation)
    
    dsl"""
        // Count the number of row and store the idx
        //var listIdx = List[(Int, Int)]()
        var maxSize = 10*($arrLeft($idxKeyLeft).length + $arrRight($idxKeyRight).length) // We suppose we don't do massive joints (should be replaced by list!)
        var listIdx = new Array[(Int, Int)](maxSize)
        var currentIdx = 0
        for (i <- 0 until $arrLeft($idxKeyLeft).length) { // Iterate over the column
            for (j <- 0 until $arrRight($idxKeyRight).length) {
                if ($arrLeft($idxKeyLeft)(i) == $arrRight($idxKeyRight)(j)) { // Match
                    //listIdx :+ (i, j)
                    listIdx(currentIdx) = (i, j)
                    //println(i + " : " + j)
                    currentIdx = currentIdx + 1
                }
            }
        }
        listIdx(currentIdx) = (-1,-1) // The end
        //println(listIdx.size)
        
        // Initialize the array
        val nbRow = currentIdx // This correspond to the number of match we did
        var arrResult = new Array[Array[String]]($nbColumnResult)
        for (i <- 0 until $nbColumnResult) {
            arrResult(i) = new Array[String](nbRow)
        }
        
        // Fill the array
        for (i <- 0 until nbRow) { // Iterate over the elements
            var savedIndex = listIdx(i)
            var currentColumnIdx = 0
            for (j <- 0 until $nbColumnResult) { // Iterate over the elements
                var value = ""
                if (j < $arrLeft.length-1) { // The first columns belong to the left (-1 for the key)
                    if(currentColumnIdx == $idxKeyLeft) { // We skip the key
                        currentColumnIdx = currentColumnIdx + 1
                    }
                    value = $arrLeft(currentColumnIdx)(savedIndex._1)
                }
                else if (j == $arrLeft.length-1) { // Middle column: the key
                    currentColumnIdx = -1 // Reset the current idx (-1 because of the +1 at the end)
                    value = $arrLeft($idxKeyLeft)(savedIndex._1) // == value = $arrRight($idxKeyRight)(savedIndex._2)
                }
                else { // Last columns: right
                    if(currentColumnIdx == $idxKeyRight) { // We skip the key
                        currentColumnIdx = currentColumnIdx + 1
                    }
                    value = $arrRight(currentColumnIdx)(savedIndex._2)
                }
                
                arrResult(j)(i) = value
                currentColumnIdx = currentColumnIdx + 1
            }
        }
        
        arrResult
    """
  }
  
  def relationPrint(relation: Rep[Relation]): Unit = {
    val arr = getRelationLowered(relation)
    dsl"""
        val nbColumn = $arr.length
        val nbRow = $arr(0).length
        for (i <- 0 until nbRow) {
            var str = ""
            for (j <- 0 until nbColumn) {
                str = str + $arr(j)(i)
                if (j != nbColumn-1) {
                    str = str + "|"
                }
            }
            println(str)
        }
    """
  }
  
}
