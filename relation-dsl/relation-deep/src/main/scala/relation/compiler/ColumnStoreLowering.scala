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
    println(resultSchema.size + " " + schema.size)
    println(schema.indexOf(resultSchema.columns(0)))
    
    // Get the column idx
    var arrIdx = Array[Int](nbColumnResult)
    var i = 0
    resultSchema.columns.foreach {resultColStr =>
        schema.columns.foreach {colStr =>
            println(colStr + " " + resultColStr)
            if (resultColStr == colStr) { // Column match (one by resultColStr)
                arrIdx(i) = schema.indexOf(colStr)
                println(arrIdx(i))
                i = i+1
            }
        }
    }
    
    val arr = getRelationLowered(relation)
    dsl"""
        //println($arrIdx(0))
        
        var arrResult = new Array[Array[String]]($nbColumnResult)
        
        arrResult(0) = $arr(1)
        arrResult
    """
    
    /*val copyRecord: Rep[Any] => Rep[Rec] =
      e => __new[Rec](schema.columns.map(column => (column, false, dsl"__struct_field[String]($e, $column)")): _*)
    dsl"""
        // Initialisation of the array
        var arrResult = new Array[Array[String]]($nbColumnResult)
        
        var i = 0
        $resultSchema.columns.foreach {resultColStr =>
            println(resultColStr)
        }
        println("end")
        
        arrResult
    """*/
    
    /*dsl"""
        // Initialisation of the array
        var arrResult = new Array[Array[String]]($nbColumnResult)
        
        // Get the column idx and feed the array
        var i = 0
        $resultSchema.columns.foreach {resultColStr =>
            var j = 0
            $schema.columns.foreach {colStr =>
                if (resultColStr == colStr) { // Column match (one by resultColStr)
                    println(i + " -> " + j)
                    //arrResult(i) = $arr(j)
                    i = i+1
                }
                j = j+1
            }
        }
        println("end")
        
        arrResult
    """*/
    
    /*
    // Get the column idx
    var arrIdx = Array[Int](nbColumnResult)
    var i = 0
    resultSchema.columns.foreach {resultColStr =>
        schema.columns.foreach {colStr =>
            if (resultColStr == colStr) { // Column match (one by resultColStr)
                arrIdx(i) = schema.indexOf(colStr)
                i = i+1
            }
        }
    }
    
    val arr = getRelationLowered(relation)
    
    dsl"""
        // Initialisation of the array
        var arrResult = new Array[Array[String]]($nbColumnResult)
        
        for(i <- 0 until $nbColumnResult) {
            println(i)
            //println($arrIdx(i))
            println(i + " -> " + $arrIdx(i))
            arrResult(i) = $arr($arrIdx(i)) // We copy th column (by reference)
        }
        println("end")
        
        arrResult
    """*/
  }
  
  def relationSelect(relation: Rep[Relation], field: String, value: Rep[String], resultSchema: Schema): LoweredRelation = {
    val arr = getRelationLowered(relation)
    val idxField = resultSchema.indexOf(field)
    
    val nbColumn = resultSchema.size // Or arr.length
    
    dsl"""
        // Could probably optimize this part (iterate just one time instead of twice)
        println($idxField)
        
        // Count nb of occurence
        var size = 0
        for (i <- 0 until $arr($idxField).length) {
            if($arr($idxField)(i) == $value) {
                size = size + 1
            }
        }
        println("Found " + size)
        
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
    ??? // TODO
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
