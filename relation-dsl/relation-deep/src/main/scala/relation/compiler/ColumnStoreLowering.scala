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
  
  //type LoweredRelation = Rep[Array[Array[Any]]] // [ColumnId] [ColumnContent]
  type LoweredRelation = Rep[Array[String]] // [ColumnId] [ColumnContent]
  
  def relationScan(scanner: Rep[RelationScanner], schema: Schema, size: Rep[Int], resultSchema: Schema): LoweredRelation = {
    val nbColumn = schema.size
    val nbRow = size
    //val a = Array.ofDim[String](3, 2)
    dsl"""
        val arraySize = $nbColumn * $nbRow // We add 1 for the number of column
        val arrResult = new Array[String](arraySize)
        var i = 0
        println( $nbColumn )
        println( $nbRow )
        while($scanner.hasNext) {
            var j = 0
            for(j <- 0 until $nbColumn) {
                arrResult(j*$nbRow + i) = $scanner.next_string()
                println( "Value of j: " + j + " : " + arrResult(j*$nbRow + i))
            }
            i = i + 1
        }
        arrResult
    """
  }
  
  def relationProject(relation: Rep[Relation], schema: Schema, resultSchema: Schema): LoweredRelation = {
    ??? // TODO
  }
  
  def relationSelect(relation: Rep[Relation], field: String, value: Rep[String], resultSchema: Schema): LoweredRelation = {
    ??? // TODO
  }
  
  def relationJoin(leftRelation: Rep[Relation], rightRelation: Rep[Relation], leftKey: String, rightKey: String, resultSchema: Schema): LoweredRelation = {
    ??? // TODO
  }
  
  def relationPrint(relation: Rep[Relation]): Unit = {
    val arr = getRelationLowered(relation)
    dsl"""
        for (i <- 0 until $arr.length) {
            println($arr(i))
        }
        println("Hi!")
    """
  }
  
}
