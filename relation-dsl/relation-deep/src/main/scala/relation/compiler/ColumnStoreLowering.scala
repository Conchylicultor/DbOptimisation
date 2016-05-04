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
