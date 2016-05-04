package relation
package compiler

import shallow._
import deep._

object Main extends App {
  
  implicit object Context extends RelationDSLOpsPackaged
  
  def pgrmA = dsl""" 
    val schema = Schema("number", "digit")
    val En = Relation.scan("data/En.csv", schema, "|")
    val selEn = En.select(x => x.getField(schema, "number") == "one")
    val projEn = selEn.project(Schema("number"))
    projEn.print
  """

  def pgrmB = dsl"""
    val EnSchema = Schema("number", "digit")
    val En = Relation.scan("data/En.csv", EnSchema, "|")
    val FrSchema = Schema("digit", "nombre")
    val Fr = Relation.scan("data/Fr.csv", FrSchema, "|")
    val EnFr = En.join(Fr, "digit", "digit")
    EnFr.print
  """
  
  def pgrmC = dsl"""
    val EnSchema = Schema("number", "digit")
    val En = Relation.scan("data/En.csv", EnSchema, "|").select(x => x.getField(EnSchema, "number") == "one")
    val projEn = En.project(Schema("number"))
    projEn.print
  """
  
  def pgrmD = dsl"""
    val schema = Schema("number", "digit")
    val R = Relation.scan("data/R.csv", schema, "|")
    val proj = R.project(Schema("digit"))
    proj.print
    val proj2 = R.project(Schema("number"))
    proj2.print
    val proj3 = R.project(Schema("digit", "number"))
    proj3.print
    val proj4 = R.project(Schema("number", "digit"))
    proj4.print
  """
  
  def pgrmE = dsl"""
    val schema = Schema("number", "digit")
    val R = Relation.scan("data/R.csv", schema, "|")
    val sel = R.select(x => x.getField(schema, "number") == "one")
    val sel2 = R.select(x => x.getField(schema, "digit") == "2")
    val sel3 = R.select(x => x.getField(schema, "digit") == "5")
    sel.print
    sel2.print
    sel3.print
  """
  
  def pgrmF = dsl"""
    val schema = Schema("number", "digit")
    val schema2 = Schema("number2", "digit")
    val R = Relation.scan("data/R.csv", schema, "|")
    val R2 = Relation.scan("data/R.csv", schema2, "|")
    val R3 = Relation.scan("data/R.csv", schema, "|")
    val R4 = Relation.scan("data/R.csv", schema, "|")
    val sel2 = R.join(R2, "digit", "digit")
    val sel3 = sel2.join(R2, "digit", "digit")
    val sel4 = sel2.join(R3, "number2", "number")
    println("simple digit__________")
    sel2.print
    println("twice digit___________")
    sel3.print
    println("digit+number____________")
    sel4.print
  """
  
  def pgrmG = dsl"""
    println("loading__________")
    val schemaPerson = Schema("name", "surname", "jobTitle")
    val schemaSex = Schema("name2", "surname2", "sex")
    val schemaSalaries = Schema("jobTitle2", "monthlySalary")
    
    println("loadingpers__________")
    val person = Relation.scan("data/person.csv", schemaPerson, "|")
    person.print
    println("loadingsex__________")
    val sex = Relation.scan("data/sex.csv", schemaSex, "|")
    sex.print
    println("loadingsalar__________")
    val salaries = Relation.scan("data/salaries.csv", schemaSalaries, "|")
    salaries.print
    
    val join1 = person.join(sex, "name", "name2")
    val join2 = join1.join(salaries, "jobTitle", "jobTitle2")
    
    println("join____________")
    join1.print
    println("join____________")
    join2.print
  """
  
  def pgrmH = dsl"""
    val En = Relation.scan("data/R.csv", Schema("number", "digit"), "|")
    val Fr = Relation.scan("data/S.csv", Schema("digit2", "nombre"), "|")
    val P = En.join(Fr, "digit", "nombre")
    P.print
  """
  
  def pgrmTotal = dsl"""
    println("Test selection -------")
    $pgrmE
    println("Test join ------")
    $pgrmF
    println("Test project ------")
    $pgrmD
  """
  
  //def pgrm = pgrmTotal
  def pgrm = pgrmG
  
  val compiler = new RelationCompiler(Context)

  compiler.compile(pgrm) 
}
