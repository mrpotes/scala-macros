package model
import restapi._

case class Tom
object Tom extends Read[Tom] with Write[Tom] {
  def fred = 1
  def read(id : String) : Tom = Tom()
  def write(obj : Tom) = println("Writing Tom "+obj)
}