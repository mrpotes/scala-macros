package restapi

trait Delete[T] {

  def delete(obj : T) : Unit
  
}