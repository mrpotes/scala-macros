package restapi

trait Write[T] {

  def write(obj : T) : Unit
  
}