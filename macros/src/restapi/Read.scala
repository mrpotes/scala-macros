package restapi

trait Read[T] {

  def read(id : String) : T
  
}